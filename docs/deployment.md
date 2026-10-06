# Deploying a Data UI Application

*From a model that fits on a napkin to a live, TLS-terminated, RBAC-backed web application, with one command.*

This document explains everything about how Data UI deployment works: the big picture, the command, every moving part behind it, where the secrets live, how the cert renews itself at 3am while you sleep, and what to do on the rare day something goes sideways. It is written so that a junior programmer can follow along. If you can run a shell command and have seen a YAML file without crying, you are qualified.

## Table of Contents

- [The Big Picture](#the-big-picture)
- [Quick Start](#quick-start)
- [What `deploy` Actually Does, Step by Step](#what-deploy-actually-does-step-by-step)
- [The Model Drives Everything](#the-model-drives-everything)
- [Environments](#environments)
- [Deployment State: Where Things Live](#deployment-state-where-things-live)
- [Secrets, and How to Get the Admin Password](#secrets-and-how-to-get-the-admin-password)
- [The Kubernetes Manifests](#the-kubernetes-manifests)
- [How HAProxy Routing Works](#how-haproxy-routing-works)
- [TLS: Certificates That Renew Themselves](#tls-certificates-that-renew-themselves)
- [Docker Credentials: Headless Deploys](#docker-credentials-headless-deploys)
- [Deploying From Another Machine](#deploying-from-another-machine)
- [Dry Runs](#dry-runs)
- [Connecting a REPL to the Live App](#connecting-a-repl-to-the-live-app)
- [Troubleshooting](#troubleshooting)
- [Starting Over: the Clean-Slate Procedure](#starting-over-the-clean-slate-procedure)

## The Big Picture

Most deployment stories involve a wall of YAML you wrote by hand, a wiki page titled "DO NOT TOUCH unless you are Dave," and a Dave who left the company in 2023. Data UI's story is shorter: **the model is the configuration.**

Your model already declares the application's identity:

    (:title "To Do List"
      :name "todos"
      :version "0.1"
      :domain "todo.demo.data-ui.com"
      :repl t
      :types ...)

The deploy pipeline reads those five keys and derives *everything* from them: the Docker image tag, the Kubernetes namespace, the release directory, the HAProxy backend, the public URL. There is no separate deployment config to drift out of sync with the application, because there is no separate deployment config.

The target environment is deliberately modest: a single machine (the "deploy host") running a [k3d](https://k3d.io) cluster (k3s in Docker), with HAProxy in front terminating TLS. No cloud bill, no managed Kubernetes, no Helm charts. One box, one command.

## Quick Start

On the deploy host (or any machine with ssh access to it; see [Deploying From Another Machine](#deploying-from-another-machine)):

    cd data-ui
    scripts/data-ui deploy todos

That's it. The output ends with:

    Deployed To Do List todos-0.1-6d8f586.
      Domain:   https://todo.demo.data-ui.com
      NodePort: http://172.18.0.2:30303
      Swank:    kubectl -n dataui-todos port-forward deploy/dataui-todos 4005:4005

Open the domain, log in as `admin` with the password from [Secrets](#secrets-and-how-to-get-the-admin-password), and you are looking at your deployed application.

Requirements:

- A clean git tree (the deploy tags the exact commit it ships; uncommitted changes would make the tag a lie).
- `sudo` access on the deploy host (for the HAProxy update, nothing else).
- The one-time host setup already done: k3d cluster, HAProxy, TLS certificate (see [TLS](#tls-certificates-that-renew-themselves)).

## What `deploy` Actually Does, Step by Step

`scripts/data-ui deploy` runs the following phases, in order. Each phase is a shell function in `scripts/data-ui`, so the script is the authoritative reference; this is the guided tour.

### 1. Compile the model (the gate)

    compile_model

Before anything ships, the model in `models/<model-name>.lisp` must compile. The script starts a *throwaway* PostgreSQL container (its own container name and port 5446, so it never collides with your dev REPL database on 5444 or the test database on 5445), initializes the schema, and runs the compilation phase of `set-model` (validation, SQL generation, lambda compilation) against it. Then the container and its volume are destroyed.

If the model doesn't compile, the deploy dies right here, before a tag, an image, or a manifest exists. A broken model never gets anywhere near the cluster.

### 2. Read identity from the model

    gather_deploy_facts

The script asks the model for its `:name`, `:title`, `:version`, `:domain`, and `:repl` values (via a tiny Lisp helper, `lisp/deployment.lisp`). From those plus the git short hash, it derives:

| Fact         | Example                                              |
|--------------|------------------------------------------------------|
| TAG          | `todos-0.1-6d8f586`                                   |
| IMAGE        | `macnod/data-ui:todos-0.1-6d8f586`                    |
| NAMESPACE    | `dataui-todos`                                        |
| RELEASE_NAME | `To Do List todos-0.1-6d8f586`                        |
| OUT_DIR      | `/data/data-ui/deploy/todos/releases/0.1-6d8f586` |

### 3. Tag the release

    create_deploy_tag

An annotated git tag (`todos-0.1-6d8f586`) is created at HEAD. If the tag already exists *and points at HEAD*, it is reused (re-deploying the same commit is fine). If it exists and points elsewhere, the deploy aborts: a tag must never silently change meaning.

### 4. Allocate a NodePort

    assign_node_port

Each instance gets a stable NodePort (starting at 30303; 30300–30302 are reserved for k3d's published loopback hops). The assignment is cached in `/data/data-ui/deploy/ports.lock`, but the cache is not the source of truth; the *cluster* is. If the lock file is missing, the port is recovered from the live Service. Only when neither exists is a new port assigned (lowest free port ≥ 30303, checked against both the lock file and every NodePort in the cluster).

### 5. Ensure instance secrets

    ensure_instance_secrets

See [Secrets](#secrets-and-how-to-get-the-admin-password). Same cache-vs-truth design: file missing → recover from the live Kubernetes Secret; no live Secret either → generate fresh credentials. Credentials are *never* regenerated for an existing instance, because the PostgreSQL volume keeps the old password and new credentials would lock the app out of its own database.

### 6. Render the manifests

    generate_manifests

The templates in `deploy/templates/*.yaml.tpl` are rendered with plain `sed` substitution of `{{PLACEHOLDERS}}`: no templating engine, no dependencies, nothing to install. Two special cases:

- Lines ending in `#@repl` survive (marker stripped) only when the model says `:repl t`; otherwise they are deleted. This is how the Swank port appears in the manifest for REPL-enabled instances and doesn't exist at all for production ones.
- The database init SQL is generated from `tests/init-template.sql` and wrapped into a ConfigMap with `kubectl create configmap --dry-run`.

Rendered manifests land in the release directory (`OUT_DIR` above), so every release's exact manifests are preserved for inspection or rollback.

### 7. Build and import the image

    build_image

A multi-stage Docker build:

- **Stage 1 (node:22-slim):** `npm ci && npm run build`, the React frontend, typechecked and bundled by Vite into static files.
- **Stage 2 (ubuntu):** Roswell + SBCL + all Lisp dependencies, then the data-ui source. The system is **pre-compiled at build time** so container startup loads fasls instead of compiling from scratch (this matters: slow startups once fought the liveness probe, and the probe won). The entrypoint runs with `--disable-debugger` so any unhandled error prints a backtrace and exits instead of waiting politely at a debugger prompt inside a container nobody is attached to.

The image is then imported into the k3d cluster with `k3d image import`, so no registry is involved and nothing leaves the machine.

### 8. Apply the manifests

    apply_manifests

`kubectl apply -f $OUT_DIR`, then wait for the PostgreSQL rollout, then the app rollout. First boot initializes the database (RBAC tables, roles, permissions, admin/guest users), which is why the app's startupProbe allows up to five minutes before the liveness probe is allowed to have opinions.

### 9. Update HAProxy

    update_haproxy

The only step that needs sudo. Details in [How HAProxy Routing Works](#how-haproxy-routing-works).

## The Model Drives Everything

Worth repeating with the actual flow drawn out:

    models/<model-name>.lisp
        :name "todos" ──────────┬─→ namespace  dataui-todos
        :version "0.1" ────────┼─→ tag        todos-0.1-<git-hash>
        :domain "todo.demo..." ┼─→ HAProxy map entry + backend
        :repl t ───────────────┴─→ Swank port in the manifest (or not)

Change `:domain` in the model and redeploy: the new domain routes to the app. Bump `:version`: new tag, new release directory. Set `:repl nil`: the Swank listener vanishes from the deployment. The model is not *input to* the configuration; it *is* the configuration.

(Note the TODO in the example model: `:repl` should be `nil` in production. The Swank port is never exposed through a Service either way; it is reachable only via `kubectl port-forward`, which requires cluster credentials.)

## Environments

An *environment* is where a running instance lives. There are three: **development**, **staging**, and **production**. The word "mode" and ad-hoc "local"/"deployed" phrasing are retired in favor of these.

The environment is a property of the *launch path*, never stored in the model:

- `scripts/data-ui repl` (no profile) → development (throwaway 5444 database: the container and its volume are removed when the REPL exits, so the schema always matches the code just loaded)
- `scripts/data-ui repl <profile>` / `e-demo start <profile>` / `demo start <profile>` → staging (profiles run under systemd units via the e-demo / demo verbs); `profile expose` is a visibility toggle, not part of the definition
- `scripts/data-ui deploy <model>` → production (also the Deploy button, once production supports it — post-MVP)

One model may run in all three environments simultaneously, and each instance has its own database by construction:

- development: `dataui` in `pg-data-ui-repl` (port 5444)
- staging profile: `pg_ident(<profile>)` in `pg-data-ui-<profile>` (its own compose project)
- production: namespace `dataui-<name>` with its own postgres

Convention: one instance per model per environment; a second copy in the same environment is a new model. Data: development is volatile (snapshots only); staging and production are persistent — staging is *not* disposable. The admin password is static in development (`admin / admin-password-1`, by convention — see [docs/snapshots.md](snapshots.md)) and generated per instance in staging (`profile.env`) and production (cluster secrets). Snapshot restore re-stamps the destination's own admin password hash, so a restored database always answers to its environment's password (see [docs/snapshots.md](snapshots.md) → Password re-stamping). During the MVP, staging is the flagship environment: it hosts Model Bank and has the Deploy button, which production lacks. Staging does not mirror production's substrate (host process vs k8s); passing staging is not a substrate guarantee. An environment is not a "tier": tiers are product offerings; environments are where an instance runs.

### `:domain-stg`

The model's `:domain` remains the canonical production FQDN, used by `deploy`. Staging exposure (`profile expose`) reads `:domain-stg`; when the author omits it, the compiler derives it by suffixing `-stg` onto the first DNS label of `:domain` (`todo.demo.data-ui.com` → `todo-stg.demo.data-ui.com`). An explicit `:domain-stg` always wins; it must differ from `:domain` (one HAProxy map line, one owner) and requires `:domain` when written. The deploy-button exception is modelbank: staging keeps the clean canonical URL (`modelbank.demo.data-ui.com`) and production takes the `-p` suffix — both explicit. `-stg` hosts and modelbank's two names all live under the existing `*.demo.data-ui.com` wildcard (DNS + TLS), so no new zone or certificate is involved.

### Generate-button prompt log

Every real `:generate-model` LLM call (the Generate button on Model Bank) writes its full prompt — system message, user message, LLM model, temperature, record name, timestamp — as one org-mode file to the shared host directory `/data/k8s/data-ui/generate-log/`, named `<app-name>-<record-name>-<timestamp>.org`. Format: one level-1 org heading per field (the former JSON keys); the `* system-prompt` heading carries two level-2 children mirroring the prompt's two parts — `** Model Syntax Reference` (`docs/model-reference.md` verbatim in a `#+begin_src markdown` block) and `** Examples` (the framing prose, then every demo in one `#+begin_src lisp` block). Both blocks are org-comma-escaped so a `C-c '` edit round-trips; in the JSON body the two parts ride one system string, with part 2 opening under a `# Examples` markdown heading. (2026-09-27: the prompt's NUL pollution — a `u:slurp` UTF-8 bug that sent ~233 literal NULs to the LLM — was fixed at the source in dc-eclectic.) Unconditional and best-effort: a logging failure (missing dir, full disk) is logged and never fails Generate; test runs using the LLM override write nothing. The API key is a header and never rides the logged body. The directory is host-level today (every host instance runs as the same user); `/data/k8s/` placement means extending it to deployed production apps later is a pure ops change — mount a hostPath PV at the same path into app pods, zero code delta. The instance cannot yet name its environment (a backlog item), so dev and staging generations are distinguished only by app + record name.

## Deployment State: Where Things Live

Deployment state lives **outside the repository**, in `/data/data-ui/deploy/` on the deploy host:

    /data/data-ui/deploy/
    ├── ports.lock                      # name env port, one per line
    └── todos/
        ├── secrets.env                 # instance credentials (0600)
        └── releases/
            ├── 0.1-c13571a/            # every release's manifests, kept
            └── 0.1-6d8f586/
                ├── 00-namespace.yaml
                ├── 05-secrets.yaml
                ├── 10-pv.yaml
                ├── 15-pvc.yaml
                ├── 20-init-sql.yaml
                ├── 30-postgres.yaml
                ├── 40-data-ui.yaml
                ├── haproxy-backend.cfg
                └── haproxy-map-entry

Why outside the repo? Because rendered manifests are *derived output* (rebuildable from model + templates + state) and secrets are, well, secret. The repo holds source; the state directory holds facts about one particular machine's cluster. Deleting the whole state directory is recoverable: ports and secrets are re-read from the live cluster on the next deploy.

Application *data* lives in a third place: the k3d cluster binds `/data/k8s` (host, on the SSD) into the node at the same path, and each instance's PersistentVolumes use `/data/k8s/data-ui/<name>-<env>/{db,files}`. So even `k3d cluster delete` cannot destroy application data; it survives on the host filesystem.

Three layers, three lifetimes:

| Layer                          | Lives                         | Survives                  |
|--------------------------------|-------------------------------|---------------------------|
| Source (model, templates)      | git repo                      | everything                |
| Deploy state (secrets, ports)  | `/data/data-ui/deploy` | cluster recreation      |
| App data (database, files)     | `/data/k8s/data-ui`           | cluster deletion          |

## Secrets, and How to Get the Admin Password

Each instance has exactly three secrets, generated once at first deploy:

- `DB_PASSWORD`: PostgreSQL password for the `dataui` user
- `ADMIN_PASSWORD`: the application's `admin` login
- `JWT_SECRET`: signs the API's access and refresh tokens

### Getting the admin password

The easy way (on the deploy host):

    grep ADMIN_PASSWORD /data/data-ui/deploy/todos/secrets.env

The canonical way (works even if the secrets file is gone, from any machine with cluster access):

    kubectl get secret -n dataui-todos dataui-todos-secrets \
        -o jsonpath='{.data.admin-password}' | base64 -d; echo

Both should agree. If they don't, trust the cluster: the file is a cache; the Secret is the truth. (This is a recurring design theme. When a cache and the cluster disagree, the cluster wins, the same way the dictionary wins at Scrabble.)

A war story, so you don't repeat it: the very first end-to-end deploy "failed" with a wall of 401s. Backend verified healthy, JWTs verified valid, much head-scratching: the operator was logging in with the *old* admin password from a previous instance's secrets. If your freshly deployed app rejects you, read the password again, slowly.

### Password re-stamping on snapshot restore

The snapshot machinery (dev + staging profiles, and production via `prd:` addresses) re-stamps the destination's own admin password hash after every restore, so the env and the database never disagree. For a `prd:<name>` destination the working password after a restore is the cluster Secret's `admin-password` — restore reads it live from the Secret (never the `secrets.env` cache) and re-stamps with exactly that value. See [snapshots.md](snapshots.md) → Production for the full `prd:` story (deploy-host-only, scale-to-0 restore, prerestore slot, the EXIT trap's failure modes).

### Password format trivia

`ADMIN_PASSWORD` is generated as `$(openssl rand -hex 8)-a1`. The `-a1` suffix is not decoration: the rbac library's password policy requires at least one letter, one digit, and one punctuation character, and sixteen random hex characters can satisfy the first two but never the third. The suffix guarantees all three, and the sixteen random hex chars provide the entropy. Yes, this was learned the hard way. No, the instance that taught us is no longer with us.

### Rotation

There is no rotation tooling yet. If you must rotate manually: update the Kubernetes Secret, update `secrets.env`, restart the deployment, and for `DB_PASSWORD` also `ALTER USER dataui PASSWORD ...` inside postgres (in that order of caution). For a demo instance, the clean-slate procedure below is honestly less error-prone.

## The Kubernetes Manifests

Each instance is fully isolated in its own namespace, `dataui-<name>`. The manifests, in apply order:

| File                | What it creates                                          |
|---------------------|----------------------------------------------------------|
| `00-namespace.yaml` | The namespace `dataui-<name>`                            |
| `05-secrets.yaml`   | `dataui-<name>-secrets` (the three credentials)          |
| `10-pv.yaml`        | Two hostPath PersistentVolumes: db (2Gi), files (5Gi)    |
| `15-pvc.yaml`       | The matching PersistentVolumeClaims                      |
| `20-init-sql.yaml`  | ConfigMap with the schema init SQL                       |
| `30-postgres.yaml`  | PostgreSQL 16 Deployment + `postgres` Service            |
| `40-data-ui.yaml`   | The app Deployment + NodePort Service                    |

Highlights of `40-data-ui.yaml`:

- **An init container** waits for postgres to answer, then applies the schema SQL, but only if the `users` table doesn't already exist, so restarts don't re-run it.
- **The app container** gets its entire configuration through environment variables (12-factor style): DB coordinates, the three secrets via `secretKeyRef`, document root, the version tag.
- **Probes:** a `startupProbe` gives first boot up to 5 minutes (database initialization happens then); after startup succeeds, a `readinessProbe` (every 5s) gates traffic and a `livenessProbe` (every 15s) restarts a hung container. All three hit `GET /health`.
- **Strategy `Recreate`**, because the files PVC is ReadWriteOnce: a rolling update would deadlock with old and new pods both claiming it.
- **Swank lines** carry the `#@repl` marker in the template and exist only for REPL-enabled models.

## How HAProxy Routing Works

HAProxy was already serving other domains on this host, so Data UI had to move in without rearranging the furniture. The design adds exactly one line to the existing config, once, and after that **new instances never touch the main config at all.**

Three pieces:

### 1. The map file: `/etc/haproxy/data-ui.map`
A plain text file mapping hostnames to backend names:

    todo.demo.data-ui.com dataui-todos
    parts.demo.data-ui.com dataui-parts     # (a future instance)

### 2. One routing rule in the https frontend (added once)

    use_backend %[req.hdr(host),lower,map(/etc/haproxy/data-ui.map)] \
        if { req.hdr(host),lower,map(/etc/haproxy/data-ui.map) -m found }

In English: lowercase the Host header, look it up in the map; if found, route to that backend. One line handles every current and future Data UI instance. The deploy script inserts it (idempotently) just above the existing `default_backend` line.

### 3. Per-instance backend drop-ins: `/etc/haproxy/conf.d/dataui-<name>.cfg`

    backend dataui-todos
        mode http
        option forwardfor
        option httpchk GET /health
        server dataui-todos 172.18.0.2:30303 check inter 2000 rise 2 fall 3

That points at the k3d node's IP and the instance's NodePort, with an active health check against the same `/health` endpoint the Kubernetes probes use. (`assign_backend_target` is fail-closed on the k3d-published loopback hop `127.0.0.1:<NodePort+1000>` — e.g. `127.0.0.1:31303` — since 2026-09-30: if the hop does not answer, the deploy dies rather than render a bridge-IP target that goes stale when docker reassigns node IPs.) The `conf.d` directory is enabled via `EXTRAOPTS` in `/etc/default/haproxy` (also a one-time, idempotent step).

On every deploy, the script: installs/updates the backend file, upserts the map entry, **validates the whole config** (`haproxy -c` across the main file and conf.d), and only then reloads. If validation fails, nothing is reloaded and the old routing keeps working.

Locally-run host profiles (`scripts/data-ui profile expose`) use the same machinery with a different backend name: `dataui-profile-<name>` points at `127.0.0.1:<HTTP_PORT>` on the host instead of a k3d NodePort (the model `:name` values `profile` and `profile-*` are reserved so the two can never collide). One domain, one backend: `profile expose` refuses a map line owned by a deployed instance, and a deploy refuses a map line owned by a profile exposure — neither stops the other side's pods; `delete` (undeploy) first, then `profile expose`. `profile unexpose` (and `profile delete`) remove the exposure.

One subtlety, learned in production (where else): `systemctl reload haproxy` re-execs the master process *with its original command line*. If `EXTRAOPTS` was just modified to add `-f /etc/haproxy/conf.d`, a reload will not pick that up: the running master has never heard of conf.d, and your shiny new backend 503s while the NodePort works perfectly. The script handles this: the deploy that *first enables* conf.d does a full `systemctl restart`; every subsequent deploy does the gentler `reload`.

### 4. The demo directory and the 404 page (`demo404`)

Hosts under `*.demo.data-ui.com` that the map does *not* know about (typos, retired demos) used to be redirected to data-ui.com — a 301 browsers cache forever, which stranded visitors after a rename. They now get a real 404 page that names the requested host and lists the demos that *are* running. The bare apex, `demo.data-ui.com`, serves the same list as a normal 200 page — the directory.

HAProxy has no CGI, so the page comes from a tiny local service, `demo-directory.service`: a stdlib-only Python HTTP server on `127.0.0.1:8484` (hardened: `DynamicUser`, `ProtectSystem=strict`), wired in as the host-owned `demo404` backend (`ops/demo404.cfg`; no `dataui-` prefix, so the profile/deploy tooling never touches it). Two `use_backend demo404` rules in the https frontend cover the apex and the unmapped-host fallthrough; everything routed by the map is unaffected.

The list can never go stale: the service re-reads `/etc/haproxy/data-ui.map` on **every request**, so `profile expose` / `unexpose` and deploys are reflected instantly, with no restart. Hosts listed in the `EXCLUDE` set at the top of the script (currently `chat`, which is infrastructure, not a demo) stay routed but are never listed.

It survives reboots on both sides: the unit is `enable`d (a `Wants` symlink into `multi-user.target`) with `Restart=on-failure`, and HAProxy loads `conf.d` — and with it `demo404.cfg` — at boot via the `EXTRAOPTS` line in `/etc/default/haproxy`. If the service is down anyway, unmapped hosts get a plain HAProxy 503 (backend down); mapped demos keep working.

Sources live in `ops/` (`demo-directory.py`, `demo-directory.service`, `demo404.cfg`); the installed copies are targets, never edited in place. Everything is managed by the `demo-directory` CLI (source: `ops/demo-directory`, installed to `/usr/local/bin/demo-directory`; checkout override via `DATA_UI_CHECKOUT`):

    # one-time: install the CLI itself, then everything else
    sudo install -m 755 ops/demo-directory \
         /usr/local/bin/demo-directory
    demo-directory install    # script + unit + HAProxy backend,
                              # enable --now, validate, reload
    demo-directory status     # enabled/active/health/demo list
    demo-directory start|stop

#### Making a change to the page

There are two copies of the script: the source (`ops/demo-directory.py` in the repo) and the installed target (`/opt/demo-directory/demo-directory.py`, the one systemd runs). Edit the repo copy, then push it live with one command:

    demo-directory update

It syntax-checks the script (refusing to install a broken file), installs it over the target, restarts the service (Python loaded the old code at startup), and health-checks the result. Never edit `/opt` directly — the next update would silently overwrite it.

Verification: the 404 page is served `no-store`, so a bogus host shows changes instantly (`curl -s https://no-such-demo.demo.data-ui.com/`); the 200 directory page at the apex caches in a browser for 60s. If the service refuses to come up, `systemctl status demo-directory` / `journalctl -u demo-directory` show the startup line.

## TLS: Certificates That Renew Themselves

All demo instances live under `*.demo.data-ui.com`, so a single wildcard certificate covers every app, present and future. Adding a new instance requires zero TLS work. That is the entire point.

### The moving parts

- **DNS:** A wildcard A record `*.demo.data-ui.com` points at the deploy host's public IP (Route 53). A small cron-driven script (`update-dns`) re-upserts the record if the host's IP changes, because residential ISPs consider a stable IP a premium feature.
- **Certificate:** Let's Encrypt, obtained with certbot's Route 53 plugin. Wildcards require the DNS-01 challenge: certbot proves domain control by creating a TXT record, which it can do because it holds an IAM access key for exactly one capability: editing records in the data-ui.com hosted zone. (The IAM user, `certbot-evo-x2`, can do nothing else. Least privilege isn't paranoia; it's just manners.)
- **HAProxy:** the `:443` bind has `crt /etc/haproxy/certs/`, a *directory*. HAProxy loads every pem in it and uses SNI to pick the right certificate per hostname. New cert for a new domain family? Drop a pem in the directory, reload. No bind-line surgery.

### One-time setup

`deploy/setup-tls.sh` does the whole dance idempotently: installs the IAM credentials for root (renewals run as root), installs the renewal hook, runs `certbot certonly --dns-route53` for `demo.data-ui.com` + `*.demo.data-ui.com`, builds the combined pem, patches the bind line, validates, reloads. Run it once per deploy host and forget it.

Note the wildcard covers `anything.demo.data-ui.com` but **not** the bare `demo.data-ui.com`; that's why the cert requests both names.

### Renewal (the part where you do nothing)

The `certbot.timer` systemd unit fires twice a day. When the cert is within 30 days of expiry, certbot renews it via DNS-01 and then runs the deploy hook installed at `/etc/letsencrypt/renewal-hooks/deploy/haproxy-pem.sh` (source: `deploy/letsencrypt-haproxy-hook.sh`), which:

1. concatenates `fullchain.pem` + `privkey.pem` into `/etc/haproxy/certs/demo.data-ui.com.pem` (HAProxy wants one file; written atomically via a `.new` + `mv`, mode 600),
2. reloads HAProxy.

To rehearse the whole thing without touching the real certificate:

    sudo certbot renew --dry-run

If that passes, future-you has nothing to do, ever. Past-you already did it.

## Docker Credentials: Headless Deploys

Deploys are triggered two ways: from a shell (interactive, you are present) and by the **Deploy button** on Model Bank, which runs the deploy script from inside the app's process — a headless systemd context with nobody watching. The second way must never depend on a human answering a prompt. One of the two prompt traps was fixed with the NOPASSWD sudoers rule for HAProxy; the other is Docker's credential helper, and it bites quietly:

`~/.docker/config.json` says `"credsStore": "pass"`, so every registry operation — including the anonymous-looking pulls of `node:22-slim` and `ubuntu` inside `docker build` — consults `docker-credential-pass`, which decrypts `~/.password-store` with the user's GPG key. If gpg-agent's cache is cold, gpg pops a **pinentry dialog on the host desktop** and waits. Nobody answers (it's a guest's deploy; the dialog isn't even on their screen), the helper times out or gets a wrong passphrase, and the build dies with:

    getting credentials - err: exit status 1, out: `exit status 2:
    gpg: public key decryption failed: Bad passphrase
    gpg: decryption failed: Bad passphrase`
    ERROR: Docker build failed.

A wrong passphrase in that dialog — typed in haste at the desktop — produces exactly this. A *correct* one only buys you gpg-agent's cache TTL; the next guest, hours later, hangs again.

### The fix on this host (done, 2026-09-30)

The password store now encrypts to a **dedicated passphrase-less GPG key**. Decryption never prompts, so credential lookups work in any context — cold boot, systemd, ssh, cron. The tradeoff: the store's contents are readable by anyone who can read `~/.password-store` (file permissions, not cryptography, protect them). That's acceptable here because the store holds nothing but the Docker Hub token; the protecting passphrase was guarding nearly nothing, while the prompt was breaking guest deploys entirely.

What was done:

    # dedicated no-passphrase key (encryption subkey, never expires)
    gpg --batch --passphrase '' --quick-generate-key \
        "data-ui-docker-creds (pass store, no passphrase)" default default never
    FPR=$(gpg --list-secret-keys --with-colons "data-ui-docker-creds" \
          | awk -F: '/^fpr:/ {print $10; exit}')
    gpg --batch --passphrase '' --quick-add-key "$FPR" default encr never

    # two stale artifacts of an old docker login (index.docker.io/v1/
    # access-token and refresh-token subpaths) — the helper only needs
    # the plain index.docker.io/v1/ entry
    pass rm -f 'docker-credential-helpers/<b64>/macnod'   # x2, the stale ones

    # re-encrypt the store (prompts once for the old key's passphrase)
    pass init "$FPR"

If the one-time pinentry times out during `pass init` (nobody at the keyboard), just re-run it — the .gpg-id is already updated and each retry re-encrypts what remains.

### The check (run after any credential change)

    # must print JSON, no dialog, no delay — even right after
    # gpgconf --kill gpg-agent (i.e. with nothing cached)
    gpgconf --kill gpg-agent
    echo "https://index.docker.io/v1/" | docker-credential-pass get

That kills gpg-agent first on purpose: a warm cache can mask a broken setup, which is exactly how the first button deploys passed and a later guest deploy failed.

### Notes for the future

- A `docker login` on this host stores the token into the `pass` store under the same no-passphrase key — nothing to redo after a re-login.
- Prefer a **read-only, non-expiring** Docker Hub access token: the deploy pipeline only ever pulls public base images; authenticated pulls just rate-limit better. When such a token does expire, the failure mode is a 401-and-anonymous-fallback, not a prompt — still no dialogs.
- A deploy running under some *other* user would read that user's `~/.docker/config.json`, not this one; every host that runs button deploys needs the same treatment (or a bare config with no `credsStore` at all, which is the anonymous-pull alternative and works fine for public images).

## Deploying From Another Machine

You don't have to be on the deploy host. If `hostname` doesn't match `$DEPLOY_HOST` (default `evo-x2`), the script:

1. requires a clean tree and a non-detached branch,
2. creates the release tag locally (so its provenance is *your* machine) and pushes branch + tag to origin,
3. ssh-es to the deploy host and re-runs itself in a dedicated **deploy clone** at `$DEPLOY_CHECKOUT` (default `~/deploy/data-ui`), checked out at your exact commit.

The deploy clone exists so a remote deploy can never disturb whatever work-in-progress lives in the deploy host's development checkout. The ssh session allocates a tty (`-t`) for one reason only: the HAProxy step needs to ask for your sudo password.

Configuration knobs (environment variables, all with defaults):

| Variable          | Default                          | Meaning                       |
|-------------------|----------------------------------|-------------------------------|
| `DEPLOY_HOST`     | `evo-x2`                         | ssh destination & hostname    |
| `DEPLOY_CHECKOUT` | `$HOME/deploy/data-ui`           | deploy clone location         |
| `DEPLOY_STATE_DIR`| `/data/data-ui/deploy`           | state directory               |
| `DATA_UI_STATE`   | `/data/data-ui`                  | root for deploy state         |
| `MODEL_FILE`      | `models/<name>.lisp`             | override model path (VIP deploys) |
| `K3D_CLUSTER`     | `evo-x2`                         | k3d cluster name              |
| `DRY_RUN`         | (unset)                          | stop after rendering manifests|

## Dry Runs

    DRY_RUN=1 scripts/data-ui deploy todos

Runs the model compile, fact gathering, port assignment, secrets handling, and manifest rendering, then stops. No tag, no image, no cluster changes, no HAProxy. The rendered manifests sit in the release directory for your inspection. Make this a habit before any deploy that changes templates.

## Connecting a REPL to the Live App

If the model was deployed with `:repl t`, the container runs a Swank server on port 4005, reachable *only* through Kubernetes port forwarding (it is never exposed via Service, NodePort, or HAProxy):

    kubectl -n dataui-todos port-forward deploy/dataui-todos 4005:4005

Then in Emacs: `M-x slime-connect RET localhost RET 4005`, and you have a live REPL inside the running production container. Inspect the compiled model, poke RBAC state, debug a hook: the full Lisp experience against the deployed instance. With great power, et cetera: this is the expert-tier escape hatch, and production models should ship `:repl nil`.

## Troubleshooting

### A diagnostic ladder

Work from the inside out; each rung isolates one layer:

    # 1. Are the pods up?
    kubectl -n dataui-todos get pods

    # 2. Does the app answer inside the cluster? (NodePort, bypasses HAProxy)
    curl http://<node-ip>:30303/health        # expect: OK

    # 3. Does HAProxy route it? (full path: TLS, map, backend)
    curl https://todo.demo.data-ui.com/health # expect: OK

    # 4. Does login work? (POST the real admin password; see Secrets)
    curl -s -X POST https://todo.demo.data-ui.com/api/login \
      -H 'Content-Type: application/json' \
      -d '{"username":"admin","password":"<see Secrets section>"}'

    # 5. Login still 401s? Read the Secrets section again — a deploy
    #    never rewrites admin credentials, so the password is exactly
    #    what the cluster Secret says.

If (2) works and (3) doesn't, it's HAProxy: check `/etc/haproxy/data-ui.map` for the domain, `/etc/haproxy/conf.d/` for the backend file, and remember the reload-vs-restart subtlety above.

### Reading the app logs

    kubectl -n dataui-todos logs deploy/dataui-todos            # current
    kubectl -n dataui-todos logs deploy/dataui-todos --previous # last crash

The app logs structured JSON lines. A crash prints a full backtrace and exits (`--disable-debugger`), so the evidence is always in `--previous`.

### Known failure modes

- **CrashLoopBackOff with "permission 'create' already exists":** the database is half-initialized: a previous first boot died partway through RBAC initialization (which is not yet idempotent; a post-MVP fix). Recovery: clean slate (below). The classic trigger, an admin password that failed rbac's complexity policy, is fixed, but other mid-init interruptions (OOM, node reboot) could reproduce it.
- **503 from the domain, NodePort fine:** HAProxy doesn't know the backend. Almost always the conf.d/EXTRAOPTS reload-vs-restart issue, on a host where conf.d was newly enabled.
- **401s in the browser after a redeploy:** are you *sure* you're using the current admin password? Read [Secrets](#secrets-and-how-to-get-the-admin-password). Ask us how we know.
- **Docker build suddenly slow:** check the build context size in the first lines of build output. A fat log file in the repo once inflated the context to 44 GB. `.dockerignore` excludes `*.log` now, but entropy never sleeps.
- **Docker build pathologically slow (minutes per `RUN ros` step, low I/O, ~1 core CPU):** first build after the Ubuntu 26.04 upgrade (Sept 2026): BuildKit `RUN` processes inherited the daemon's `LimitNOFILE=infinity` (=2147483584), and SBCL's startup fd sweep (`close()` across the whole fd space) takes minutes at that limit — one `ros install sbcl-bin` went from ~2s to 592s. Two fixes, both in place: a systemd drop-in (`/etc/systemd/system/docker.service.d/limit-nofile.conf`, `LimitNOFILE=1048576`) and `ulimit -n 1048576 &&` prefixes on every `RUN ros …` in the Dockerfile (host-independent). Verified: same step 592s → 4s.
- **"getting credentials ... Bad passphrase", then "Docker build failed":** Docker tried to consult the `pass` credential store and gpg prompted. Should be extinct since the store moved to a no-passphrase key — see [Docker Credentials: Headless Deploys](#docker-credentials-headless-deploys). If it returns, first suspect a new `credsStore` entry in `~/.docker/config.json` (a stray `docker login` on another host user), not the store itself.

### Rate limiting

`scripts/data-ui traffic` reports the HAProxy per-IP rate-limit counters (stick-table in the https frontend) and flags clients near or over the limits. It needs sudo for the HAProxy admin socket.

## Starting Over: the Clean-Slate Procedure

The fast path: delete the whole instance with one command:

    scripts/data-ui delete todos

It deletes the namespace and PVs, wipes the instance's data on the host, removes the HAProxy backend and map entry, and removes the deploy state (including secrets), then prompts you to type the instance name before doing any of it (set `FORCE=1` to skip the prompt, e.g. in a deploy/record/delete rehearsal loop). Before destroying anything it saves a farewell snapshot of the instance's last state to the shared pool as `prd-<name>-last` (the prd twin of the stop verb's `-last` slot; `FORCE=1` skips that too, and a failed save aborts the delete with the data still intact). Docker images, git release tags, and the farewell snapshot are left alone. A failed delete can simply be re-run; every step tolerates already-deleted resources, and the re-run skips the farewell (nothing left to save).

The manual equivalent, if you want to do it piecewise (or only partway; steps 1–3 are enough for a clean redeploy of the same instance):

    # 1. Remove the app and its namespace
    kubectl delete namespace dataui-todos

    # 2. PVs are cluster-scoped with Retain policy; delete them explicitly
    kubectl delete pv dataui-todos-demo-db-pv dataui-todos-demo-files-pv

    # 3. Wipe the instance's data on the host (root-owned; same path
    #    on host and node via the /data/k8s bind)
    sudo rm -rf /data/k8s/data-ui/todos-demo

    # 4. Optional: drop cached secrets to get fresh credentials
    rm /data/data-ui/deploy/todos/secrets.env

    # 5. Deploy
    scripts/data-ui deploy todos

Steps 2 and 3 are the ones people forget. A Released PV refuses to bind to a new claim, and stale postgres data under `/data/k8s/data-ui` will be happily adopted by the new instance, old password and all. (Or skip the list entirely and use `scripts/data-ui delete`, which forgets nothing — except the farewell snapshot it deliberately leaves in the pool.)

---

That's the machine. A napkin's worth of model in, a running application out: database, API, RBAC, frontend, TLS, DNS, all of it derived, none of it hand-maintained. The 10,000 lines you didn't write are the feature.
