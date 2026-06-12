# Deploying a Data UI Application

*From a model that fits on a napkin to a live, TLS-terminated, RBAC-backed
web application — with one command.*

This document explains everything about how Data UI deployment works: the
big picture, the command, every moving part behind it, where the secrets
live, how the cert renews itself at 3am while you sleep, and what to do on
the rare day something goes sideways. It is written so that a junior
programmer can follow along. If you can run a shell command and have seen
a YAML file without crying, you are qualified.

## Table of Contents

- [The Big Picture](#the-big-picture)
- [Quick Start](#quick-start)
- [What `deploy` Actually Does, Step by Step](#what-deploy-actually-does-step-by-step)
- [The Model Drives Everything](#the-model-drives-everything)
- [Deployment State: Where Things Live](#deployment-state-where-things-live)
- [Secrets, and How to Get the Admin Password](#secrets-and-how-to-get-the-admin-password)
- [The Kubernetes Manifests](#the-kubernetes-manifests)
- [How HAProxy Routing Works](#how-haproxy-routing-works)
- [TLS: Certificates That Renew Themselves](#tls-certificates-that-renew-themselves)
- [Deploying From Another Machine](#deploying-from-another-machine)
- [Dry Runs](#dry-runs)
- [Connecting a REPL to the Live App](#connecting-a-repl-to-the-live-app)
- [Troubleshooting](#troubleshooting)
- [Starting Over: the Clean-Slate Procedure](#starting-over-the-clean-slate-procedure)

## The Big Picture

Most deployment stories involve a wall of YAML you wrote by hand, a wiki
page titled "DO NOT TOUCH unless you are Dave," and a Dave who left the
company in 2023. Data UI's story is shorter: **the model is the
configuration.**

Your model already declares the application's identity:

    (:title "To Do List"
      :name "todo"
      :version "0.1"
      :domain "todo.demo.data-ui.com"
      :repl t
      :types ...)

The deploy pipeline reads those five keys and derives *everything* from
them: the Docker image tag, the Kubernetes namespace, the release
directory, the HAProxy backend, the public URL. There is no separate
deployment config to drift out of sync with the application, because
there is no separate deployment config.

The target environment is deliberately modest: a single machine (the
"deploy host") running a [k3d](https://k3d.io) cluster (k3s in Docker),
with HAProxy in front terminating TLS. No cloud bill, no managed
Kubernetes, no Helm charts. One box, one command.

## Quick Start

On the deploy host (or any machine with ssh access to it — see
[Deploying From Another Machine](#deploying-from-another-machine)):

    cd data-ui
    scripts/data-ui deploy

That's it. The output ends with:

    Deployed To Do List todo-0.1-6d8f586.
      Domain:   https://todo.demo.data-ui.com
      NodePort: http://172.18.0.2:30300
      Swank:    kubectl -n dataui-todo port-forward deploy/dataui-todo 4005:4005

Open the domain, log in as `admin` with the password from
[Secrets](#secrets-and-how-to-get-the-admin-password), and you are
looking at your deployed application.

Requirements:

- A clean git tree (the deploy tags the exact commit it ships; uncommitted
  changes would make the tag a lie).
- `sudo` access on the deploy host (for the HAProxy update, nothing else).
- The one-time host setup already done: k3d cluster, HAProxy, TLS
  certificate (see [TLS](#tls-certificates-that-renew-themselves)).

## What `deploy` Actually Does, Step by Step

`scripts/data-ui deploy` runs the following phases, in order. Each phase
is a shell function in `scripts/data-ui`, so the script is the
authoritative reference; this is the guided tour.

### 1. Compile the model (the gate)

    compile_default_model

Before anything ships, the model in `models/default-model.lisp` must
compile. The script starts a *throwaway* PostgreSQL container (its own
container name and port 5446, so it never collides with your dev REPL
database on 5444 or the test database on 5445), initializes the schema,
and runs the compilation phase of `set-model` — validation, SQL
generation, lambda compilation — against it. Then the container is
destroyed.

If the model doesn't compile, the deploy dies right here, before a tag,
an image, or a manifest exists. A broken model never gets anywhere near
the cluster.

### 2. Read identity from the model

    gather_deploy_facts

The script asks the model for its `:name`, `:title`, `:version`,
`:domain`, and `:repl` values (via a tiny Lisp helper,
`lisp/deployment.lisp`). From those plus the git short hash, it derives:

| Fact         | Example                                              |
|--------------|------------------------------------------------------|
| TAG          | `todo-0.1-6d8f586`                                   |
| IMAGE        | `macnod/data-ui:todo-0.1-6d8f586`                    |
| NAMESPACE    | `dataui-todo`                                        |
| RELEASE_NAME | `To Do List todo-0.1-6d8f586`                        |
| OUT_DIR      | `~/.local/state/data-ui-deploy/todo/releases/0.1-6d8f586` |

### 3. Tag the release

    create_deploy_tag

An annotated git tag (`todo-0.1-6d8f586`) is created at HEAD. If the tag
already exists *and points at HEAD*, it is reused (re-deploying the same
commit is fine). If it exists and points elsewhere, the deploy aborts —
a tag must never silently change meaning.

### 4. Allocate a NodePort

    assign_node_port

Each instance gets a stable NodePort (starting at 30300). The assignment
is cached in `~/.local/state/data-ui-deploy/ports.lock`, but the cache is
not the source of truth — the *cluster* is. If the lock file is missing,
the port is recovered from the live Service. Only when neither exists is
a new port assigned (lowest free port ≥ 30300, checked against both the
lock file and every NodePort in the cluster).

### 5. Ensure instance secrets

    ensure_instance_secrets

See [Secrets](#secrets-and-how-to-get-the-admin-password). Same
cache-vs-truth design: file missing → recover from the live Kubernetes
Secret; no live Secret either → generate fresh credentials. Credentials
are *never* regenerated for an existing instance, because the PostgreSQL
volume keeps the old password and new credentials would lock the app out
of its own database.

### 6. Render the manifests

    generate_manifests

The templates in `deploy/templates/*.yaml.tpl` are rendered with plain
`sed` substitution of `{{PLACEHOLDERS}}` — no templating engine, no
dependencies, nothing to install. Two special cases:

- Lines ending in `#@repl` survive (marker stripped) only when the
  model says `:repl t`; otherwise they are deleted. This is how the
  Swank port appears in the manifest for REPL-enabled instances and
  doesn't exist at all for production ones.
- The database init SQL is generated from `tests/init-template.sql` and
  wrapped into a ConfigMap with `kubectl create configmap --dry-run`.

Rendered manifests land in the release directory (`OUT_DIR` above), so
every release's exact manifests are preserved for inspection or rollback.

### 7. Build and import the image

    build_image

A multi-stage Docker build:

- **Stage 1 (node:22-slim):** `npm ci && npm run build` — the React
  frontend, typechecked and bundled by Vite into static files.
- **Stage 2 (ubuntu):** Roswell + SBCL + all Lisp dependencies, then the
  data-ui source. The system is **pre-compiled at build time** so
  container startup loads fasls instead of compiling from scratch
  (this matters: slow startups once fought the liveness probe, and the
  probe won). The entrypoint runs with `--disable-debugger` so any
  unhandled error prints a backtrace and exits instead of waiting
  politely at a debugger prompt inside a container nobody is attached to.

The image is then imported into the k3d cluster with `k3d image import` —
no registry involved; nothing leaves the machine.

### 8. Apply the manifests

    apply_manifests

`kubectl apply -f $OUT_DIR`, then wait for the PostgreSQL rollout, then
the app rollout. First boot initializes the database (RBAC tables, roles,
permissions, admin/guest users), which is why the app's startupProbe
allows up to five minutes before the liveness probe is allowed to have
opinions.

### 9. Update HAProxy

    update_haproxy

The only step that needs sudo. Details in
[How HAProxy Routing Works](#how-haproxy-routing-works).

## The Model Drives Everything

Worth repeating with the actual flow drawn out:

    models/default-model.lisp
        :name "todo" ──────────┬─→ namespace  dataui-todo
        :version "0.1" ────────┼─→ tag        todo-0.1-<git-hash>
        :domain "todo.demo..." ┼─→ HAProxy map entry + backend
        :repl t ───────────────┴─→ Swank port in the manifest (or not)

Change `:domain` in the model and redeploy: the new domain routes to the
app. Bump `:version`: new tag, new release directory. Set `:repl nil`:
the Swank listener vanishes from the deployment. The model is not *input
to* the configuration — it *is* the configuration.

(Note the TODO in the example model: `:repl` should be `nil` in
production. The Swank port is never exposed through a Service either way —
it is reachable only via `kubectl port-forward`, which requires cluster
credentials.)

## Deployment State: Where Things Live

Deployment state lives **outside the repository**, in
`~/.local/state/data-ui-deploy/` on the deploy host:

    ~/.local/state/data-ui-deploy/
    ├── ports.lock                      # name env port, one per line
    └── todo/
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

Why outside the repo? Because rendered manifests are *derived output*
(rebuildable from model + templates + state) and secrets are, well,
secret. The repo holds source; the state directory holds facts about one
particular machine's cluster. Deleting the whole state directory is
recoverable: ports and secrets are re-read from the live cluster on the
next deploy.

Application *data* lives in a third place: the k3d cluster mounts
`~/k3d/volumes/dataui` (host) at `/data/dataui` (node), and each
instance's PersistentVolumes use
`/data/dataui/<name>-<env>/{db,files}`. So even `k3d cluster delete`
cannot destroy application data — it survives on the host filesystem.

Three layers, three lifetimes:

| Layer                          | Lives                         | Survives                  |
|--------------------------------|-------------------------------|---------------------------|
| Source (model, templates)      | git repo                      | everything                |
| Deploy state (secrets, ports)  | `~/.local/state/data-ui-deploy` | cluster recreation      |
| App data (database, files)     | `~/k3d/volumes/dataui`        | cluster deletion          |

## Secrets, and How to Get the Admin Password

Each instance has exactly three secrets, generated once at first deploy:

- `DB_PASSWORD` — PostgreSQL password for the `dataui` user
- `ADMIN_PASSWORD` — the application's `admin` login
- `JWT_SECRET` — signs the API's access and refresh tokens

### Getting the admin password

The easy way (on the deploy host):

    grep ADMIN_PASSWORD ~/.local/state/data-ui-deploy/todo/secrets.env

The canonical way (works even if the secrets file is gone, from any
machine with cluster access):

    kubectl get secret -n dataui-todo dataui-todo-secrets \
        -o jsonpath='{.data.admin-password}' | base64 -d; echo

Both should agree. If they don't, trust the cluster — the file is a
cache; the Secret is the truth. (This is a recurring design theme. When
a cache and the cluster disagree, the cluster wins, the same way the
dictionary wins at Scrabble.)

A war story, so you don't repeat it: the very first end-to-end deploy
"failed" with a wall of 401s. Backend verified healthy, JWTs verified
valid, much head-scratching — the operator was logging in with the *old*
admin password from a previous instance's secrets. If your freshly
deployed app rejects you, read the password again, slowly.

### Password format trivia

`ADMIN_PASSWORD` is generated as `$(openssl rand -hex 8)-a1`. The
`-a1` suffix is not decoration: the rbac library's password policy
requires at least one letter, one digit, and one punctuation character,
and sixteen random hex characters can satisfy the first two but never
the third. The suffix guarantees all three, and the sixteen random hex
chars provide the entropy. Yes, this was learned the hard way. No, the
instance that taught us is no longer with us.

### Rotation

There is no rotation tooling yet. If you must rotate manually: update
the Kubernetes Secret, update `secrets.env`, restart the deployment, and
for `DB_PASSWORD` also `ALTER USER dataui PASSWORD ...` inside postgres
— in that order of caution. For a demo instance, the clean-slate
procedure below is honestly less error-prone.

## The Kubernetes Manifests

Each instance is fully isolated in its own namespace, `dataui-<name>`.
The manifests, in apply order:

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

- **An init container** waits for postgres to answer, then applies the
  schema SQL — but only if the `users` table doesn't already exist, so
  restarts don't re-run it.
- **The app container** gets its entire configuration through
  environment variables (12-factor style): DB coordinates, the three
  secrets via `secretKeyRef`, document root, the version tag.
- **Probes:** a `startupProbe` gives first boot up to 5 minutes
  (database initialization happens then); after startup succeeds, a
  `readinessProbe` (every 5s) gates traffic and a `livenessProbe`
  (every 15s) restarts a hung container. All three hit `GET /health`.
- **Strategy `Recreate`**, because the files PVC is ReadWriteOnce — a
  rolling update would deadlock with old and new pods both claiming it.
- **Swank lines** carry the `#@repl` marker in the template and exist
  only for REPL-enabled models.

## How HAProxy Routing Works

HAProxy was already serving other domains on this host, so Data UI had
to move in without rearranging the furniture. The design adds exactly
one line to the existing config, once, and after that **new instances
never touch the main config at all.**

Three pieces:

### 1. The map file: `/etc/haproxy/data-ui.map`

A plain text file mapping hostnames to backend names:

    todo.demo.data-ui.com dataui-todo
    parts.demo.data-ui.com dataui-parts     # (a future instance)

### 2. One routing rule in the https frontend (added once)

    use_backend %[req.hdr(host),lower,map(/etc/haproxy/data-ui.map)] \
        if { req.hdr(host),lower,map(/etc/haproxy/data-ui.map) -m found }

In English: lowercase the Host header, look it up in the map; if found,
route to that backend. One line handles every current and future Data UI
instance. The deploy script inserts it (idempotently) just above the
existing `default_backend` line.

### 3. Per-instance backend drop-ins: `/etc/haproxy/conf.d/dataui-<name>.cfg`

    backend dataui-todo
        mode http
        option forwardfor
        option httpchk GET /health
        server dataui-todo 172.18.0.2:30300 check inter 2000 rise 2 fall 3

That points at the k3d node's IP and the instance's NodePort, with an
active health check against the same `/health` endpoint the Kubernetes
probes use. The `conf.d` directory is enabled via `EXTRAOPTS` in
`/etc/default/haproxy` (also a one-time, idempotent step).

On every deploy, the script: installs/updates the backend file, upserts
the map entry, **validates the whole config** (`haproxy -c` across the
main file and conf.d), and only then reloads. If validation fails,
nothing is reloaded and the old routing keeps working.

One subtlety, learned in production (where else): `systemctl reload
haproxy` re-execs the master process *with its original command line*.
If `EXTRAOPTS` was just modified to add `-f /etc/haproxy/conf.d`, a
reload will not pick that up — the running master has never heard of
conf.d, and your shiny new backend 503s while the NodePort works
perfectly. The script handles this: the deploy that *first enables*
conf.d does a full `systemctl restart`; every subsequent deploy does the
gentler `reload`.

## TLS: Certificates That Renew Themselves

All demo instances live under `*.demo.data-ui.com`, so a single wildcard
certificate covers every app, present and future. Adding a new instance
requires zero TLS work. That is the entire point.

### The moving parts

- **DNS:** A wildcard A record `*.demo.data-ui.com` points at the deploy
  host's public IP (Route 53). A small cron-driven script
  (`update-dns`) re-upserts the record if the host's IP changes,
  because residential ISPs consider a stable IP a premium feature.
- **Certificate:** Let's Encrypt, obtained with certbot's Route 53
  plugin. Wildcards require the DNS-01 challenge — certbot proves
  domain control by creating a TXT record, which it can do because it
  holds an IAM access key for exactly one capability: editing records
  in the data-ui.com hosted zone. (The IAM user, `certbot-evo-x2`, can
  do nothing else. Least privilege isn't paranoia; it's just manners.)
- **HAProxy:** the `:443` bind has `crt /etc/haproxy/certs/` —
  a *directory*. HAProxy loads every pem in it and uses SNI to pick the
  right certificate per hostname. New cert for a new domain family?
  Drop a pem in the directory, reload. No bind-line surgery.

### One-time setup

`deploy/setup-tls.sh` does the whole dance idempotently: installs the
IAM credentials for root (renewals run as root), installs the renewal
hook, runs `certbot certonly --dns-route53` for `demo.data-ui.com` +
`*.demo.data-ui.com`, builds the combined pem, patches the bind line,
validates, reloads. Run it once per deploy host and forget it.

Note the wildcard covers `anything.demo.data-ui.com` but **not** the
bare `demo.data-ui.com` — that's why the cert requests both names.

### Renewal (the part where you do nothing)

The `certbot.timer` systemd unit fires twice a day. When the cert is
within 30 days of expiry, certbot renews it via DNS-01 and then runs the
deploy hook installed at
`/etc/letsencrypt/renewal-hooks/deploy/haproxy-pem.sh`
(source: `deploy/letsencrypt-haproxy-hook.sh`), which:

1. concatenates `fullchain.pem` + `privkey.pem` into
   `/etc/haproxy/certs/demo.data-ui.com.pem` (HAProxy wants one file;
   written atomically via a `.new` + `mv`, mode 600),
2. reloads HAProxy.

To rehearse the whole thing without touching the real certificate:

    sudo certbot renew --dry-run

If that passes, future-you has nothing to do, ever. Past-you already
did it.

## Deploying From Another Machine

You don't have to be on the deploy host. If `hostname` doesn't match
`$DEPLOY_HOST` (default `evo-x2`), the script:

1. requires a clean tree and a non-detached branch,
2. creates the release tag locally (so its provenance is *your* machine)
   and pushes branch + tag to origin,
3. ssh-es to the deploy host and re-runs itself in a dedicated **deploy
   clone** at `$DEPLOY_CHECKOUT` (default `~/deploy/data-ui`), checked
   out at your exact commit.

The deploy clone exists so a remote deploy can never disturb whatever
work-in-progress lives in the deploy host's development checkout. The
ssh session allocates a tty (`-t`) for one reason only: the HAProxy step
needs to ask for your sudo password.

Configuration knobs (environment variables, all with defaults):

| Variable          | Default                          | Meaning                       |
|-------------------|----------------------------------|-------------------------------|
| `DEPLOY_HOST`     | `evo-x2`                         | ssh destination & hostname    |
| `DEPLOY_CHECKOUT` | `$HOME/deploy/data-ui`           | deploy clone location         |
| `DEPLOY_STATE_DIR`| `~/.local/state/data-ui-deploy`  | state directory               |
| `K3D_CLUSTER`     | `evo-x2`                         | k3d cluster name              |
| `DRY_RUN`         | (unset)                          | stop after rendering manifests|

## Dry Runs

    DRY_RUN=1 scripts/data-ui deploy

Runs the model compile, fact gathering, port assignment, secrets
handling, and manifest rendering — then stops. No tag, no image, no
cluster changes, no HAProxy. The rendered manifests sit in the release
directory for your inspection. Make this a habit before any deploy that
changes templates.

## Connecting a REPL to the Live App

If the model was deployed with `:repl t`, the container runs a Swank
server on port 4005 — reachable *only* through Kubernetes port
forwarding (it is never exposed via Service, NodePort, or HAProxy):

    kubectl -n dataui-todo port-forward deploy/dataui-todo 4005:4005

Then in Emacs: `M-x slime-connect RET localhost RET 4005`, and you have
a live REPL inside the running production container. Inspect the
compiled model, poke RBAC state, debug a hook — the full Lisp experience
against the deployed instance. With great power, et cetera: this is the
expert-tier escape hatch, and production models should ship `:repl nil`.

## Troubleshooting

### A diagnostic ladder

Work from the inside out; each rung isolates one layer:

    # 1. Are the pods up?
    kubectl -n dataui-todo get pods

    # 2. Does the app answer inside the cluster? (NodePort, bypasses HAProxy)
    curl http://<node-ip>:30300/health        # expect: OK

    # 3. Does HAProxy route it? (full path: TLS, map, backend)
    curl https://todo.demo.data-ui.com/health # expect: OK

    # 4. Is the API alive end to end?
    curl -s -X POST https://todo.demo.data-ui.com/api/login \
      -H 'Content-Type: application/json' \
      -d '{"username":"admin","password":"<see Secrets section>"}'

If (2) works and (3) doesn't, it's HAProxy: check
`/etc/haproxy/data-ui.map` for the domain, `/etc/haproxy/conf.d/` for
the backend file, and remember the reload-vs-restart subtlety above.

### Reading the app logs

    kubectl -n dataui-todo logs deploy/dataui-todo            # current
    kubectl -n dataui-todo logs deploy/dataui-todo --previous # last crash

The app logs structured JSON lines. A crash prints a full backtrace and
exits (`--disable-debugger`), so the evidence is always in `--previous`.

### Known failure modes

- **CrashLoopBackOff with "permission 'create' already exists":** the
  database is half-initialized — a previous first boot died partway
  through RBAC initialization (which is not yet idempotent; a post-MVP
  fix). Recovery: clean slate (below). The classic trigger — an admin
  password that failed rbac's complexity policy — is fixed, but other
  mid-init interruptions (OOM, node reboot) could reproduce it.
- **503 from the domain, NodePort fine:** HAProxy doesn't know the
  backend. Almost always the conf.d/EXTRAOPTS reload-vs-restart issue,
  on a host where conf.d was newly enabled.
- **401s in the browser after a redeploy:** are you *sure* you're using
  the current admin password? Read
  [Secrets](#secrets-and-how-to-get-the-admin-password). Ask us how we
  know.
- **Docker build suddenly slow:** check the build context size in the
  first lines of build output. A fat log file in the repo once inflated
  the context to 44 GB. `.dockerignore` excludes `*.log` now, but
  entropy never sleeps.

## Starting Over: the Clean-Slate Procedure

For a demo instance, the fastest fix for a wedged deployment is a clean
rebuild. This destroys the instance's data; that is the point.

    # 1. Remove the app and its namespace
    kubectl delete namespace dataui-todo

    # 2. PVs are cluster-scoped with Retain policy; delete them explicitly
    kubectl delete pv dataui-todo-demo-db-pv dataui-todo-demo-files-pv

    # 3. Wipe the instance's data on the host (root-owned; via the node)
    docker exec k3d-evo-x2-server-0 rm -rf /data/dataui/todo-demo

    # 4. Optional: drop cached secrets to get fresh credentials
    rm ~/.local/state/data-ui-deploy/todo/secrets.env

    # 5. Deploy
    scripts/data-ui deploy

Steps 2 and 3 are the ones people forget. A Released PV refuses to bind
to a new claim, and stale postgres data under `/data/dataui` will be
happily adopted by the new instance — old password and all.

---

That's the machine. A napkin's worth of model in, a running application
out: database, API, RBAC, frontend, TLS, DNS — all of it derived, none
of it hand-maintained. The 10,000 lines you didn't write are the
feature.
