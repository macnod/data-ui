# Competitive Landscape

How Data UI compares to existing tools for building database-backed,
RBAC-protected applications.


## The Gap in the Market

Existing tools occupy points along a spectrum — from raw frameworks
(Rails, Django) to low-code platforms (Directus, Budibase) to AI code
generators (Bolt.new, v0). Each addresses a slice of the problem:
scaffolding CRUD, or wrapping a database with an API, or generating
frontend code from a prompt. But none covers the full distance from
data model to deployed, secured, multi-user application.

The gaps are not at the margins. They are structural:

- **RBAC is bolted on, not compiled in.** Every platform treats
  permissions as a separate configuration layer — something you set up
  after the app exists, per table, per role, per operation. Nobody
  compiles RBAC from the data model itself, let alone guarantees that
  every role × resource × operation combination is consistent as the
  model evolves.
- **Deployment is someone else's problem.** You can generate an app,
  or you can deploy an app, but the same tool does not do both from the
  same artifact. The model doesn't know its own domain. TLS
  certificates, Kubernetes manifests, HAProxy routing, persistent
  volumes — these are manual operations layered on top, not properties
  of the model.
- **Custom logic breaks the model.** When an application needs
  validation hooks, lifecycle behavior, or write-through to related
  tables, every tool falls back to imperative code — Python
  controllers, JavaScript functions, shell scripts. The model is no
  longer the whole application. The guarantees evaporate.
- **Output is code, not a compiled artifact.** AI generators produce
  source code that must be read, understood, and maintained. Low-code
  platforms store configuration in databases, not in reviewable text.
  Neither produces a single, compact, auditable artifact that *is* the
  application.

Data UI is not a better version of these tools. It is a different
category: a compiler that takes a small declarative model and produces
a complete, running application — database schema, RBAC enforcement,
REST API, React frontend, validation hooks compiled to native code, and
one-command deployment to TLS at the model's own domain. The model is
the application. The compiler guarantees consistency across the entire
expansion. There is no second system to configure, no separate deploy
pipeline to operate, no imperative code that drifts from the spec.


## The Three Differentiators

### 1. One-click deploy to TLS at a model-specified domain

The model declares its own domain:

```lisp
(:domain "todo.demo.data-ui.com")
```

One command — `scripts/data-ui deploy` — compiles the model, builds a
Docker image, renders Kubernetes manifests, provisions PostgreSQL with
persistent volumes, obtains and renews a Let's Encrypt TLS certificate
via DNS-01, configures HAProxy routing, and brings the application live
at the specified domain. All derived from the model. All deterministic
and repeatable.

**Nobody else does this.** The entire industry treats "build the app"
and "deploy the app with TLS to your domain" as separate problems solved
by separate tools:

- **Frappe** requires a full server setup — bench, nginx, certbot,
  supervisor, Redis. The "Frappe installation" is legendary for its
  complexity. Frappe Cloud is a managed service, not a deploy action in
  the framework.
- **Directus** offers Directus Cloud (managed hosting), but that is a
  separate product, not a capability of the open-source engine.
- **Supabase** hosts your backend but does not deploy your frontend or
  configure your domain.
- **PocketBase** is a single binary you run somewhere — hosting, TLS,
  and domain configuration are entirely on you.
- **Bolt.new, v0, Lovable** generate code in a browser sandbox.
  Deployment to Vercel or Netlify is a separate step; TLS and domain
  configuration are manual.
- **Hasura, Appsmith, Retool, Budibase** — none deploy a complete
  application (frontend + backend + database) to your own domain with
  TLS from a single declarative artifact.

Data UI collapses build and deploy into one model. The 30-second demo
isn't "here's a running dev server" — it's "here's a live, TLS-secured,
publicly accessible application."


### 2. Native compilation — the compiled model is executable machine code

Data UI runs on SBCL (Steel Bank Common Lisp), which compiles to native
machine code. The compiler's output — `*compiled-model*` — is not just a
data structure describing the application. It is a live, executable
artifact containing:

- The application specification (types, fields, views, relationships) — data
- The deployment configuration (domain, version, identity) — data
- The executable application logic (CRUD functions, RBAC checks,
  validation) — native machine code
- Compiled hook lambdas (validation and lifecycle hooks authored in the
  model) — native machine code

All in one structure. No interpreter. No VM. No JIT warmup. The generic
backend functions (`be-list`, `be-insert`, `be-update`, etc.) are
compiled function calls, not interpreted scripts. When a validation hook
runs, it calls a function pointer to native code that was placed in the
compiled model at compile time.

Every comparable tool runs on an interpreter or managed runtime:

- **Frappe** — Python (CPython interpreter) + MariaDB + Redis + WSGI
  server + nginx. Five moving parts.
- **Directus** — Node.js (V8 JIT). Warm-up overhead, garbage collection
  pauses.
- **Supabase** — PostgREST (Haskell) + Node.js edge functions.
- **PocketBase** — Go (compiled, but no REPL, no interactive development,
  no homoiconicity).
- **Bolt.new, v0, Lovable** — generate JavaScript/TypeScript that runs
  on V8.

Data UI's deployment footprint is a single native process plus
PostgreSQL. No Node.js runtime, no Python interpreter, no JVM, no Redis,
no separate WSGI server. The performance ceiling is higher and the
operational surface is smaller.


### 3. The model is an API for a non-human consumer

Data UI's model format is explicitly designed for AI consumption. An AI
does not write arbitrary code into a Data UI application — it selects
from a defined vocabulary (the hook registry) and fills parameters,
exactly as it fills a function call. The compiler guarantees the result
is consistent.

This is structurally different from every other approach:

- **AI code generators (Bolt.new, v0, Lovable, Replit Agent)** — the AI
  produces imperative source code. The output is not reproducible from a
  compact spec. There is no invariant enforcement. A smarter model does
  not close this gap; it just drifts more eloquently.
- **Low-code platforms (Directus, Budibase, Appsmith, Retool)** — not
  designed for AI consumption at all. Configuration is visual and
  manual, stored in databases, not in reviewable text artifacts.
- **Frappe** — DocTypes are JSON metadata (good), but custom logic
  requires Python controllers (code). No structured hook vocabulary for
  AI to select from.
- **Hasura DDN** — declarative metadata (HML) is compact and
  structured, but there is no frontend, no deployment, and no hook
  system for custom logic.
- **A2UI (Google)** — a declarative JSON protocol for AI-driven UI
  rendering. Interesting, but UI-only — no database, no API, no auth, no
  deployment.

Data UI is the only system where an AI can produce a small, structured
model and the compiler expands it into a complete, consistent,
deployed application — database, API, RBAC, frontend, TLS, domain —
with no code generation and no drift.


## Feature Comparison

| Capability                        | Data UI | Frappe | Directus | PocketBase | Hasura | Supabase | Bolt/v0 |
|-----------------------------------|---------|--------|----------|------------|--------|----------|---------|
| DB schema from model              | ✅      | ✅      | ✅¹       | ✅          | ❌²     | ✅        | ❌       |
| Auto-generated REST API           | ✅      | ✅      | ✅        | ✅          | ✅³     | ✅        | ❌       |
| Auto-generated frontend UI        | ✅      | ✅⁴     | ✅⁴       | ✅⁴         | ❌      | ❌        | ✅       |
| RBAC from model                   | ✅      | ✅      | ❌⁵       | ❌          | ❌⁵     | ❌⁶       | ❌       |
| Field-level scoping               | ✅      | ❌      | ✅        | ❌          | ✅      | ✅⁶      | ❌       |
| Deploy to TLS at model's domain   | ✅      | ❌      | ❌        | ❌          | ❌      | ❌        | ❌       |
| Native machine code               | ✅      | ❌      | ❌        | ✅⁷         | ❌      | ❌        | ❌       |
| Model fits in an email            | ✅      | ❌      | ❌        | ❌          | ✅      | ❌        | N/A     |
| AI-consumable model format        | ✅      | ❌      | ❌        | ❌          | ✅      | ❌        | N/A⁸    |
| Structured hook vocabulary        | ✅      | ❌      | ❌        | ❌          | ❌      | ❌        | ❌       |
| Interactive development (REPL)    | ✅      | ❌      | ❌        | ❌          | ❌      | ❌        | ❌       |

¹ Directus wraps an existing database; it does not compile one from a model.
² Hasura connects to an existing database.
³ GraphQL, not REST.
⁴ Admin panel only, not a custom application frontend.
⁵ Permissions must be configured separately from the data model.
⁶ Via PostgreSQL Row Level Security (manual SQL policies).
⁷ Go compiles to native code, but PocketBase has no REPL, no interactive
  development, and no homoiconicity.
⁸ These tools consume natural language prompts, not structured models.
  Output is imperative code, not a reproducible specification.


## The Closest Competitors

### Frappe Framework

The closest philosophical cousin. Frappe is metadata-driven: DocTypes
(JSON) define models, views, and permissions, and the framework
generates database tables, REST APIs, and admin UI from them. It powers
ERPNext, a full ERP system.

Where Data UI pulls ahead:

- **Deployment.** Frappe requires a multi-service server setup (Python,
  MariaDB, Redis, nginx, supervisor). Data UI deploys from the model in
  one command with TLS.
- **Native compilation.** Frappe runs on CPython (interpreted). Data UI
  compiles to native machine code, including hook lambdas.
- **Model compactness.** A Frappe application requires many DocTypes
  plus Python controllers. A Data UI model for a comparable application
  fits in a single file.
- **AI consumption.** Frappe has no structured hook vocabulary. Custom
  logic requires Python code. Data UI's registry makes the model
  AI-consumable without code generation.
- **Interactive development.** Frappe has no REPL. Data UI's Slime/REPL
  workflow enables real-time compiler development and introspection.

### Directus

The strongest RBAC offering in the market. Directus wraps any SQL
database and auto-generates REST/GraphQL APIs, an admin studio, and
granular permissions (collection-level, field-level, item-level with
dynamic filters, hierarchical role inheritance).

Where Data UI pulls ahead:

- **Model-driven, not database-first.** Directus requires you to bring
  (or manually create) a database schema. Data UI compiles the schema
  from the model.
- **Deployment.** Directus does not deploy a complete application to
  your domain with TLS.
- **Native compilation.** Directus runs on Node.js (V8 JIT). Data UI
  runs as a single native-compiled process.
- **AI consumption.** Directus permissions and flows are configured
  visually or via API, not from a compact, reviewable model artifact.
- **Frontend.** Directus provides an admin panel, not a custom
  application frontend. Data UI generates a schema-driven React frontend
  for the application itself.

### PocketBase

The best "whole app in a file" in the market. A single Go binary with
embedded SQLite, REST API, auth, admin dashboard, and file storage.

Where Data UI pulls ahead:

- **PostgreSQL.** PocketBase is SQLite-only. Data UI uses PostgreSQL
  with views, triggers, and full relational integrity.
- **RBAC from model.** PocketBase has basic auth but no model-driven
  RBAC. Data UI compiles complete role-based access control from the
  model.
- **Deployment.** PocketBase is a binary you run somewhere. Data UI
  deploys to Kubernetes with TLS at the model's domain.
- **Interactive development.** Go has no REPL. Data UI's REPL enables
  real-time development and introspection of the running system.
- **Frontend.** PocketBase provides an admin dashboard, not a custom
  application frontend.

### AI Code Generators (Bolt.new, v0, Lovable, Replit Agent)

These tools generate full-stack applications from natural language
prompts. They are impressive demos but fundamentally different from Data
UI.

Where Data UI pulls ahead:

- **Reproducibility.** AI generators produce imperative code. There is
  no compact spec to reproduce or audit. Data UI's model is the spec;
  the application is a pure function of it.
- **Invariant enforcement.** AI generators have no enforcement mechanism.
  The AI pattern-matches locally and drifts globally. Data UI's compiler
  guarantees global consistency.
- **Maintenance.** AI-generated code must be maintained like any other
  code. A Data UI application is maintained by editing the model and
  recompiling.
- **Deployment.** AI generators produce code in a sandbox. Deployment,
  TLS, and domain configuration are separate manual steps.


## The Practical Result

The three differentiators are not independent features. They compound.
A model that compiles to native code, carries its own deployment
configuration, and is designed for AI consumption means something
specific in practice:

**You can build and deploy a real application in minutes, not days.**
Starting from an existing model, a few minutes to tweak, test, and
deploy. Starting from scratch, perhaps 30 minutes. Either way, the
result is not a dev server or a sandbox preview — it is a live,
TLS-secured, publicly accessible application at its own domain, backed
by PostgreSQL, with full RBAC enforcement, a REST API, and a React
frontend, all from a single model file that fits in an email.

No comparable tool can make this claim. Consider what it takes to deploy
a Frappe application: install the bench CLI, provision a server, install
Python, MariaDB, Redis, nginx, supervisor, configure each service, run
`bench new-site`, set up DNS, obtain TLS certificates, configure nginx
vhosts, and start the supervisor processes. The setup alone is a
multi-hour exercise, and that is before you have written a single
DocType.

The gap is not incremental. It is categorical. When the model *is* the
application — schema, logic, permissions, deployment, domain — the
compiler guarantees that the deployed system is consistent with the
specification. There is no imperative code to drift, no separate
permission configuration to forget, no deployment checklist to get
wrong. The application is more robust not because of better testing or
more careful engineering, but because there is structurally less that
*can* go wrong. The distance between intent and running software
collapses to a single compile step.
