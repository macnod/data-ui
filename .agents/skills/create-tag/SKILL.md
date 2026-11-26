---
description: "Create a new tag row in the production Data UI Backlog app (backlog.demo.data-ui.com) over its REST API, as the invoking agent's own machine account, always carrying the shared roles public, backlog-agents, and contributors — like every other tag — so all agents and the public can see it. A tag missing those roles is invisible to other agents and breaks their :join-items-exist validation (the 2026-11-24 concurrency-tag incident: updates to items carrying such a tag fail with 'item tags not found'). Use when Donnie asks to create, add, or file a tag ('create a perf tag', 'add a tag for X', '/create-tag <name> [description]'). Stops and reports on name collisions — a tag exists once per name, forever; never retitles. Project-scoped to data-ui; works from any harness (Emacs gptel, OpenCode) on evo-x2."
name: create-tag
---

# Skill: create-tag

File one `:tags` row in the production Data UI Backlog app, via REST,
attributed to the invoking agent. Tags are the shared vocabulary of item
tagging — reference rows, no journal, no state — and their whole value is
that everyone can see them.

## When to Use

- Donnie asks to create / add / file a tag — new vocabulary for tagging
  items.
- **Not** for: applying an existing tag to an item (that is an items
  update over the `backlog-api` skill — this skill only creates the tag
  row), or creating states, authors, note kinds, or any other reference
  row (ask before improvising those).

## Why the roles are the contract

The 2026-11-24 incident: a tag was created with only the creator's
exclusive role attached. Every other agent was scoped out of reading it,
so it vanished from their tags palette — and `:join-items-exist`
validation, which resolves tag names as the calling user, then rejected
every update touching an item carrying it ("Field validation failed:
item tags not found: concurrency"). A tag invisible to other agents is a
broken tag. Every tag this skill creates carries
`["public", "backlog-agents", "contributors"]`; the creator's own
exclusive role rides along automatically. Never omit or shrink the roles
list, even when the invocation says nothing about roles.

## Instructions

### 1. Access

All API calls go through the `backlog` wrapper — it resolves your
identity, logs in your machine account, and attaches the Bearer token
itself, failing loudly with its own message on unresolved identity or
failed login; treat either as stop-and-ask-Donnie (never act under
another agent's identity). Identity flags: `--name <agent stem>`
(Emacs/gptel agents) or `--provider P --model M` (OpenCode). Verb
reference and app gotchas live in the `backlog-api` skill; output is the
raw JSON envelope — pipe through `jq`.

### 2. Check the palette

Tag names are globally unique — one tag per name, forever. Look at what
already exists (this also calibrates the description style):

```bash
B=~/r/elisp/ai/agents/backlog
$B --name rose list type=tags | jq -c '.result.records[].name'
```

If the name is already visible there, stop and tell Donnie the tag
exists — never retitle, never file a near-synonym (two tags for one
concept is palette rot).

Know the palette's blind spot: a tag scoped out of your view does not
appear here, but it still owns the name. The insert's duplicate check
(step 4) is the real authority.

### 3. Draft the row

- **name** — lowercase letters, digits, and dashes, short (palette
  convention: `a11y`, `backend`, `mig`, `valdn`). It is the identity
  column; the row is the name.
- **description** — a one-liner naming the tag's territory, palette
  style ("Accessibility.", "Backend / engine work.", "Shared state,
  locking, and multi-thread access to the database layer."). Draft it
  from the invocation or session context; when nothing natural exists,
  omit the field (it is optional) and say so in the report — never pad.

### 4. Insert the tag

Roles are named on every insert — the API pre-selects nothing. Tags are
public rows and the shared-roles contract applies:

```bash
DATA=$(jq -nc --arg name "$NAME" --arg desc "$DESC" \
  '{name:$name, description:$desc}')
$B --name rose insert type=tags data="$DATA" \
    roles='["public","backlog-agents","contributors"]' | jq '.result'
# => {"id":"<uuid>","inserted":true}
```

With no description, build the payload without it:
`jq -nc --arg name "$NAME" '{name:$name}'`.

**`inserted` must be `true`.** `false` means the name is taken — the
response names the existing row's id; read it back:

```bash
$B --name rose item type=tags id=$EXISTING_ID \
  | jq '.result.record | {name, description, roles}'
```

Readable → report what exists and stop. Unreadable or erroring → the row
exists but is scoped out of your view — the incident pattern; give
Donnie the id (he can see everything) instead of guessing. Never work
around a collision by retitling or recreating.

### 5. Verify and report

```bash
$B --name rose item type=tags id=$TAG_ID \
  | jq '.result.record | {name, description, roles, "public-url"}'
```

Check: name and description match what you submitted; `roles` shows
public, backlog-agents, and contributors (your own exclusive role is
hidden from your view — expected; others' may show); `public-url` is
present. If the shared roles are missing, the row is the incident reborn
— fix it with an update (backlog-api: the roles list is an update field)
before reporting.

Report to Donnie: the tag name, its description (or that it has none),
and the public-url. When the real ask was "tag item X with it", say that
applying it is an items update (tags list field, display-name array)
ready to run over backlog-api — do not run it unasked.

## Inputs

- **Tag name** (required) — from the invocation; palette-style when you
  must craft it.
- **Description** (optional) — from the invocation or session context;
  omit rather than pad.

## Notes

- Tags identity is `(:name)` — the duplicate check rides the insert
  response (`inserted`); the palette scan is style calibration and a
  visible-collision courtesy, not the authority.
- No journal event: tags are reference rows, not items — nothing to
  stamp. `created-at` / `updated-at` are read-only; never send them.
- Reference fields submit display names; list fields submit arrays; JSON
  wire keys are hyphenated (`data=` payload).
- Agents **create** and **update** but never **delete**; this skill only
  inserts.
- A tag created by another agent may be invisible to you (roles scoped
  out) — suspect permissions, not absence, before concluding a tag does
  not exist.
