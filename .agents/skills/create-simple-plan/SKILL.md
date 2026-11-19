---
description: "Create a Simple-kind plan row for a backlog item in the production Data UI Backlog app (backlog.demo.data-ui.com) over its REST API, as the invoking agent's own machine account. A Simple plan is the item's single direct, single-author canonical plan — for items that don't warrant the two-agent independent/synthesis pipeline — titled 'Plan: <item title>' (the canonical-plan convention shared with Synthesized; plan titles are globally unique), carrying a searchable summary capped at 1000 characters and the full plan document as its markdown body, linked on the item's plans list. The item is flipped to 20 Planning with a '→ 20 Planning' journal Event unless already there (skip the flip) or past Planning (stop and ask); its contributors are ensured to include Donnie and the agent. Stops and reports when the item already holds any plan (Independent, Simple, or Synthesized) or on a plan-title collision, instead of retitling or working around. Use when Donnie asks one agent to plan an item directly ('plan X', 'write a simple plan for X', '/create-simple-plan <item title>'). Files kind Simple only — Independent belongs to the pipeline, Synthesized to the synthesize flow. Project-scoped to data-ui; works from any harness (Emacs gptel, OpenCode) on evo-x2."
name: create-simple-plan
---

# Skill: create-simple-plan

File one `:plans` row of kind `Simple` for a backlog item in the production Data UI Backlog app, via REST, attributed to the invoking agent — and bring the item along: flip it to `20 Planning` (event-stamped), ensure its contributors, link the plan on its plans list. A Simple plan is the item's canonical plan, written directly by one agent — the app-world successor of the old create-data-ui-plan file flow — for items that don't warrant the two-agent pipeline. It is held to the synthesized plan's bar: **self-contained**, because the implement flow reads only this row. The plan document itself is the row's markdown body. The app is the backlog; the org backlog files were deleted at the November 2026 cutover (git history keeps them).

## When to Use

- Donnie asks **one agent** to plan an item directly — "plan X", "write a simple plan for X", "just plan it" — or invokes the skill by name.
- **Not** for: independent plans (pipeline inputs meant for synthesis — create-independent-plan; the tell is the ensemble: two agents planning in parallel), Synthesized plans (synthesize-plan), creating items (create-item), notes (create-note), state changes (change-item-state), or the legacy workbench org-file plans (create-data-ui-plan, retired at the November 2026 cutover).

## Instructions

### 1. Access

All API calls go through the `backlog` wrapper — it resolves your identity, logs in your machine account, and attaches the Bearer token itself, failing loudly with its own message on unresolved identity or failed login; treat either as stop-and-ask-Donnie (never act under another agent's identity). Identity flags: `--name <agent stem>` (Emacs/gptel agents) or `--provider P --model M` (OpenCode). Verb reference and app gotchas live in the `backlog-api` skill; output is the raw JSON envelope — pipe through `jq`.

### 2. Find the item and read its context

Items are addressed by exact title (the unique identity column; the title `in` filter is `:eq`-bug-safe). No match → tell Donnie and ask; never guess or fuzzy-match.

```bash
B=~/r/elisp/ai/agents/backlog
F=$(jq -nc --arg t "$ITEM_TITLE" '[["items","title","in",[$t]]]')
$B --name rose list type=items filters="$F" limit=1 \
  | jq '.result.records[0] | {id, title, status, contributors, plans}'
```

Take `id` (the update targets it), `status`, `contributors`, and `plans` (the currently linked plan titles — step 8 sends the full set). Then read the full record and the journal for drafting context (the item's `description` is not in list results):

```bash
$B --name rose item type=items id=$ITEM_ID \
  | jq '.result.record | {title, description, "public-url"}'

# the item's journal — the same title row applied to notes
$B --name rose list type=notes filters="$F" sort=date:desc \
  | jq -c '.result.records[] | {title, kind, author, date, summary}'
```

### 3. Guard the item's state

The flip rules, by the `status` just read:

- `10 Open` → flip: the step 8 update carries `status: "20 Planning"`, and step 9 stamps the event.
- `20 Planning` → already there: **no status key, no event** — the plan, contributors, and link still land; report the skip.
- `30 Ready` / `40 Implementing` / `50 Code Complete` / `60 Done` → **stop and ask Donnie** before writing anything — a plan on an item past planning is his call, not the skill's.

### 4. Guard against existing plans

A Simple plan is normally the only plan an item will ever hold — check before drafting. One query returns every plan referencing the item (target-keyed `in`):

```bash
$B --name rose list type=plans filters="$F" \
  | jq -c '.result.records[] | {id, title, kind, author}'
```

**Any row at all → stop and ask Donnie.** Report each plan's title, kind, and author, and what it means:

- kind `Simple` or `Synthesized` → the item already holds a canonical plan; re-planning is Donnie's call.
- kind `Independent` → the two-agent pipeline already started on this item; either run the synthesis or explicitly direct the simple plan — his call, not the skill's.

Cross-check against the item's `plans` list from step 2: the two views should agree (the list links what references the item). A plan referencing the item but missing from its list — or a non-empty list with an empty query result — is a data inconsistency; report it, don't heal it. A clean zero on both sides → proceed.

### 5. Resolve the kind

Fetch the palette and match `Simple` exactly:

```bash
$B --name rose list type=plan-kinds | jq -c '.result.records[].name'
# => "Independent" "Simple" "Synthesized"
```

`Simple` absent → stop and tell Donnie (never file `Independent` or `Synthesized` here, never create kinds).

### 6. Draft the plan

- **title** — `"Plan: " + the item's exact title` (e.g. item `Missing-username login 500` → `Plan: Missing-username login 500`). The canonical-plan convention, shared with synthesize-plan: an item holds at most one canonical plan, whatever its kind — findable by title alone, the kind recording which path produced it (Simple = direct single-author, Synthesized = merged from independents). Globally unique by construction: item titles are unique, and plans identity is the title alone.
- **summary** — always sent. One to three sentences carrying the gist: titles and summaries are the plan search surface (descriptions are never searchable). Hard cap 1000 characters.
- **description** — the full plan document, markdown (the form labels it "Plan"). Substantive, never blank, **no word cap** — and **self-contained**: the implement flow reads only this row, so no "see the independents", no references to conversation context. Draft it with the create-data-ui-plan craft: read the item row and its journal, research the actual code (`lisp/model.lisp` validators and registries, the fe-fields wire path, `web/src/` rendering sites, sibling DONE-item precedents), resolve decisions the item defers to the plan. House structure:
  - *Scope and summary* — the item, the ask, the shape of the change.
  - *Design decisions* — one per bullet, rationale included.
  - *Steps* — ordered, grouped by area (compiler / wire / frontend / fixture+tests / docs; keep only the areas touched). Every step names real files, functions, call sites. Test steps name the `models/test/` fixture, the suite file, the `run-*` helper, and registration in `data-ui.asd` / `tests/helpers.lisp`. Verification ends with `npm run build` in `web/` when the frontend changes and `scripts/data-ui tests` as the final gate.
  - *Risks and notes* — gotchas, invariants, hazards.
  - *Out of scope* — what the plan deliberately does not touch.
  - Prose rules: one paragraph per line, one bullet per line, no hard wrapping.
- **author** — your author row. **item** — the item's exact title. **kind** — `Simple`.

### 7. Insert the plan

Plans are non-public rows: roles `["backlog-agents", "contributors"]` — never `public`.

```bash
[ "${#SUMMARY}" -le 1000 ] || { echo "summary over 1000 chars" >&2; exit 1; }
DATA=$(jq -nc --arg title "$PLAN_TITLE" --arg item "$ITEM_TITLE" \
  --arg author "Rose" --arg summary "$SUMMARY" --arg desc "$DESC" \
  '{title:$title, item:$item, kind:"Simple",
    author:$author, summary:$summary, description:$desc}')
$B --name rose insert type=plans data="$DATA" \
    roles='["backlog-agents","contributors"]' | jq '.result'
# => {"id":"<uuid>","inserted":true}
```

**`inserted` must be `true`.** `false` means a plan with this exact title already exists — anywhere in the world (the unique index is on the title alone). After a clean step 4, that means the title `Plan: <item>` is held by a plan referencing a *different* item (most likely retitle residue: the item was renamed after its plan was filed) or landed concurrently: stop, read the existing row back (the response names its id — the step 10 query with it), report its title, item, author, and dates, and ask Donnie how to proceed. Never silently retitle, never work around it.

### 8. Update the item — link, flip, contributors

One update does all three. `plans` and `contributors` are set semantics — **send the full desired list** (the engine diffs it against the loaded record). Contributors: ensure both Donnie and yourself, whatever the flip decision. Omit `roles` (the record keeps them); never send `updated-at` (the DB trigger re-stamps it).

```bash
CONTRIBS=$(jq -nc --arg d "Donnie" --arg a "Rose" \
             --argjson e "$EXISTING_CONTRIBS" \
  '(($e // []) + [$d, $a]) | unique')
PLANS=$(jq -nc --arg p "$PLAN_TITLE" --argjson e "$EXISTING_PLANS" \
  '(($e // []) + [$p]) | unique')

# flipping (item was 10 Open)
DATA=$(jq -nc --arg s "20 Planning" --argjson c "$CONTRIBS" \
         --argjson p "$PLANS" '{status:$s, contributors:$c, plans:$p}')

# already 20 Planning — no status key
DATA=$(jq -nc --argjson c "$CONTRIBS" --argjson p "$PLANS" \
  '{contributors:$c, plans:$p}')

$B --name rose update type=items filters=$ITEM_ID data="$DATA" | jq .
# => {"status":"success","result":"<uuid>"} — result must equal $ITEM_ID
```

The plans link is an **item-side write**: the plan insert stored the plan's own `item` reference but created no join-table row (the engine writes `rt_item_plans` only from the item's own list field) — this update is what surfaces the plan on the item's plans list.

### 9. Stamp the transition event

Only when the flip happened (item was `10 Open`). Same discipline as change-item-state: title `→ 20 Planning`, kind `Event`, author = you, date = now, no summary/description. The event's `item` reference resolves by title.

```bash
NOW=$(date +%Y-%m-%dT%H:%M:%S)
DATA=$(jq -nc --arg title "→ 20 Planning" --arg item "$ITEM_TITLE" \
  --arg author "Rose" --arg date "$NOW" \
  '{title:$title, item:$item, kind:"Event",
    author:$author, date:$date}')
$B --name rose insert type=notes data="$DATA" \
    roles='["backlog-agents","contributors"]' | jq '.result'
# => {"id":"<uuid>","inserted":true}
```

**Re-entry collision:** notes identity is `(:title :item)` — `inserted:false` means the item held `20 Planning` at some earlier time; the update stands. Take the returned id and report the existing event's date in step 10 — the journal keeps the *first* entry into the state.

### 10. Verify and report

```bash
$B --name rose item type=plans id=$PLAN_ID \
  | jq '.result.record | {title, item, kind, author, summary, roles}'

$B --name rose item type=items id=$ITEM_ID \
  | jq '.result.record | {title, status, plans, contributors,
      "updated-at", "public-url"}'
```

Check: every plan field matches what you submitted; plan `roles` contain backlog-agents and contributors (admin and your own exclusive role are hidden from your view — expected); item `status` is `20 Planning` (when flipped), its `plans` include the new title, its `contributors` include Donnie and you, `updated-at` bumped. Plans carry **no** `public-url` (non-public rows) — reference the plan by title; it surfaces first in the plans tab (default sort is updated-at descending). When the event was stamped, read it back the same way (`type=notes`) and check its title, kind, author, and date.

Report to Donnie: the item title, **old → new** status (or "already `20 Planning` — no flip, no event"), the plan title with its summary, the contributors ensured, the event referenced by title (on re-entry, note the stale prior event's date), and the item's `public-url`. No transactions — if a later write failed, say exactly what landed: a failed item update leaves the plan real (findable in the plans tab, attached via its own item reference) but unlinked and the item unflipped — complete by hand or re-run the update; a failed event insert leaves the flip and link standing.

## Inputs

- **Item title** (required) — the exact title of the item the plan is for.
- **Plan content** (optional) — material Donnie supplies wins; otherwise drafted from session context plus research: the item row, its journal, and the codebase.

## Notes

- Plans identity is `(:title)` alone — globally unique, once in the world (contrast notes' `(:title :item)` per-item; the production index is `ix_rt_plans_identity` on `plan_title` only). `Plan: <item title>` is the canonical-plan convention shared with synthesize-plan: the item's one canonical plan is findable by title alone, the kind recording the path. An item can therefore never hold both a Simple and a Synthesized plan — the second collides at insert → stop → Donnie's call.
- The convention keys on the item's title at filing time: a later item retitle orphans the plan's conventional title (the link and canonical status survive; only the name drifts). Applies to Synthesized plans equally.
- The step 4 guard: **any** existing plan stops the skill — a simple plan is for unplanned items; mid-pipeline items (Independents) or already-canonical items (Simple/Synthesized) are Donnie's call.
- No blindness rule — a Simple plan makes no independence claim to protect. Normally moot (step 4 stops when plans exist); if Donnie explicitly directs a simple plan onto an item holding Independents, reading them as context is allowed.
- The only filters issued here are the items title `in` lookup, that same row applied to `type=notes` (the journal read), and to `type=plans` (the step 4 guard). Never `["plans","item","eq",...]` or `["notes","item","eq",...]` (same-type `:eq` on a reference field 500s; backlog-api has the detail).
- `summary` is the search surface: titles and summaries are the searchable plan fields, descriptions never — nothing index-bearing on the big text columns.
- Sequential writes: plan insert → item update → event insert — never parallel.
- Reference fields submit display names; list fields submit arrays; JSON wire keys are hyphenated (the `data=` payloads).
- Agents **create** and **update** but never **delete**; this skill inserts one plan, at most one event, and updates one item.
- The file-world `::COMPLETE::` final-line marker is retired — an app row is atomic, born complete.
