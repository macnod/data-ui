---
description: "Create an Independent-kind plan row for a backlog item in the production Data UI Backlog app (backlog.demo.data-ui.com) over its REST API, as the invoking agent's own machine account. The plan is titled '<short item name> - <agent display name>' (plan titles are globally unique), carries a searchable summary capped at 1000 characters and the full plan document as its markdown body, and is linked on the item's plans list. The item is flipped to 20 Planning with a '→ 20 Planning' journal Event unless already there (skip the flip) or past Planning (stop and ask); its contributors are ensured to include Donnie and the agent. Stops and reports on plan-title collisions instead of retitling. Use when Donnie asks to create, draft, or file an independent plan for a backlog item ('create an independent plan for X', '/create-independent-plan <item title>'). Files kind Independent only — Synthesized belongs to the synthesize flow. Project-scoped to data-ui; works from any harness (Emacs gptel, OpenCode) on evo-x2."
name: create-independent-plan
---

# Skill: create-independent-plan

File one `:plans` row of kind `Independent` for a backlog item in the production Data UI Backlog app, via REST, attributed to the invoking agent — and bring the item along: flip it to `20 Planning` (event-stamped), ensure its contributors, link the plan on its plans list. The plan document itself is the row's markdown body. The app is the backlog; the org backlog files were deleted at the November 2026 cutover (git history keeps them).

## When to Use

- Donnie asks to create / draft / file an independent plan for a backlog item — the agent researches and writes the plan, then files it as an app row.
- **Not** for: creating items (create-item), notes (create-note), state changes (change-item-state), Synthesized-kind plans (the synthesize flow, a later skill), or the legacy workbench org-file plans (create-data-ui-plan, retired at the November 2026 cutover).

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

Take `id` (the update targets it), `status`, `contributors`, and `plans` (the currently linked plan titles — step 7 sends the full set). Then read the full record and the journal for drafting context (the item's `description` is not in list results):

```bash
$B --name rose item type=items id=$ITEM_ID \
  | jq '.result.record | {title, description, "public-url"}'

# the item's journal — the same title row applied to notes
$B --name rose list type=notes filters="$F" sort=date:desc \
  | jq -c '.result.records[] | {title, kind, author, date, summary}'
```

### 3. Guard the item's state

The flip rules, by the `status` just read:

- `10 Open` → flip: the step 7 update carries `status: "20 Planning"`, and step 8 stamps the event.
- `20 Planning` → already there: **no status key, no event** — the plan, contributors, and link still land; report the skip.
- `30 Ready` / `40 Implementing` / `50 Code Complete` / `60 Done` → **stop and ask Donnie** before writing anything — an independent plan on an item past planning is his call, not the skill's.

### 4. Resolve the kind

Fetch the palette and match `Independent` exactly:

```bash
$B --name rose list type=plan-kinds | jq -c '.result.records[].name'
# => "Independent" "Synthesized"
```

`Independent` absent → stop and tell Donnie (never file `Synthesized` here, never create kinds).

### 5. Draft the plan

- **title** — the naming convention: a crafted short noun phrase naming the ask (item-title discipline applied twice — the shortest phrase that still names it), then `" - "`, then your display name. Item `Fix :eq filter on reference fields` filed by Rosie → `Eq filter fix - Rosie`. Plans identity is `(:title)` alone — a plan title exists **once in the world** — so the agent suffix doubles as the cross-agent uniqueness guard.
- **summary** — always sent. One to three sentences carrying the gist: titles and summaries are the plan search surface (descriptions are never searchable). Hard cap 1000 characters.
- **description** — the plan document itself, markdown (the form labels it "Plan"). Substantive, never blank, **no word cap** — a plan is a whole document; that is what separates it from a note. Draft it with the create-data-ui-plan craft: read the item and its journal, research the actual code, resolve decisions the item defers to the plan — then file the result here instead of a workbench org file.
- **author** — your author row. **item** — the item's exact title. **kind** — `Independent`.

### 6. Insert the plan

Plans are non-public rows: roles `["backlog-agents", "contributors"]` — never `public`.

```bash
[ "${#SUMMARY}" -le 1000 ] || { echo "summary over 1000 chars" >&2; exit 1; }
DATA=$(jq -nc --arg title "$PLAN_TITLE" --arg item "$ITEM_TITLE" \
  --arg author "Rose" --arg summary "$SUMMARY" --arg desc "$DESC" \
  '{title:$title, item:$item, kind:"Independent",
    author:$author, summary:$summary, description:$desc}')
$B --name rose insert type=plans data="$DATA" \
    roles='["backlog-agents","contributors"]' | jq '.result'
# => {"id":"<uuid>","inserted":true}
```

**`inserted` must be `true`.** `false` means a plan with this title already exists — anywhere in the world (another agent's same-named plan, or your own prior plan on this item; the unique index is on the title alone): stop, read the existing row back (the response names its id — the step 9 query with it), report its title, item, author, and dates, and ask Donnie how to proceed. Never silently retitle.

### 7. Update the item — link, flip, contributors

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

### 8. Stamp the transition event

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

**Re-entry collision:** notes identity is `(:title :item)` — `inserted:false` means the item held `20 Planning` at some earlier time; the update stands. Take the returned id and report the existing event's date in step 9 — the journal keeps the *first* entry into the state.

### 9. Verify and report

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

- Plans identity is `(:title)` alone — globally unique, once in the world (contrast notes' `(:title :item)` per-item; the production index is `ix_rt_plans_identity` on `plan_title` only). The `<short name> - <agent>` convention is the uniqueness guard across agents; re-filing on the same item collides with your own prior plan → stop, report, ask.
- Plans are non-public rows — roles `["backlog-agents", "contributors"]`, no `public-url`. Journal events (notes) are non-public rows too — the step 8 spelling.
- The only filters issued here are the items title `in` lookup and that same row applied to `type=notes` (target-keyed `:in` — the journal read). Never `["notes","item","eq",...]` or `["plans","item","eq",...]` (same-type `:eq` on a reference field 500s; backlog-api has the detail).
- `summary` is the search surface: titles and summaries are the searchable plan fields, descriptions never — nothing index-bearing on the big text columns; a plan is a whole file (the model's own design note).
- Sequential writes: plan insert → item update → event insert — never parallel.
- Reference fields submit display names; list fields submit arrays; JSON wire keys are hyphenated (the `data=` payloads).
- Agents **create** and **update** but never **delete**; this skill inserts one plan and updates one item.
- Kind `Independent` only — kinds are never created by skills; `Synthesized` belongs to the synthesize flow (a later skill).
