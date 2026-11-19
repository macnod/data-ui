---
description: "Synthesize two or more Independent plans for a backlog item into one canonical plan row in the production Data UI Backlog app (backlog.demo.data-ui.com) over its REST API, as the invoking agent's own machine account. The output is a Synthesized-kind plan titled 'Plan: <item title>' carrying a searchable summary (capped at 1000 characters) and the full merged plan document as its markdown body, linked on the item's plans list; a Note-kind journal entry records the merge. Requires at least two Independent plans on the item — fewer stops and reports. Stops and asks before writing anything when the item is past 20 Planning or already holds a Synthesized plan. The synthesizer is a contributor, not just a judge: it verifies contested facts against the actual code, resolves every conflict explicitly, and may substitute its own better path wherever both inputs are beaten. The item stays at 20 Planning — approval to 30 Ready is Donnie's. Use when Donnie asks to synthesize, merge, or combine the plans for a backlog item ('synthesize-plan <item title>'). Project-scoped to data-ui; works from any harness (Emacs gptel, OpenCode) on evo-x2."
name: synthesize-plan
---

# Skill: synthesize-plan

Read every Independent plan filed for a backlog item in the production Data UI Backlog app, then write one synthesized plan — kind `Synthesized`, titled `Plan: <item title>` — that keeps the best of each input, resolves their disagreements, and becomes the single plan the implement flow executes. The synthesizer is a contributor, not just a judge: where it sees a better path than either input takes, it goes with that path instead and records the departure. The item stays at `20 Planning` (approval to `30 Ready` is Donnie's), gains the plan on its plans list, and gains a Note-kind journal entry recording the merge. Does not implement anything. Requires **at least two** Independent plans on the item; fewer stops the skill.

## When to Use

- Donnie says `synthesize-plan <item title>`, or asks to synthesize / merge / combine the plans for a backlog item.
- **Not** for: writing a lone independent plan (create-independent-plan), promoting a lone plan with no peer (Donnie's call, never this skill's), items that already hold a Synthesized plan (stop and ask — every re-synthesis goes through Donnie), implementing (a later skill), notes on items (create-note), state changes (change-item-state), or the legacy workbench org-file flow (synthesize-data-ui-plan — the retired file-world predecessor of this skill).

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

Take `id` (the update targets it), `status`, `contributors`, and `plans` (the currently linked plan titles — step 9 sends the full set). Then read the full record and the journal for drafting context (the item's `description` is not in list results):

```bash
$B --name rose item type=items id=$ITEM_ID \
  | jq '.result.record | {title, description, "public-url"}'

# the item's journal — the same title row applied to notes
$B --name rose list type=notes filters="$F" sort=date:desc \
  | jq -c '.result.records[] | {title, kind, author, date, summary}'
```

### 3. Guard the item's state

The rules, by the `status` just read:

- `10 Open` → flip: the step 9 update carries `status: "20 Planning"`, and step 10 stamps the event. (Normally create-independent-plan already flipped the item; this is hand-loaded-data parity with the old file skill's TODO → PLANNING.)
- `20 Planning` → the normal case: **no status key, no event** — the plan, contributors, link, and journal note still land.
- `30 Ready` / `40 Implementing` / `50 Code Complete` / `60 Done` → **stop and ask Donnie** before writing anything. The old file skill demoted READY → PLANNING on re-synthesis; in the app, approval is Donnie's — a synthesis on an item past planning is his call, not the skill's.

### 4. Find the candidate plans — and guard against an existing synthesis

One query returns every plan referencing the item (target-keyed `in` — the app-world equivalent of the old `~/workbench/<slug>-plan-*.org` glob):

```bash
$B --name rose list type=plans filters="$F" \
  | jq -c '.result.records[] | {id, title, kind, author, summary}'
```

- **Candidates = the kind `Independent` rows. At least two are required.** Fewer → stop and report: the item title, what was found (title + author of each), and that the peer plan has not landed — suggest waiting on it. Never proceed with one input: no re-copying or re-verifying a lone plan under the synthesis title, and no writing a plan from the item alone (that is create-independent-plan, not this skill). Promoting a lone plan is Donnie's call.
- **Any kind `Synthesized` row on the item → stop and ask Donnie.** Read it first (it is a prior synthesis, not a peer) and report its title, author, and dates. Every re-synthesis goes through Donnie — never update, retitle, or work around an existing synthesized plan.
- Cross-check: this reference set and the item's `plans` list (step 2) should agree. A plan referencing the item but missing from its plans list (or vice versa) is a data inconsistency — report it to Donnie rather than silently healing.

### 5. Resolve the kind

Fetch the palette and match `Synthesized` exactly:

```bash
$B --name rose list type=plan-kinds | jq -c '.result.records[].name'
# => "Independent" "Synthesized"
```

`Synthesized` absent → stop and tell Donnie (never file `Independent` here, never create kinds). The journal note (step 11) resolves `Note` against `note-kinds` the same way.

### 6. Read every candidate fully — the synthesis

This skill has no blindness rule — reading the peers is the job. Fetch each candidate's full row (the `description` is the whole plan document):

```bash
$B --name rose item type=plans id=$CAND_ID \
  | jq -r '.result.record.description'
```

For each candidate, note:

- Scope and summary — identical intent, or does one drift?
- Design decisions — and their rationale/precedent.
- Steps — files, functions, call sites named; ordering; which plan is more codebase-accurate where they differ.
- Risks and notes — gotchas only one planner caught.
- Anything the item deferred that one plan resolves and the other leaves open (or resolves differently).

Verify contested facts against the actual code before deciding between them (`lisp/model.lisp`, `lisp/backend.lisp`, `lisp/plist-json.lisp`, `web/src/`, `docs/` per the repo AGENT.md "Where to Look"). A plan's confidence is not evidence; the code is. Spot-check at least the sites where the plans disagree.

**Resolve every conflict explicitly.** For each disagreement, pick a winner — the codebase-accurate one, the one following house precedent (DONE items, prior plans), or the better-reasoned call — and record the choice plus the rejected alternative in a *Design decisions* bullet. If a conflict genuinely needs Donnie (the two plans take incompatible product directions), make the best-reasoned call, flag it prominently in the reply (not buried in the plan), and note the alternative. No "TBD" survives.

**The synthesizer may beat both plans.** Where neither candidate has the best path for a step or decision — a simpler design, a missed precedent, a cleaner call site — contribute your own path and go with it, verified against the actual code with the same rigor as a contested fact. Record each as a synthesizer contribution in *Design decisions*, naming what both inputs did and why the new path wins.

**Fill any gap:** a decision, step, risk, or verification one plan has and the others lack gets merged in unless it is wrong.

### 7. Draft the synthesized plan

- **title** — `"Plan: " + the item's exact title` (e.g. item `Missing-username login 500` → `Plan: Missing-username login 500`). Globally unique by construction (item titles are unique, and plans identity is the title alone) — this is the canonical-plan convention, distinguishing the Synthesized row from the per-agent independents.
- **summary** — always sent. One to three sentences carrying the gist: titles and summaries are the plan search surface (descriptions are never searchable). Hard cap 1000 characters.
- **description** — the full synthesized plan document, markdown (the form labels it "Plan"). No word cap — and it must be **self-contained**: the implement flow reads only this row, never the independents. House structure:
  - *Scope and summary* — the item, plus the conflict outcomes.
  - *Design decisions* — one per bullet, rationale included; where plans disagreed, note the source plans and why the winner won (naming the agents is fine and useful); where the synthesis departs from every candidate, say so explicitly and why.
  - *Steps* — ordered, grouped by area (compiler / wire / frontend / fixture+tests / docs / verification; keep only the areas touched). Every step names real files, functions, call sites. Test steps name the `models/test/` fixture, the suite file, the `run-*` helper, and registration in `data-ui.asd` / `tests/helpers.lisp`. Verification ends with `npm run build` in `web/` when the frontend changes and `scripts/data-ui tests` as the final gate.
  - *Risks and notes* — the union of the planners' catches, plus any found during conflict checking.
  - *Out of scope* — the union, so nothing quietly sneaks in.
  - Prose rules: one paragraph per line, one bullet per line, no hard wrapping.
- **author** — your author row. **item** — the item's exact title. **kind** — `Synthesized`.

### 8. Insert the plan

Plans are non-public rows: roles `["backlog-agents", "contributors"]` — never `public`.

```bash
[ "${#SUMMARY}" -le 1000 ] || { echo "summary over 1000 chars" >&2; exit 1; }
DATA=$(jq -nc --arg title "$PLAN_TITLE" --arg item "$ITEM_TITLE" \
  --arg author "Rose" --arg summary "$SUMMARY" --arg desc "$DESC" \
  '{title:$title, item:$item, kind:"Synthesized",
    author:$author, summary:$summary, description:$desc}')
$B --name rose insert type=plans data="$DATA" \
    roles='["backlog-agents","contributors"]' | jq '.result'
# => {"id":"<uuid>","inserted":true}
```

**`inserted` must be `true`.** `false` means a plan with this exact title already exists — a prior synthesis under the convention title, or a legacy row that happens to hold it: stop, read the existing row back (the response names its id — the step 12 query with it), report its title, item, author, and dates, and ask Donnie how to proceed. Never silently retitle, never update it in place — re-synthesis goes through him.

### 9. Update the item — link, contributors, (flip)

One update does all of it. `plans` and `contributors` are set semantics — **send the full desired list** (the engine diffs it against the loaded record). Contributors: ensure Donnie, yourself, and every candidate's author (each create-independent-plan run already added its own author; this is idempotent and heals hand-loaded data). Omit `roles` (the record keeps them); never send `updated-at` (the DB trigger re-stamps it).

```bash
CAND_AUTHORS='["Rosie","Kimi-hs"]'   # from step 4, as a JSON array
CONTRIBS=$(jq -nc --arg d "Donnie" --arg a "Rose" \
             --argjson authors "$CAND_AUTHORS" --argjson e "$EXISTING_CONTRIBS" \
  '(($e // []) + [$d, $a] + $authors) | unique')
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

The plans link is an **item-side write**: the plan insert stored the plan's own `item` reference but created no join-table row — this update is what surfaces the plan on the item's plans list.

### 10. Stamp the transition event

Only when the flip happened (item was `10 Open`). Same discipline as change-item-state: title `→ 20 Planning`, kind `Event`, author = you, date = now, no summary/description.

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

**Re-entry collision:** notes identity is `(:title :item)` — `inserted:false` means the item held `20 Planning` at some earlier time; the update stands. Take the returned id and report the existing event's date in step 12 — the journal keeps the *first* entry into the state.

### 11. Write the journal note

The synthesis is recorded in the item's journal as a Note-kind row. Resolve the kind first: `list type=note-kinds` must contain `Note` (Event rows are transition stamps — step 10's job, not this one). Then:

- **title** — a short phrase naming the synthesis, distinct on the item (notes identity is `(:title :item)`), e.g. `Independent plans synthesized`.
- **summary** — one to three sentences, hard cap 1000 characters.
- **description** — markdown, any length (note descriptions are not sortable and carry no index, so nothing needs trimming): which independent plans were merged (titles + authors), each conflict and its winner (one line each), synthesizer-contributed paths, and anything flagged for Donnie's review.
- **author** — you. **date** — now.

```bash
DATA=$(jq -nc --arg title "$NOTE_TITLE" --arg item "$ITEM_TITLE" \
  --arg author "Rose" --arg date "$NOW" --arg summary "$NOTE_SUMMARY" \
  --arg desc "$NOTE_DESC" \
  '{title:$title, item:$item, kind:"Note",
    author:$author, date:$date,
    summary:$summary, description:$desc}')
$B --name rose insert type=notes data="$DATA" \
    roles='["backlog-agents","contributors"]' | jq '.result'
# => {"id":"<uuid>","inserted":true}
```

**`inserted` must be `true`.** `false` means a note with this title already exists on this item (a prior synthesis note): stop, read it back, report its title and date, and ask Donnie.

### 12. Verify and report

```bash
$B --name rose item type=plans id=$PLAN_ID \
  | jq '.result.record | {title, item, kind, author, summary, roles}'

$B --name rose item type=items id=$ITEM_ID \
  | jq '.result.record | {title, status, plans, contributors,
      "updated-at", "public-url"}'
```

Read the event and the note back the same way (`type=notes`, by id). Check: every plan field matches what you submitted; plan `roles` contain backlog-agents and contributors (admin and your own exclusive role are hidden from your view — expected); item `status` is `20 Planning`, its `plans` include the new title, its `contributors` include Donnie, you, and every candidate author, `updated-at` bumped; the event and the note carry the right title, kind, author, and date. Plans carry **no** `public-url` (non-public rows) — reference the plan by title; it surfaces first in the plans tab (default sort is updated-at descending).

Report to Donnie: the item title, **old → new** status (or "already `20 Planning` — no flip, no event"), the synthesized plan title with its summary, which candidates were folded in (titles + authors), each conflict and its winner (one line each), any synthesizer-contributed path that beat all inputs (flag these clearly — they are new, not merged), anything flagged for review, the synthesis note by title, and the item's `public-url`.

No transactions — sequential writes, stop on the first failure, and report exactly what landed. A failed item update leaves the plan real (findable in the plans tab, attached via its own item reference) but unlinked, the item unflipped and contributors un-ensured — complete by hand or re-run the update. A failed event insert leaves the flip and link standing. A failed note insert leaves the plan, link, and flip standing — only the journal record is missing; file it by hand (create-note) or re-run.

## Inputs

- **Item title** (required) — the exact title of the item whose Independent plans are being synthesized.

## Notes

- Plans identity is `(:title)` alone — globally unique, once in the world (`ix_rt_plans_identity` on `plan_title`). `Plan: <item title>` is unique by construction. The candidates are the item's kind-Independent rows (the per-agent plans, `create-independent-plan`'s output); the output is the single kind-Synthesized row — the app-world mapping of the old `~/workbench/<slug>-plan-<agent>.org` files and `<slug>-plan.org`.
- At least two candidates are required — a synthesis cross-checks independent plans against each other; a lone plan has no peer to catch its mistakes. Promoting a lone plan is Donnie's call.
- The item stays at `20 Planning` — plan completion is not approval. Flipping to `30 Ready` is Donnie's (change-item-state).
- An existing Synthesized plan on the item stops the skill — every re-synthesis goes through Donnie. The same goes for insert collisions on the title.
- The only filters issued here are the items title `in` lookup, that same row applied to `type=plans` (the candidate read), and to `type=notes` (the journal read). Never `["plans","item","eq",...]` or `["notes","item","eq",...]` (same-type `:eq` on a reference field 500s; backlog-api has the detail).
- The plans link is an item-side write: the plan insert stores the plan's own `item` reference but creates no `rt_item_plans` joiner row (the engine writes joiners only from the type owning the list field) — the step 9 update surfaces the plan on the item.
- The journal gains at most two rows: the transition Event (only when the item was flipped from `10 Open`) and the synthesis Note. The file-world `::COMPLETE::` final-line marker is retired here — an app row is atomic, born complete (seeded legacy descriptions carry it as file residue).
- `summary` is the search surface: titles and summaries are the searchable plan fields, descriptions never.
- Sequential writes: plan insert → item update → event → note — never parallel.
- Reference fields submit display names; list fields submit arrays; JSON wire keys are hyphenated (the `data=` payloads).
- Agents **create** and **update** but never **delete**; this skill inserts one plan, at most one event, one note, and updates one item.
