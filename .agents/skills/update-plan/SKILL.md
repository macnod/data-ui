---
description: "Updater's pass of the plan ping-pong loop for the production Data UI Backlog app (backlog.demo.data-ui.com), as the invoking agent's own machine account: fold feedback into the item's canonical plan (kind Simple or Synthesized) — verify [error] findings against the actual code before accepting, own judgment on improvements and taste with a one-line reason for every rejection, flag product/scope disagreements for Donnie — then update the plan row (description replaced wholesale, summary refreshed when the substance changes) and file the disposition as an Update-kind journal note titled 'Update disposition — N' (accepted → what changed, rejected → why, flagged → for Donnie). Feedback is Donnie's in-session paste or defaults to the item's latest Review note; stops when the latest loop note is already a disposition (that review is already folded) or when the findings are already reflected in the plan text. A no-change run (everything rejected or flagged) skips the plan write but still files its disposition. Never touches the item's state — the loop runs at 20 Planning; approval to 30 Ready is Donnie's. Use when Donnie asks to fold feedback in, apply a review, or update a plan ('fold the review into X', 'apply that feedback to the plan', '/update-plan <item title> <feedback>'). Project-scoped to data-ui; works from any harness (Emacs gptel, OpenCode) on evo-x2."
name: update-plan
---

# Skill: update-plan

Updater's pass of the plan ping-pong loop (the reviewer's half is review-plan). Reads the feedback — Donnie's in-session paste or the item's latest `Review`-kind journal note — verifies each finding against the actual code, folds the accepted ones into the canonical plan row, and files the disposition as an `Update`-kind journal note: the durable record of the pass and the next review-plan run's input. The updater holds the tiebreak on implementation judgment — that stamp is what prevents attrition — but not on facts (verify, do not stamp) and not on product direction (flag, do not resolve). Edits only the plan row (plus the contributors ensure on the item); never the item's state, never the repo — verification reads the data-ui code but edits nothing there. The loop advances only by Donnie's invocations — this skill never chains. App-world port of the old file skill data-ui-update-pass (retired at the November 2026 cutover).

## When to Use

- Donnie asks to fold feedback into a plan — "fold the review into X", "apply that feedback to the plan", "update the plan per the review", "/update-plan <item title> <feedback>".
- Requires a canonical plan (kind `Simple` or `Synthesized`) attached to the item — the same resolution as review-plan and implement-plan. Independents only → stops (suggesting synthesize-plan when there are two or more); no plans at all → stops and reports.
- Feedback arrives in-session (Donnie pastes or names it) or defaults to the item's latest `Review` note; without either there is nothing to fold — stop and ask.
- **Not** for: reviewing (review-plan — this skill never files findings), implementing (implement-plan), editing an Independent plan (a pipeline input, not a canonical plan), or any project other than data-ui. If the session's AGENT.md in context is not data-ui's, stop.

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

Take `id`, `status`, `contributors`, and `plans` (step 3 cross-checks them). Then read the full record — the `description` is the ask, ground truth for judging scope:

```bash
$B --name rose item type=items id=$ITEM_ID \
  | jq '.result.record | {title, description, "public-url"}'
```

### 3. Resolve the plan source

One query returns every plan referencing the item (target-keyed `in`):

```bash
$B --name rose list type=plans filters="$F" \
  | jq -c '.result.records[] | {id, title, kind, author, "updated-at"}'
```

Resolve in this order — the canonical plan is the edit target:

- **Exactly one canonical plan** — kind `Simple` or `Synthesized`, keyed on kind (the same resolution as review-plan and implement-plan). Independents beside it are context, not edit targets. If the plan document declares itself superseded or withdrawn → stop and ask Donnie.
- **Two or more canonical plans** → stop and ask — ambiguous, and cannot happen through the family skills; report both rows.
- **Independents only, two or more** → stop and ask, suggesting synthesize-plan — the canonical plan is one synthesis away.
- **Exactly one Independent** → stop and ask — an Independent is a pipeline input, not a canonical plan.
- **No plans at all** → stop and report — there is nothing to update.
- Cross-check against the item's `plans` list from step 2: the two views should agree. A plan referencing the item but missing from its list — or a non-empty list with an empty query result — is a data inconsistency; report it, don't heal it.

Read the canonical plan in full — the `description` is the plan document being edited; note its `summary` (refreshed in step 7 when the fold changes substance) and `updated-at` (the fold bumps it; plans sort by it):

```bash
$B --name rose item type=plans id=$PLAN_ID \
  | jq '.result.record | {title, kind, author, summary, description,
      "updated-at"}'
```

### 4. Read the loop state

The item's journal decides what there is to fold. One fetch (the same title row applied to `type=notes`):

```bash
$B --name rose list type=notes filters="$F" sort=date:desc limit=100 \
  | jq -c '{latest_loop: [.result.records[]
                | select(.kind=="Review" or .kind=="Update")][0],
            update_count: ([.result.records[]
                | select(.kind=="Update")] | length),
            updates: [.result.records[] | select(.kind=="Update")
                      | {title, summary}],
            details: [.result.records[] | select(.title=="Details")][0]}'
```

- **Loop notes** are kind `Review` or `Update`; `latest_loop` is the first in date-desc order — the one signal both loop skills read.
- **Disposition counter** ($N): `update_count` + 1 — the new note is `Update disposition — $N`.
- **Prior dispositions** (`updates`): their summaries are context — a finding an earlier disposition rejected, re-raised without new evidence, deserves a hard look before acceptance (the finiteness rule read from the updater's side).
- **Details note**: when present (create-item's overflow valve), fetch its `description` too — it carries the item's full original context and is part of ground truth for the ask.

### 5. Verify the note-kinds palette

```bash
$B --name rose list type=note-kinds | jq -c '.result.records[].name'
# => "Event" "Note" "Review" "Update"
```

`Update` absent → stop and tell Donnie — never file under another kind, never create kinds (kind creation is admin's; the RBAC already keeps machine accounts out).

### 6. Gather the feedback

**Explicit in-session feedback wins**: Donnie pasted the findings, named them, or pointed at a review from outside the journal. That is the feedback; say so in the report. It may not arrive tagged `[error]`/`[improvement]`/`[taste]` — the step 7 discipline applies to untagged feedback anyway.

Otherwise the **default**: the item's latest `Review` note — valid only when it is also the latest loop note (a `Review` as latest loop note is an unfolded review; an `Update` means the fold already happened). Since `latest_loop` is a `Review` here, that note is it — fetch its findings via the `item` verb:

```bash
$B --name rose item type=notes id=$REVIEW_ID \
  | jq '.result.record | {title, kind, author, date, description}'
```

Guards:

- **Latest loop note is an `Update` → stop, name it, ask** — that review has already been folded; the disposition note's title and date identify the pass. Explicit feedback overrides this guard.
- **No loop notes and no explicit feedback → stop and report**: nothing to fold.
- **A default feedback whose findings close with `nothing substantive`** (the loop's closed signal) → stop and report: the loop is closed, nothing to fold. `[taste]` findings may remain — they are advisory; fold them only on Donnie's explicit instruction.
- **Backstop — before folding, compare the findings against the current plan text.** Every finding already reflected (a prior fold landed but its disposition note never did, or another agent folded without filing) → stop instead of editing, and offer to write the missing disposition — if Donnie says yes, skip step 8a (the folds are in) and write the disposition under 8b describing the landed changes as accepted. Partially reflected findings fold normally; the disposition notes what was already present.

Announce the feedback source in the report (journal note or in-session paste). When explicit feedback is folded while an unfolded `Review` note is the latest loop note, add a line to the disposition noting it remains un-folded (the next fold's default).

### 7. Verify the findings and fold

**Ground truth for facts:** the actual code — the repo at `~/common-lisp/data-ui` (`lisp/`, `web/`, `docs/`); consult the area docs AGENT.md names (model-reference.md for model vocabulary, hook-registry.md for hook contracts, rest.md for the API grammar) when judging claims about them. **Ground truth for the ask:** the item row and its journal — the Details note above all.

Classify each finding before touching the plan:

- **A factual claim** — about a file, function, call site, wire key, behavior — is `[error]`-class whatever its tag: verify it against the actual code yourself. True → accept and fix the plan. False → reject, and the evidence (file, function) is the rejection reason. The updater is not a rubber stamp for the reviewer.
- **`[improvement]`, `[taste]`, and untagged non-factual points** are the updater's call — the stamp lives here. Accept what genuinely improves the plan; reject the rest with a one-line reason. No unexplained rejections, no blanket acceptance.
- **Anything about what the item *should be*** — scope, product direction, decisions the item leaves open — is neither side's to win: flag it for Donnie and leave the plan on its current path, noting the open question where relevant.

Every finding gets exactly one disposition — accepted (folded), rejected (one-line reason), or flagged (for Donnie). Nothing is silently dropped.

**Compose the revised plan document** — fold the accepted changes coherently into the plan's structure, not a diff appended to it; settled sections stay put; keep the house prose rules (one paragraph or bullet per line, no hard wrapping). Plans are versionless: the `description` is replaced wholesale, and the Review/Update notes are the only durable record of how the plan got its shape. **Refresh the `summary`** when the fold changes what the plan does (≤1000 chars — titles and summaries are the search surface; a stale summary after a substantive fold is a lie to search). A fold that corrects without changing substance may leave the summary as read in step 3.

**Compose the disposition** — the next review pass's input. Opens with the thread line (`Responds to: Review findings — M` on the default path, `Responds to: Donnie's in-session feedback` on a paste), then the list, one line per finding, only non-empty sections:

```
Disposition list:
- accepted: <finding> → <what changed>
- rejected: <finding> → <one-line reason>
- flagged: <finding> → <why it needs Donnie>
```

### 8. Write — plan row first, then the disposition note

Sequential, never parallel: the disposition must never claim changes that didn't land.

**8a. Update the plan row** — only when at least one finding was accepted:

```bash
DATA=$(jq -nc --arg desc "$NEW_PLAN_DOC" --arg sum "$NEW_SUMMARY" \
  '{description:$desc, summary:$sum}')
$B --name rose update type=plans filters=$PLAN_ID data="$DATA" | jq .
# => {"status":"success","result":"<uuid>"} — result must equal $PLAN_ID
```

Partial data only — just the fields being changed. Never `title` (canonical identity), never `kind` or `author` (the plan's provenance), never `roles`, never `updated-at` (the DB trigger re-stamps it). A failure here stops the run: report and do not write the note — a disposition claiming changes that didn't land is the one thing worse than no disposition.

**8b. Insert the disposition note** — always, even the no-change run (all rejected or flagged: the plan row is untouched, and the disposition says so):

- **title** — `Update disposition — $N` (the counter from step 4; notes identity is `(:title :item)`, so each round's title is unique on the item).
- **summary** — always sent, ≤1000 chars (the searchable surface): counts + verdict, e.g. "Folded Review findings — 2 into Plan: X — 3 accepted, 1 rejected, 1 flagged for Donnie" or "No changes folded — 2 rejected, 1 flagged; plan row untouched".
- **description** — the disposition from step 7, any length (not sortable, no index).

```bash
[ "${#SUMMARY}" -le 1000 ] || { echo "summary over 1000 chars" >&2; exit 1; }
NOW=$(date +%Y-%m-%dT%H:%M:%S)
DATA=$(jq -nc --arg title "Update disposition — $N" --arg item "$ITEM_TITLE" \
  --arg author "Rose" --arg date "$NOW" \
  --arg summary "$SUMMARY" --arg desc "$DISPOSITION" \
  '{title:$title, item:$item, kind:"Update",
    author:$author, date:$date,
    summary:$summary, description:$desc}')
$B --name rose insert type=notes data="$DATA" \
    roles='["public","backlog-agents","contributors"]' | jq '.result'
# => {"id":"<uuid>","inserted":true}
```

**`inserted` must be `true`.** `false` means a note with this title already exists on the item — a hand-filed `Update disposition — N`, or a counter miscount (a journal longer than the step 4 fetch): stop, read the existing row back (its id rides the response), report, and ask. Never retitle past it.

### 9. Ensure contributors, verify, and report

Ensure both Donnie and yourself on the item's `contributors` — set semantics, send the full desired list (append-only; never remove anyone). Skip the update entirely when both are already present (a no-op update needlessly re-stamps `updated-at`):

```bash
CONTRIBS=$(jq -nc --arg d "Donnie" --arg a "Rose" \
             --argjson e "$EXISTING_CONTRIBS" \
  '(($e // []) + [$d, $a]) | unique')
DATA=$(jq -nc --argjson c "$CONTRIBS" '{contributors:$c}')
$B --name rose update type=items filters=$ITEM_ID data="$DATA" | jq .
# => {"status":"success","result":"<uuid>"} — result must equal $ITEM_ID
```

Verify both writes:

```bash
$B --name rose item type=notes id=$NOTE_ID \
  | jq '.result.record | {title, item, kind, author, date, summary,
      "public-url"}'

$B --name rose item type=plans id=$PLAN_ID \
  | jq '.result.record | {title, kind, author, summary, "updated-at"}'
```

Check: every note field matches what you submitted; `public-url` present; the note appears in the item's journal (the child region under the item's view-form, newest first). On the plan row: the description carries the folds (spot-check one accepted change is present) and `updated-at` bumped — plans sort by it, so the updated plan surfaces first in the plans tab. Plans carry no `public-url` (non-public rows) — reference the plan by title.

Report to Donnie: the item title and its state; the feedback source and round (which review was folded, which disposition number this is); **the disposition itself** — he reads it in the report, the note is the durable copy; what landed (plan row updated or untouched, note filed); the note's `public-url` and the item's `public-url`; the contributors addition if any. Sequential writes, no transactions — say exactly what landed and what didn't: a failed plan update leaves the note unwritten (by design); a failed note insert after a landed plan update leaves the fold in place with no disposition — the disposition lives only in this report, and a re-run with the same feedback hits the step 6 backstop and offers to write the missing note.

## Inputs

- **Item title** (required) — the exact title of the item whose plan is updated.
- **Feedback** (conditionally required) — the findings to fold: Donnie's in-session paste, or the journal's latest `Review` note by default. Required in-session when the journal offers no unfolded Review — else there is nothing to fold. Explicit feedback always overrides the journal.
- **Round** — never a parameter: derived from the journal (prior `Update` notes + 1).

## Notes

- Loop notes are kind `Review` or `Update`; this skill files `Update` only (review-plan files `Review`, create-note files `Note`, create-item / change-item-state file `Event`). Titles carry the counters; kinds are the query keys.
- The state machine, one signal read by both loop skills: this skill folds when the latest loop note is a `Review` and stops when it is an `Update`; review-plan re-reviews when it is an `Update` and fresh-reviews otherwise.
- The disposition must never claim changes that didn't land — hence plan row first, note second, and the note is skipped (with a report) when the plan update fails. A no-change run skips the plan write but still files its disposition.
- Rejections are content, not closures: the reviewer reads the reasons, drops what it cannot beat, and hunts elsewhere — which is what keeps the loop productive.
- Loop health is Donnie's read: the stop condition is shrinking dispositions (accepted deltas getting smaller) or the reviewer's `nothing substantive` — not reviewer silence.
- Prior rejected findings are not re-raised without new evidence — the finiteness rule. From this side: a finding an earlier disposition rejected, re-raised without new evidence, deserves a hard look before acceptance.
- The only filters issued here are the items title `in` lookup, that same row applied to `type=notes` and `type=plans`. Never `["notes","item","eq",...]`, `["plans","item","eq",...]`, or `["notes","kind","eq",...]` — kind filtering, where a skill needs it, is the target-keyed row `[["note-kinds","name","in",[...]]]` (same-type `:eq` on a reference field 500s; backlog-api has the detail).
- Never edits the item's state — the loop runs at `20 Planning`, and approval to `30 Ready` is Donnie's. No state gate here by design: a fold on an item past Planning is Donnie's explicit call (his invocation), and the report names the state.
- Never edits an Independent plan — Independents are pipeline inputs for synthesis, not canonical plans.
- `summary` is the search surface: titles and summaries are the searchable plan fields, descriptions never — a substantive fold refreshes the summary.
- Sequential writes: plan update → note insert → contributors update — never parallel.
- Agents **create** and **update** but never **delete**; this skill updates one plan row, inserts one note, and updates at most one item's contributors.
- The old file-world skill, data-ui-update-pass, was retired at the November 2026 cutover; its craft (verify before accepting, one-line rejection reasons, flagging product disagreements, the disposition shape) lives on in steps 6–8.
