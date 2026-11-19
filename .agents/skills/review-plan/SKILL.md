---
description: "Review the canonical plan (kind Simple or Synthesized) attached to a backlog item in the production Data UI Backlog app (backlog.demo.data-ui.com), as the invoking agent's own machine account: verify the plan's factual claims against the actual code and report findings tagged [error] / [improvement] / [taste] with file/function citations, filed as a Review-kind journal note titled 'Review findings — N'. Takes just the item title — fresh review or re-review is read from the journal's latest loop note (kind Review or Update), and an in-session disposition list or instruction overrides. A re-review that finds nothing at [error]/[improvement] level ends with exactly 'nothing substantive' — the loop's stop signal, which also opens the note's summary on a closing review. Never edits the plan or the item's state; folding feedback in is update-plan. Stops when the item holds no canonical plan (Independents only → suggests synthesize-plan; none at all). Use when Donnie asks to review a plan ('review the plan for X', 're-review X', '/review-plan <item title>'). Project-scoped to data-ui; works from any harness (Emacs gptel, OpenCode) on evo-x2."
name: review-plan
---

# Skill: review-plan

Reviewer's pass of the plan ping-pong loop. Reads the item's canonical plan row (kind `Simple` or `Synthesized`), verifies its factual claims against the actual code, and files the findings as a `Review`-kind journal note — the durable record of the pass and the next update-plan run's input. Writes only that note (plus the contributors ensure); never the plan row, never the item's state. Pairs with update-plan (the updater's pass, which folds feedback and files `Update`-kind disposition notes). The loop advances only by Donnie's invocations — this skill never chains. App-world port of the old file skill data-ui-review-pass (retired at the November 2026 cutover); verification reads the data-ui repo but edits nothing there.

## When to Use

- Donnie asks to review a plan — "review the plan for X", "re-review X", "/review-plan <item title>", optionally with a disposition list or extra instruction.
- Requires a canonical plan (kind `Simple` or `Synthesized`) attached to the item. Independents only → stops (suggesting synthesize-plan when there are two or more); no plans at all → stops and reports.
- **Not** for: folding feedback into the plan (update-plan — this skill never edits the plan row), implementing (implement-plan), reviewing code rather than a plan document, or any project other than data-ui. If the session's AGENT.md in context is not data-ui's, stop.

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

Take `id`, `status`, `contributors`, and `plans` (step 3 cross-checks them). Then read the full record — the `description` is not in list results:

```bash
$B --name rose item type=items id=$ITEM_ID \
  | jq '.result.record | {title, description, "public-url"}'
```

### 3. Resolve the plan source

One query returns every plan referencing the item (target-keyed `in`):

```bash
$B --name rose list type=plans filters="$F" \
  | jq -c '.result.records[] | {id, title, kind, author}'
```

Resolve in this order:

- **Exactly one canonical plan** — kind `Simple` or `Synthesized`, keyed on kind (the same resolution as implement-plan). That is the review target; read it fully below. Independents beside it are context, not review targets. If the plan document declares itself superseded or withdrawn → stop and ask Donnie.
- **Two or more canonical plans** → stop and ask — ambiguous, and cannot happen through the family skills; report both rows.
- **Independents only, two or more** → stop and ask, suggesting synthesize-plan — the canonical plan is one synthesis away.
- **Exactly one Independent** → stop and ask — an Independent is a pipeline input, not a canonical plan.
- **No plans at all** → stop and report — there is nothing to review (no fallback; unlike implement-plan, the item's description is an ask, not a plan).
- Cross-check against the item's `plans` list from step 2: the two views should agree. A plan referencing the item but missing from its list — or a non-empty list with an empty query result — is a data inconsistency; report it, don't heal it.

Read the canonical plan in full — the `description` is the plan document:

```bash
$B --name rose item type=plans id=$PLAN_ID \
  | jq '.result.record | {title, kind, author, summary, description}'
```

### 4. Read the loop state

The item's journal decides the pass mode. One fetch (the same title row applied to `type=notes`):

```bash
$B --name rose list type=notes filters="$F" sort=date:desc limit=100 \
  | jq -c '{latest_loop: [.result.records[]
                | select(.kind=="Review" or .kind=="Update")][0],
            review_count: ([.result.records[]
                | select(.kind=="Review")] | length),
            details: [.result.records[] | select(.title=="Details")][0]}'
```

- **Loop notes** are kind `Review` or `Update`; `latest_loop` is the first in date-desc order.
- **Mode**: `latest_loop` is kind `Update` → **re-review** of that note — its `description` is the disposition list (update-plan's output format: accepted → what changed, rejected → why, flagged → for Donnie); fetch it via the `item` verb with the note's id. `latest_loop` is kind `Review`, or null → **fresh review**.
- **Round counter** ($N): `review_count` + 1 — the new note is `Review findings — N`.
- **Details note**: when present (create-item's overflow valve), fetch its `description` too — it carries the item's full original context and is part of ground truth.

Announce the detected mode in the report. Donnie's in-session instruction always overrides: a pasted disposition list forces re-review of that list (say so in the report — it came from him, not the journal); "fresh review" forces a full pass regardless of journal state; other direction ("focus on the wire path") shapes the review without changing the mode.

Read the prior Review notes' summaries for context (they ride the journal fetch) — a finding a prior disposition rejected stays settled unless you bring new evidence. That discipline is what keeps the loop finite.

### 5. Verify the note-kinds palette

```bash
$B --name rose list type=note-kinds | jq -c '.result.records[].name'
# => "Event" "Note" "Review" "Update"
```

`Review` absent → stop and tell Donnie — never file under another kind, never create kinds (kind creation is admin's; the RBAC already keeps machine accounts out).

### 6. Review the plan

**Ground truth for the ask:** the item row (description) and its journal — the Details note above all. **Ground truth for facts:** the actual code — the repo at `~/common-lisp/data-ui` (`lisp/`, `web/`, `docs/`); consult the area docs AGENT.md names (model-reference.md for model vocabulary, hook-registry.md for hook contracts, rest.md for the API grammar) when judging claims about them. A plan claim that contradicts the code is an `[error]` whatever the item wanted.

**Fresh review** — read the plan document end to end, then verify:

- every factual claim against the code: file names, function names, the fe-fields wire path, registries, validators, hook contracts, cited DONE-item precedents;
- internal consistency: steps in a workable order, nothing referencing things that don't exist, self-containment — a Simple or Synthesized plan must carry everything implement-plan needs (no 'see the independents');
- gaps: what the item's ask implies that the plan misses.

**Re-review** — the disposition note is the input:

- **Accepted → confirm it landed**: read the current plan document and check each accepted change is actually present and correct.
- **Rejected → contest only with evidence**: a citation the updater missed. Never re-argue judgment calls — taste stays taste.
- **Flagged → Donnie's**: note their status; do not resolve them.
- **Focus on what changed** since the last review — the accepted folds and whatever they touch — not a full re-derivation of the plan.
- **Stop rule**: nothing at `[error]`/`[improvement]` level remaining → the note body ends with exactly `nothing substantive` (its own final line, nothing after it) — the loop's stop signal. `[taste]` findings may remain open at closure.

**Findings format** — every finding carries its tag and a citation (file + function/section):

- `[error]` — factually wrong: misstates the code, cites a file/function/behavior that isn't there, contradicts the item's ask.
- `[improvement]` — correct but weaker than it should be: missed a simpler path, missed a precedent, coverage gap.
- `[taste]` — structure or style preference; advisory, never blocks closure.

### 7. Draft and insert the Review note

- **title** — `Review findings — $N` (the counter from step 4; notes identity is `(:title :item)`, so each round's title is unique on the item).
- **summary** — always sent, ≤1000 chars (the searchable surface): mode + round + counts + verdict, e.g. "Re-review (round 2) of Plan: X — 1 [error], 2 [improvement], 1 [taste]; loop continues". A closing review — one whose body ends with the exact line `nothing substantive` — starts its summary with those words (they are the summary's opening text), e.g. "Nothing substantive. Re-review (round 2) of Plan: X; loop closed", so the stop signal is readable in journal listings without opening the note.
- **description** — the findings, any length (not sortable, no index). Opens with a thread line (`Fresh review of Plan: <title>` / `Re-review of Update disposition — M`), then the findings, each tagged with its citation; a closed loop ends with the exact line `nothing substantive`.

```bash
[ "${#SUMMARY}" -le 1000 ] || { echo "summary over 1000 chars" >&2; exit 1; }
NOW=$(date +%Y-%m-%dT%H:%M:%S)
DATA=$(jq -nc --arg title "Review findings — $N" --arg item "$ITEM_TITLE" \
  --arg author "Rose" --arg date "$NOW" \
  --arg summary "$SUMMARY" --arg desc "$DESC" \
  '{title:$title, item:$item, kind:"Review",
    author:$author, date:$date,
    summary:$summary, description:$desc}')
$B --name rose insert type=notes data="$DATA" \
    roles='["backlog-agents","contributors"]' | jq '.result'
# => {"id":"<uuid>","inserted":true}
```

**`inserted` must be `true`.** `false` means a note with this title already exists on the item — a hand-filed `Review findings — N`, or a counter miscount (a journal longer than the step 4 fetch): stop, read the existing row back (its id rides the response), report, and ask. Never retitle past it.

### 8. Ensure contributors

Ensure both Donnie and yourself on the item's `contributors` — set semantics, send the full desired list (append-only; never remove anyone). Skip the update entirely when both are already present (a no-op update needlessly re-stamps `updated-at`):

```bash
CONTRIBS=$(jq -nc --arg d "Donnie" --arg a "Rose" \
             --argjson e "$EXISTING_CONTRIBS" \
  '(($e // []) + [$d, $a]) | unique')
DATA=$(jq -nc --argjson c "$CONTRIBS" '{contributors:$c}')
$B --name rose update type=items filters=$ITEM_ID data="$DATA" | jq .
# => {"status":"success","result":"<uuid>"} — result must equal $ITEM_ID
```

### 9. Verify and report

```bash
$B --name rose item type=notes id=$NOTE_ID \
  | jq '.result.record | {title, item, kind, author, date, summary}'
```

Check: every field matches what you submitted; the note appears in the item's journal (the child region under the item's view-form, newest first).

Report to Donnie: the item title and its state; the mode and round, and what triggered it (journal state or his instruction); **the findings themselves** — he reads them in the report, the note is the durable copy; the item's `public-url`; the contributors addition if any. Two writes, no transactions — the note insert stands even if the contributors update fails; say exactly what landed and what didn't.

## Inputs

- **Item title** (required) — the exact title of the item whose plan is reviewed.
- **Disposition list / instruction** (optional) — a pasted disposition list forces re-review of it; "fresh review" forces a full pass; other in-session direction shapes the review without changing the mode.
- **Round** — never a parameter: derived from the journal (prior Review notes + 1).

## Notes

- Loop notes are kind `Review` or `Update`; this skill files `Review` only (update-plan files `Update`, create-note files `Note`, create-item / change-item-state file `Event`). Titles carry the counters; kinds are the query keys.
- The state machine, one signal read by both loop skills: update-plan folds when the latest loop note is a `Review` and stops when it is an `Update`; this skill re-reviews when it is an `Update` and fresh-reviews otherwise.
- The only filters issued here are the items title `in` lookup, that same row applied to `type=notes` and `type=plans`. Never `["notes","item","eq",...]`, `["plans","item","eq",...]`, or `["notes","kind","eq",...]` — kind filtering, where a skill needs it, is the target-keyed row `[["note-kinds","name","in",["Review","Update"]]]` (same-type `:eq` on a reference field 500s; backlog-api has the detail).
- Never edits the plan row — folding feedback is update-plan's. Never flips item state — the loop runs at `20 Planning`, and approval to `30 Ready` is Donnie's.
- Prior rejected findings are not re-raised without new evidence — the finiteness rule.
- `nothing substantive` is exact text, the note body's final line, nothing after it — and on a closing review, the note summary's opening words.
- Sequential writes: note insert, then contributors update — never parallel.
- Agents **create** and **update** but never **delete**; this skill inserts one note and updates at most one item's contributors.
- The old file-world skill, data-ui-review-pass, was retired at the November 2026 cutover; its craft (ground-truth verification, tagged findings with citations, the re-review discipline, the stop signal) lives on in step 6.
