---
description: "Implement one backlog item from the production Data UI Backlog app (backlog.demo.data-ui.com) end to end, as the invoking agent's own machine account. Starts only from 30 Ready (flips the item to 40 Implementing with a '→ 40 Implementing' journal Event) or 40 Implementing (resumes an interrupted run — no duplicate event); every other state stops and asks. Plan source: the item's canonical 'Plan: <item title>' row (kind Simple or Synthesized); 2+ Independents without one → stop and suggest synthesize-plan; a lone Independent → stop and ask; no plans → the item's own description, flagged. Guards the data-ui git repo clean and the live image up before any write. Invoking the skill is Donnie's explicit permission to edit Lisp files for the item; changes are left unstaged, the final gate is scripts/data-ui tests, and on completion the item is flipped to 50 Code Complete with an event plus an implementation-summary journal Note. 60 Done stays Donnie's — he reviews, commits, and flips. Use when Donnie asks to implement, work, or start a backlog item ('implement X', 'start implementing X', '/implement-plan <item title>'). Project-scoped to data-ui; works from any harness (Emacs gptel, OpenCode) on evo-x2."
name: implement-plan
---

# Skill: implement-plan

Implement one backlog item from the production Data UI Backlog app, with the bookkeeping attributed to the invoking agent: flip the item to `40 Implementing` (event-stamped), implement its canonical plan in the data-ui repo, leave every change unstaged, then flip to `50 Code Complete` (event-stamped) and file an implementation-summary journal Note. `60 Done` stays Donnie's — he reviews, commits, and flips. The app is the backlog; the org backlog files were deleted at the November 2026 cutover (git history keeps them).

## When to Use

- Donnie asks to implement / work / start a specific backlog item — "implement X", "start implementing X", "work the plan for X", "/implement-plan <item title>".
- Requires the item at `30 Ready` (normal start) or `40 Implementing` (resume an interrupted run). `20 Planning` means the plan isn't approved yet — approval to `30 Ready` is Donnie's, not this skill's.
- **Not** for: planning (create-simple-plan, create-independent-plan, synthesize-plan), state changes on their own (change-item-state — this skill flips states only as part of implementing), notes on their own (create-note), or any project other than data-ui — "implement todo N" in job-search or another repo means that project's own todo.org workflow. If the session's AGENT.md in context is not data-ui's, stop.

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

Take `id` (the update targets it), `status`, `contributors`, and `plans` (the linked plan titles — step 4 cross-checks them). Then read the full record and the journal (the item's `description` is not in list results):

```bash
$B --name rose item type=items id=$ITEM_ID \
  | jq '.result.record | {title, description, "public-url"}'

# the item's journal — the same title row applied to notes
# (target-keyed :in, the :eq-bug-safe shape)
$B --name rose list type=notes filters="$F" sort=date:desc \
  | jq -c '.result.records[] | {title, kind, author, date, summary}'
```

### 3. Guard the item's state

By the `status` just read:

- `30 Ready` → **normal start**: step 6 flips to `40 Implementing` and stamps the event.
- `40 Implementing` → **resume**: no flip, no event (the journal keeps the first entry into the state); step 6 ensures contributors only. Note the date of the existing `→ 40 Implementing` event from the journal — the report shows it as the original start.
- `10 Open` → **stop and ask** — unplanned item; route to the planning skills first.
- `20 Planning` → **stop and ask** — the plan isn't approved yet; approval to `30 Ready` is Donnie's.
- `50 Code Complete` → **stop and ask** — already implemented; reviewing, committing, or reopening is his call.
- `60 Done` → **stop and ask**.

### 4. Resolve the plan source

One query returns every plan referencing the item (target-keyed `in`):

```bash
$B --name rose list type=plans filters="$F" \
  | jq -c '.result.records[] | {id, title, kind, author}'
```

Resolve in this order:

- **Exactly one canonical plan** — kind `Simple` or `Synthesized`. That is the plan; read it fully (below). Its title should read `Plan: <item title>`; if it doesn't (the item was renamed after filing), use it anyway and note the drift in the report — the kind marks it canonical, not the title. If the plan document declares itself superseded or withdrawn → stop and ask Donnie.
- **Two or more canonical plans** → stop and ask — ambiguous, and cannot happen through the family skills; report both rows.
- **Independents only, two or more** → stop and ask, suggesting synthesize-plan — the canonical plan is one synthesis away.
- **Exactly one Independent** → stop and ask — an Independent is a pipeline input, not a canonical plan. Donnie's call: a second independent plus synthesis, or a Simple plan (create-simple-plan).
- **No plans at all** → the item's own `description` (step 2) is the plan — the old skill's fallback — **flagged in the final report**.
- Cross-check against the item's `plans` list from step 2: the two views should agree. A plan referencing the item but missing from its list — or a non-empty list with an empty query result — is a data inconsistency; report it, don't heal it.

Read the canonical plan in full — the `description` is the plan document:

```bash
$B --name rose item type=plans id=$PLAN_ID \
  | jq '.result.record | {title, kind, author, summary, description}'
```

### 5. Guard the workspace and the live image

Both checks precede every write — a failed guard leaves the app untouched.

- **Repo clean.** `git -C ~/common-lisp/data-ui status --porcelain` must output nothing. Untracked files count — including this family's own skill dirs awaiting Donnie's commit; he commits them first. Not empty → stop, show him the output; the house rule is that Donnie provides a clean repo.
- **Live image up.** `(eval-in-data-ui "(lisp-implementation-type)")` must return a value — via the `Eval` tool (Emacs harnesses) or `emacsclient -e` (OpenCode; see global.md → Elisp Access for quoting). Error or no socket → stop; ask Donnie to start the Data UI image and Slime. Never start Emacs yourself, never pass `-a` to emacsclient.

### 6. Flip to 40 Implementing (or resume)

Contributors: ensure both Donnie and yourself — set semantics, send the full desired list.

```bash
CONTRIBS=$(jq -nc --arg d "Donnie" --arg a "Rose" \
             --argjson e "$EXISTING_CONTRIBS" \
  '(($e // []) + [$d, $a]) | unique')
```

**Fresh start** (item was `30 Ready`) — update, then event, sequentially:

```bash
DATA=$(jq -nc --arg s "40 Implementing" --argjson c "$CONTRIBS" \
  '{status:$s, contributors:$c}')
$B --name rose update type=items filters=$ITEM_ID data="$DATA" | jq .
# => {"status":"success","result":"<uuid>"} — result must equal $ITEM_ID

NOW=$(date +%Y-%m-%dT%H:%M:%S)
DATA=$(jq -nc --arg title "→ 40 Implementing" --arg item "$ITEM_TITLE" \
  --arg author "Rose" --arg date "$NOW" \
  '{title:$title, item:$item, kind:"Event",
    author:$author, date:$date}')
$B --name rose insert type=notes data="$DATA" \
    roles='["backlog-agents","contributors"]' | jq '.result'
# => {"id":"<uuid>","inserted":true}
```

`inserted:false` on the event means the item held `40 Implementing` at some earlier time (an earlier excursion — e.g. reopened to 30 and started again); the update stands, the journal keeps the first entry. Read the existing event back by the returned id and note its date in the report.

**Resume** (item already `40 Implementing`) — **no status key, no event**. Send the update only if Donnie or you are missing from `contributors` (a no-op update needlessly re-stamps `updated-at`); if both are present, skip the update entirely. Report "resuming — first entry into `40 Implementing` kept from <date>", taking the date from the journal:

```bash
$B --name rose list type=notes filters="$F" \
  | jq -c '.result.records[] | select(.title=="→ 40 Implementing") | {date, author}'
```

### 7. Implement

The invocation of this skill is Donnie's explicit permission to edit Lisp source files in data-ui for this item — the AGENT.md restriction lifts for the duration. Still:

- Re-read the doc for each area before touching it: `docs/model-reference.md` (models/fixtures), `docs/lisp-style.md` (any Lisp), `docs/hook-registry.md` (hook code) — plus whichever doc AGENT.md names for the area (`docs/rest.md` for the REST layer, `docs/deployment.md` for deploy work, ...).
- Track the implementation steps with your harness's todo/task-list mechanism.
- After editing any `.lisp` or `.el` file: verify parentheses — `(ai-check-parens "<file>")` via the `Eval` tool or `emacsclient -e` — then reload it into the live image before testing anything (`eval-in-data-ui` with `(load "...")`; `(hard-reset)` when the change is broad), or you test stale code. Mind the `rest.lisp` reload gotcha in AGENT.md.
- Test only through `tests/helpers.lisp` runners; you may add a `run-*` helper there (agents are explicitly permitted to modify that file), but the test suites themselves still need human permission. Test models come only from `models/test/`. Long in-image suites use the background-thread + file pattern (60s eval timeout — never poll with sleeps).
- Frontend changes: `npm install && npm run build` in `web/` — without the build, changes are invisible.
- **Final gate:** `scripts/data-ui tests` (full suite against the code on disk, isolated test database) must pass before the wrap-up writes of steps 8–9.
- Never stage or commit anything. All changes stay unstaged for Donnie's review.
- In-session direction from Donnie overrides the plan document where they conflict — record every deviation in the summary note.
- **Cannot complete** (blocked, tests failing, plan turns out wrong)? Stop implementing. Leave the item at `40 Implementing` — no 50 flip, no summary note — and report exactly where the work stands: files touched, what is done, what is not, and why. A later run resumes at 40.

### 8. Flip to 50 Code Complete

Only when the plan is implemented and every gate is green. Same discipline as step 6's fresh start — update, then event, sequentially:

```bash
DATA=$(jq -nc --arg s "50 Code Complete" --argjson c "$CONTRIBS" \
  '{status:$s, contributors:$c}')
$B --name rose update type=items filters=$ITEM_ID data="$DATA" | jq .
# => {"status":"success","result":"<uuid>"} — result must equal $ITEM_ID

NOW=$(date +%Y-%m-%dT%H:%M:%S)
DATA=$(jq -nc --arg title "→ 50 Code Complete" --arg item "$ITEM_TITLE" \
  --arg author "Rose" --arg date "$NOW" \
  '{title:$title, item:$item, kind:"Event",
    author:$author, date:$date}')
$B --name rose insert type=notes data="$DATA" \
    roles='["backlog-agents","contributors"]' | jq '.result'
# => {"id":"<uuid>","inserted":true}
```

`inserted:false` on the event → the item held `50 Code Complete` earlier (reopened and re-implemented); the update stands; report the stale event's date.

### 9. File the implementation-summary Note

A Note-kind journal entry — the app-world equivalent of the old skill's DONE-with-summary. Title `Implementation summary` (unique per item), author = you, date = now, `summary` always sent (≤1000 chars, the searchable surface), `description` = the body, any length (not sortable, no index): what was built, files touched, tests run and their results, deviations from the plan, and what Donnie should look at first in review.

```bash
[ "${#SUMMARY}" -le 1000 ] || { echo "summary over 1000 chars" >&2; exit 1; }
DATA=$(jq -nc --arg title "Implementation summary" --arg item "$ITEM_TITLE" \
  --arg author "Rose" --arg date "$NOW" \
  --arg summary "$SUMMARY" --arg desc "$DESC" \
  '{title:$title, item:$item, kind:"Note",
    author:$author, date:$date,
    summary:$summary, description:$desc}')
$B --name rose insert type=notes data="$DATA" \
    roles='["backlog-agents","contributors"]' | jq '.result'
# => {"id":"<uuid>","inserted":true}
```

`inserted:false` → a prior run already filed this note (an interrupted run that got as far as the note). Stop and ask Donnie whether to update the existing row — never re-file under a retitled name.

### 10. Verify and report

```bash
$B --name rose item type=items id=$ITEM_ID \
  | jq '.result.record | {title, status, contributors,
      "updated-at", "public-url"}'

for ID in $EVENT_40_ID $EVENT_50_ID $NOTE_ID; do
  $B --name rose item type=notes id=$ID \
    | jq -c '.result.record | {title, kind, author, date}'
done
```

Check: item `status` is `50 Code Complete`; `contributors` include Donnie and you; `updated-at` bumped; each event and the note exist with the right author and date.

Report to Donnie: the item title and its full state path (`30 → 40 → 50`, or `40 → 50` resuming, with the original-start date on a resume); the plan source used (canonical plan title + kind — noting title drift — or **item description, flagged**); files touched; test results including the final gate (`scripts/data-ui tests`) and the frontend build when `web/` changed; the events and note referenced by title (noting stale prior ones by date); the item's `public-url`. **All changes are unstaged** — he reviews, commits, then flips `60 Done` (by hand or change-item-state). No transactions — on partial failure say exactly what landed: a failed 50-flip leaves the work real but the item at `40 Implementing` (re-run step 8); a failed note insert leaves both flips standing (re-run step 9).

## Inputs

- **Item title** (required) — the exact title of the item to implement.
- **Extra direction** (optional) — in-session instructions from Donnie override the plan document where they conflict; every deviation is recorded in the summary note.

## Notes

- **End state is `50 Code Complete`, never `60 Done`**: commit and the 60 flip are Donnie's — the house workflow (the agent never stages or commits; the human reviews, then closes).
- **Resume semantics:** `40 Implementing` is re-enterable; the journal keeps the *first* `→ 40 Implementing` event; a resume stamps no duplicate events and skips a no-op item update (a pointless `updated-at` re-stamp).
- **The plan-source chain is stateless** — a resume resolves it the same way: the canonical plan is the kind-`Simple`-or-`Synthesized` row among the plans referencing the item. Title drift from a later item retitle doesn't change that (the kind marks canonical, not the title); only the conventional name drifts.
- The only filters issued here are the items title `in` lookup, that same row applied to `type=notes` (journal reads) and `type=plans` (plan-source resolution). Never `["plans","item","eq",...]` or `["notes","item","eq",...]` (same-type `:eq` on a reference field 500s; backlog-api has the detail).
- The clean-repo gate counts untracked files — this family's own skill dirs block the first run until Donnie commits them. That is the house rule (he provides a clean repo), not a bug.
- Sequential writes: 40-flip update → event → *(implement)* → 50-flip update → event → note. Never parallel.
- Reference fields submit display names (`status` = the state display name, `item` = the item title, `kind` = "Event" / "Note", `author` = your author row name); list fields submit arrays; JSON wire keys are hyphenated (the `data=` payloads).
- Agents **create** and **update** but never **delete**; this skill updates one item and inserts at most two events and one note.
- The old file-world skill, `implement-data-ui-plan`, was retired at the November 2026 cutover (skill dirs and org backlog files deleted; git history keeps them); its craft (clean repo, live image, permission framing, paren checks, test gates, unstaged wrap-up) lives on in steps 5, 7, and 10.
