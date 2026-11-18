---
description: "Change an item's status in the production Data UI Backlog app (backlog.demo.data-ui.com) over its REST API, as the invoking agent's own machine account. Every transition stamps a '→ <status>' journal Event (author = the agent, date = now); flipping an item into 40 Implementing or beyond adds the agent to its contributors if absent. Refuses no-op flips (item already in the requested state). Use when Donnie asks to change, set, flip, advance, or reopen an item's state — mark done, mark ready, start implementing, code complete ('mark X done', 'X is ready now', 'start implementing X', '/change-item-state <item title> <state>'). Project-scoped to data-ui; works from any harness (Emacs gptel, OpenCode) on evo-x2."
name: change-item-state
---

# Skill: change-item-state

Change one `:items` row's status in the production Data UI Backlog app, via REST, attributed to the invoking agent. Every transition stamps a journal Event (`→ <state>`); flips into 40 Implementing or beyond add the agent to the item's contributors. Item state lives in the app; the org backlog files were deleted at the November 2026 cutover (git history keeps them).

## When to Use

- Donnie asks to change / set / flip / advance / reopen an item's state — mark something done, mark ready, start implementing, code complete, reopen a done item.
- **Not** for: creating items (create-item), filing notes or plans, or edits to other fields (description, tags, target-date, queue-slot) — later skills in this family; ask before improvising them.

## Instructions

### 1. Access

All API calls go through the `backlog` wrapper — it resolves your identity, logs in your machine account, and attaches the Bearer token itself, failing loudly with its own message on unresolved identity or failed login; treat either as stop-and-ask-Donnie (never act under another agent's identity). Identity flags: `--name <agent stem>` (Emacs/gptel agents) or `--provider P --model M` (OpenCode). Verb reference and app gotchas live in the `backlog-api` skill; output is the raw JSON envelope — pipe through `jq`.

### 2. Find the item and its current state

Items are addressed by exact title (the unique identity column; the title `in` filter is `:eq`-bug-safe). No match → tell Donnie and ask; never guess or fuzzy-match.

```bash
B=~/r/elisp/ai/agents/backlog
F=$(jq -nc --arg t "$TITLE" '[["items","title","in",[$t]]]')
$B --name rose list type=items filters="$F" limit=1 \
  | jq '.result.records[0] | {id, title, status, contributors}'
```

Take `id` (the uuid — the update targets it directly), `status` (current display name), and `contributors` (needed for the 40+ rule).

### 3. Resolve the target state

Fetch the palette and match the requested state exactly:

```bash
$B --name rose list type=states | jq -c '.result.records[].name'
# => "10 Open" "20 Planning" "30 Ready" "40 Implementing" "50 Code Complete" "60 Done"
```

Natural-language mapping: done / finished / shipped → `60 Done`; implementing / in progress / started / working on → `40 Implementing`; ready → `30 Ready`; planning → `20 Planning`; open / reopened → `10 Open`; code complete → `50 Code Complete`. Ambiguous or unmatched → ask Donnie; never pick a close name.

**Refuse no-ops:** requested state == current state → stop and report the current state (no update, no event).

Transitions are otherwise free-form — forward, backward, skips — no lifecycle enforcement lives here; the semantic discipline (e.g. flip to 40 Implementing only when implementation actually begins) is AGENT.md's job. The report always shows old → new so regressions stay visible.

### 4. Update the item

The update verb takes partial `data` (omitted fields are left unchanged); **omit `roles`** — the record keeps them. Never send `updated-at` / `created-at` (both read-only; the DB trigger re-stamps `updated-at` on every update). `status` submits as the display name.

**Contributors rule:** when the new state is `40 Implementing`, `50 Code Complete`, or `60 Done` and your author is not already in `contributors`, send the full list with yourself appended — the list then reflects who actually worked the item. Otherwise send status only.

```bash
# status only
DATA=$(jq -nc --arg s "$STATE" '{status:$s}')

# status + contributors (40+ and you're absent)
DATA=$(jq -nc --arg s "$STATE" --arg a "Rose" --argjson e "$CONTRIBS" \
        '{status:$s, contributors:(($e // []) + [$a])}')

$B --name rose update type=items filters=$ITEM_ID data="$DATA" | jq .
# => {"status":"success","result":"<uuid>"} — result must equal $ITEM_ID
```

A non-success response (HTTP 4xx/5xx with a JSON error body) → report the error verbatim to Donnie and stop.

### 5. Stamp the transition event

Every transition lands a journal Event, matching the seed pattern: title `→ <state>`, kind `Event`, author = you, date = now, no summary/description. The event's `item` reference resolves by title, so the item must already exist (it does — step 4 ran first).

```bash
NOW=$(date +%Y-%m-%dT%H:%M:%S)
DATA=$(jq -nc --arg title "→ $STATE" --arg item "$TITLE" \
  --arg author "Rose" --arg date "$NOW" \
  '{title:$title, item:$item, kind:"Event",
    author:$author, date:$date}')
$B --name rose insert type=notes data="$DATA" \
    roles='["public","backlog-agents","contributors"]' | jq '.result'
# => {"id":"<uuid>","inserted":true}
```

**Re-entry collision:** notes identity is `(:title :item)` — one `→ <state>` event per item, forever. `inserted:false` means the item held this state before; the status update stands. Take the returned id (it names the pre-existing row) and read it back in step 6 to report its date — the journal keeps the *first* entry into that state.

### 6. Verify and report

Read both rows back (record `roles` ride `/api/item`, not `/api/list`):

```bash
$B --name rose item type=items id=$ITEM_ID \
  | jq '.result.record | {title, status, contributors, "updated-at", "public-url"}'

$B --name rose item type=notes id=$EVENT_ID \
  | jq '.result.record | {title, item, kind, author, date, "public-url"}'
```

Check: item `status` is the new state; `updated-at` bumped; contributors include you when the 40+ rule fired; the event's title/author/date are right and it has a `public-url`.

Report to Donnie: the item title, **old → new** status, the contributors addition (if any), the event's `public-url` (on re-entry, noting the stale prior event by its date), and the item's `public-url`. No transactions — if the event insert failed, the status change stands; say exactly what landed and what didn't.

## Inputs

- **Item title** (required) — the exact title of the item to flip.
- **Target state** (required) — a state display name or its natural-language equivalent; resolved against the live palette.

## Notes

- Sequential writes: update, then event — never parallel.
- The update verb's response has no `inserted` flag; duplicate-identity semantics apply only to the event insert.
- The only filter issued here is the items title `in` lookup — never `["notes","item","eq",...]` (same-type `:eq` on a reference field 500s; backlog-api has the detail). Item targeting uses the bare uuid.
- Errors from the update verb render as HTTP 4xx/5xx plus a JSON error body — report verbatim, no blind retries.
- Context: "next" item = lowest queue-slot among non-`60 Done` items — flipping something into `60 Done` changes what is next.
