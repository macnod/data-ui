---
description: "Create a new item in the production Data UI Backlog app (backlog.demo.data-ui.com) over its REST API, as the invoking agent's own machine account, with queue-slot positioning and a Created journal event. When the distilled description would exceed ~300 words, files the overflow as a birth 'Details' note (kind Note) linked to the item and leaves the item description a summary of it. Ask for the position when the invocation carries no positioning parameter, and confirm the mvp tag unless the invocation passes mvp. Use when Donnie asks to file, add, or create a backlog item ('file that as a backlog item', 'add an item for X', '/create-item [existing item title] [mvp]'). Project-scoped to data-ui; works from any harness (Emacs gptel, OpenCode) on evo-x2."
name: create-item
---

# Skill: create-item

Create one `:items` row in the production Data UI Backlog app from session context, via REST, attributed to the invoking agent. Also stamps the item's first journal event ("Created"). When the drafted description would exceed ~300 words, additionally files a birth "Details" note (kind Note) carrying the full context verbatim, leaving the item description a summary of it. New items are app rows; the org backlog files were deleted at the November 2026 cutover (git history keeps them).

## When to Use

- Donnie asks to file / add / create a new backlog item — a bug, capability gap, chore, or question for the backlog.
- **Not** for: notes on existing items (create-note), plans, state changes, or tag/author creation (other skills in this family; ask before improvising them). One exception rides this skill: the birth "Details" note filed on its own new item when the description overflows (step 2b) — that is not a create-note call.

## Instructions

### 1. Access

All API calls go through the `backlog` wrapper — it resolves your identity, logs in your machine account, and attaches the Bearer token itself, failing loudly with its own message on unresolved identity or failed login; treat either as stop-and-ask-Donnie (never file under another agent's identity). Identity flags: `--name <agent stem>` (Emacs/gptel agents) or `--provider P --model M` (OpenCode). Verb reference and app gotchas live in the `backlog-api` skill; output is the raw JSON envelope — pipe through `jq`.

### 2. Draft the row from session context

- **title** — short noun phrase naming the ask, ≤ 120 chars, unique. Spec detail belongs in the description, not the title.
- **description** — markdown distilling the ask and its context from the conversation. Always substantive; never blank. Draft the full text first ($DRAFT) and apply the size check: over ~300 words, $DRAFT does not go in the description field — split it (step 2b) and write $DESC as a summary of the note. At or under ~300 words, $DESC is simply $DRAFT.
- **status** — display name (not uuid). Default `10 Open` unless Donnie says otherwise. States: 10 Open, 20 Planning, 30 Ready, 40 Implementing, 50 Code Complete, 60 Done.
- **tags** — existing tag names only (`:join-items-exist` rejects unknown names; fetch the palette with `list type=tags`). If the natural tag doesn't exist, leave it off and say so — creating tags is out of scope here. Pick the natural tags yourself; the only tag that needs Donnie's say-so is `mvp` — see the mvp parameter below.
- **contributors** — default `["Donnie", "<your author>"]` unless the request says otherwise.
- **target-date** — only when Donnie named a deadline.
- **queue-slot** — computed below; always sent explicitly (blank = NULL = sorts last, invisible to "next item").

### 2b. Split an oversized description

When the full description draft ($DRAFT) exceeds ~300 words (≈ 2000 characters, ballpark — count words), the description field does not carry it:

```bash
wc -w <<<"$DRAFT"   # over 300 → split (this step)
```

The split produces three pieces:

- **Item description ($DESC)** — written fresh: two to four sentences carrying the ask's substance. No pointer to the note — the journal's child region under the item's fields already surfaces it, so the summary stands alone.
- **Details note body** — $DRAFT, verbatim. No trimming, however long: note descriptions are not sortable and carry no index, so any length is fine, and this note is the item's details layer, not a standalone journal note. The report carries the word count.
- **Details note summary ($NOTE_SUMMARY)** — a one-to-three-sentence gist: the note's search surface (note search matches titles and summaries, never descriptions). Hard cap 1000 chars.

The split is automatic — no ask, no parameter (an explicit in-session instruction from Donnie to keep everything in the description overrides it; note the size in the report). Write order stays sequential: item (holding the summary description), Created event, then the Details note (step 5b).

### 3. Compute the queue slot

Queue discipline: `:queue-slot` integer, smaller = earlier, gaps of 100 from 1000.

**No positioning argument → ask.** Never silently append: ask Donnie where the item should land (before which item, or append at the end) and only then apply the matching rule below. The append rule runs only on an explicit "append" / "at the end".

**Append (only on Donnie's explicit say-so):** new slot = highest non-null slot + 100.

**Argument names an existing item X (by title or queue-slot number) → land immediately before X:** new slot = `floor((pred_slot + X_slot) / 2)`. If X is the first item (no predecessor): `X_slot - 100`. A prose request for "after X" means position-before X's successor (successor via `queue-slot gt X_slot`, `sort=queue-slot:asc`, `limit=1`).

```bash
B=~/r/elisp/ai/agents/backlog

# X by title (also doubles as the duplicate-title check for your draft)
F=$(jq -nc --arg t "$XTITLE" '[["items","title","in",[$t]]]')
$B --name rose list type=items filters="$F" limit=1 \
  | jq '.result.records[0]'

# predecessor of X (largest slot below X's)
F=$(jq -nc --argjson s "$XSLOT" '[["items","queue-slot","lt",$s]]')
$B --name rose list type=items filters="$F" sort=queue-slot:desc limit=1 \
  | jq '.result.records[0]'

# highest slot (append case)
F='[["items","queue-slot","gte",0]]'
$B --name rose list type=items filters="$F" sort=queue-slot:desc limit=1 \
  | jq '.result.records[0]'
```

Edge cases: no match for X's title → tell Donnie, ask (never silently append). X's slot is null → same. Predecessor gap of 1 (midpoint floors to a tie) → proceed but flag the tie in your report.

**The `:eq` filter bug**: never filter by a same-type `:eq` row on a reference field (e.g. `["items","status","eq","10 Open"]`) — it 500s or silently matches zero. Use only the shapes above (own-column `lt`/`gte`, title `in`) or target-keyed `:in` rows — backlog-api has the detail.

### 4. Insert the item

Roles are named on every insert — the API pre-selects nothing. Items are public rows: `["public", "backlog-agents", "contributors"]`. Reference fields submit **display names** (`status: "10 Open"`), list fields submit arrays of names.

```bash
DATA=$(jq -nc --arg title "$TITLE" --arg desc "$DESC" \
  --arg status "10 Open" --argjson tags "$TAGS_JSON" \
  --argjson contribs "$CONTRIBS_JSON" --argjson slot "$SLOT" \
  '{title:$title, description:$desc, status:$status,
    tags:$tags, contributors:$contribs, "queue-slot":$slot}')
$B --name rose insert type=items data="$DATA" \
    roles='["public","backlog-agents","contributors"]' | jq '.result'
# => {"id":"<uuid>","inserted":true}
```

**`inserted` must be `true`.** A duplicate identity (title already exists) returns the *existing* row's id with `inserted: false` and creates nothing — if you see that, stop and tell Donnie which row already holds the title.

### 5. Stamp the Created event

Every item's journal starts with a Created event (the seed pattern). Sequential insert, after the item exists (the event's `item` reference resolves by title):

```bash
NOW=$(date +%Y-%m-%dT%H:%M:%S)
DATA=$(jq -nc --arg item "$TITLE" --arg author "Rose" --arg date "$NOW" \
  '{item:$item, kind:"Event", title:"Created",
    author:$author, date:$date}')
$B --name rose insert type=notes data="$DATA" \
    roles='["backlog-agents","contributors"]' | jq '.result'
```

### 5b. Insert the Details note (overflow path only)

Only when step 2b split the description. Kind is `Note` (the note-kinds palette is Event / Note / Review / Update — the same palette step 3 of create-note verifies live; Review and Update are the plan-loop skills'). `NOW` comes from step 5. The note lands in the item's journal — the child region under the item's view-form — through its `item` reference.

```bash
DATA=$(jq -nc --arg item "$TITLE" --arg author "Rose" --arg date "$NOW" \
  --arg summary "$NOTE_SUMMARY" --arg desc "$DRAFT" \
  '{title:"Details", item:$item, kind:"Note",
    author:$author, date:$date,
    summary:$summary, description:$desc}')
$B --name rose insert type=notes data="$DATA" \
    roles='["backlog-agents","contributors"]' | jq '.result'
# => {"id":"<uuid>","inserted":true}
```

**`inserted` must be `true`.** `false` would mean a note titled "Details" already exists on the item — impossible on a fresh one (its only journal row is the Created event, and a pre-existing item title already stopped the skill at step 4) — so treat it as an anomaly: stop, read the existing row back (its id rides the response), and tell Donnie.

### 6. Verify and report

Read the rows back with the `item` verb (record `roles` ride `/api/item`, not `/api/list`):

```bash
$B --name rose item type=items id=$ITEM_ID \
  | jq '.result.record | {title, status, "queue-slot", tags,
      contributors, roles, "public-url"}'
```

Check: `public-url` present; `roles` contains public, backlog-agents, contributors (admin and your own exclusive role are hidden from your view — that is expected); slot/status/tags/contributors match what you submitted. Verify the event the same way (`type=notes`): title, kind, author, and date match what you submitted. When the overflow path ran, verify the Details note the same way: every field matches what you submitted (title Details, kind Note, your summary, the full body).

Report to Donnie: the title, the slot with its neighbors (the items before and after), the public-url, and confirmation of the Created event. When the overflow path ran, also report that the description was filed as a summary with the full details in the Details note — give the body's word count. If anything failed mid-way, say exactly what landed and what didn't (no transactions — the item insert stands even if the event or Details-note insert fails).

## Inputs

- **Item content** (required) — from session context: the ask, its context, tags, deadline if any.
- **Oversized description** — no parameter: the step 2b split fires automatically whenever the drafted description exceeds ~300 words. Donnie never has to ask for it, and an explicit in-session instruction to keep everything in the description overrides it (note the size in the report).
- **Positioning argument** (optional) — an existing item's title or queue-slot number; the new item lands immediately before it. When omitted, ask Donnie where the item should land before filing — never default to appending.
- **mvp parameter** (optional) — the literal word `mvp`, independent of the positioning argument. When present, the item carries the `mvp` tag. When absent, ask Donnie whether it should carry `mvp` before inserting (a forgotten mvp tag means a later update, not a guess). If the positioning question is also pending, ask both in one message.

## Notes

- Agents **create** and **update** but never **delete** (role spelling omits it); this skill only inserts.
- Insert order is sequential (item, then Created event, then the overflow Details note) — never parallel writes.
- The birth "Details" note is this skill's overflow valve, not a create-note call: fixed title, kind Note, and a verbatim body of any length — note descriptions are not sortable and carry no index, so nothing needs trimming (create-note's compactness craft governs standalone notes; the report carries the word count).
- JSON wire keys are hyphenated (`queue-slot`) — the `data=` payloads carry them; the server upper-cases and keywordizes them.
- The next-item rule (not needed here, but context): "next" = lowest queue-slot among non-"60 Done" items.
