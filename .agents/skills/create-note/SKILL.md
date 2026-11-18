---
description: "Create a Note-kind journal note attached to a backlog item in the production Data UI Backlog app (backlog.demo.data-ui.com) over its REST API, as the invoking agent's own machine account. The note carries a crafted title (unique per item), a searchable summary capped at 1000 characters, and a markdown body; dated now, authored by the agent. Stops and reports on title collisions instead of retitling. Use when Donnie asks to note, record, or attach context to a backlog item — a decision, finding, or discussion distillation ('note that X on the Y item', 'record that on the backlog', '/create-note <item title> <note content>'). Files kind Note only — Event rows belong to create-item / change-item-state. Project-scoped to data-ui; works from any harness (Emacs gptel, OpenCode) on evo-x2."
name: create-note
---

# Skill: create-note

File one Note-kind `:notes` row attached to an item in the production Data UI Backlog app, via REST, attributed to the invoking agent. Notes are the journal's reference layer — context distilled from a discussion, decisions, findings, requirements — beside the Event layer (`Created`, `→ <state>`) stamped by create-item / change-item-state.

## When to Use

- Donnie asks to note / record / file context on a backlog item — a decision, a finding, a requirement, anything worth remembering that does not change the item's state.
- **Not** for: creating items (create-item), state changes (change-item-state), plans (later skills in this family; a note is compact, a plan is a whole document), or Event-kind journal rows (this skill files kind `Note` only).

## Instructions

### 1. Access

All API calls go through the `backlog` wrapper — it resolves your identity, logs in your machine account, and attaches the Bearer token itself, failing loudly with its own message on unresolved identity or failed login; treat either as stop-and-ask-Donnie (never act under another agent's identity). Identity flags: `--name <agent stem>` (Emacs/gptel agents) or `--provider P --model M` (OpenCode). Verb reference and app gotchas live in the `backlog-api` skill; output is the raw JSON envelope — pipe through `jq`.

### 2. Find the item

Items are addressed by exact title (the unique identity column; the title `in` filter is `:eq`-bug-safe). No match → tell Donnie and ask; never guess or fuzzy-match.

```bash
B=~/r/elisp/ai/agents/backlog
F=$(jq -nc --arg t "$ITEM_TITLE" '[["items","title","in",[$t]]]')
$B --name rose list type=items filters="$F" limit=1 \
  | jq '.result.records[0] | {id, title, status, "public-url"}'
```

Take `title` (the display name the note's `item` reference submits — the uuid is not needed) and `public-url` (for the report).

### 3. Resolve the kind

Fetch the palette and match `Note` exactly:

```bash
$B --name rose list type=note-kinds | jq -c '.result.records[].name'
# => "Event" "Note" "Review" "Update"
```

`Note` absent → stop and tell Donnie (never file an Event here, never create kinds). Event rows are the stamp discipline of create-item / change-item-state; Review and Update rows belong to review-plan / update-plan (the plan ping-pong loop); this skill files kind `Note` only.

### 4. Draft the note

- **title** — a short descriptive phrase naming the note (seed style: "View form — unfinished plan sketch", "Auto-registration"). It must be distinct on the item: notes identity is `(:title :item)`, so a title can exist once per item, forever. No model cap — short by discipline, like an item title.
- **summary** — always sent. One to three sentences carrying the gist: this is the note's search surface (note search matches titles and summaries; descriptions are never searchable). Hard cap 1000 characters.
- **description** — the body, markdown. Substantive, never blank. A note is a compact journal reference — if the material needs more, it is a plan (a later skill), not a note; say so instead of filing a wall.
- **author** — your author row. **date** — now, or the date Donnie names in the invocation.

### 5. Insert the note

```bash
[ "${#SUMMARY}" -le 1000 ] || { echo "summary over 1000 chars" >&2; exit 1; }
NOW=$(date +%Y-%m-%dT%H:%M:%S)
DATA=$(jq -nc --arg title "$NOTE_TITLE" --arg item "$ITEM_TITLE" \
  --arg author "Rose" --arg date "$NOW" --arg summary "$SUMMARY" \
  --arg desc "$DESC" \
  '{title:$title, item:$item, kind:"Note",
    author:$author, date:$date,
    summary:$summary, description:$desc}')
$B --name rose insert type=notes data="$DATA" \
    roles='["public","backlog-agents","contributors"]' | jq '.result'
# => {"id":"<uuid>","inserted":true}
```

**`inserted` must be `true`.** `false` means a note with this title already exists on this item: stop, read the existing row back (the response names its id — the step 6 query with it), report its title and date, and ask Donnie whether to retitle the new note or hand-update the existing one. Never silently retitle.

### 6. Verify and report

```bash
$B --name rose item type=notes id=$NOTE_ID \
  | jq '.result.record | {title, item, kind, author, date, summary,
      "public-url"}'
```

Check: every field matches what you submitted, `public-url` present. The note appears in the item's journal automatically (notes embed under the item's view-form, newest first).

Report to Donnie: the item title, the note title, the summary, the note's `public-url`, and the item's `public-url`. A single insert — if it failed, nothing landed; report the error verbatim, no blind retries.

## Inputs

- **Item title** (required) — the exact title of the item the note attaches to.
- **Note content** (required) — from session context.
- **Note title** (optional) — crafted from the content when the invocation does not give one; must be distinct on the item.
- **Date** (optional) — now by default; a date Donnie names in the invocation.

## Notes

- Notes identity is `(:title :item)` — the duplicate check rides the insert response (`inserted`); no pre-check filter is needed, and this skill never filters notes at all (see next).
- The only filter issued here is the items title `in` lookup — never `["notes","item","eq",...]` (same-type `:eq` on a reference field 500s; backlog-api has the detail).
- `summary` is the search surface: titles and summaries are the searchable note fields, descriptions never (nothing index-bearing on big text columns). The 1000-char cap keeps summaries safe for any index that may one day sit on the column.
- Reference fields submit display names (`item` = the item title, `kind` = "Note", `author` = your author row name); list fields submit arrays; JSON wire keys are hyphenated (`data=` payloads).
- Roles: notes are public rows, `["public", "backlog-agents", "contributors"]` — same spelling as every public insert in this family.
- Agents **create** and **update** but never **delete**; this skill only inserts.
