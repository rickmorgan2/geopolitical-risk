---
name: close-up-shop
description: Session wrap-up for this repo (Pressure Points: the economic shocks and conflict forecasts pipeline). Use at the end of a work session, or before compacting, to write a durable record of the most recent work. Triggers on "close up shop", "let's close up shop", "/close-up-shop", "write a session summary", "session notes", "update the log", or similar. Summarizes work since the last LOG.md entry (not the whole session), adds a dated entry at the top of LOG.md, and updates DECISIONS.md and CLAUDE.md if the session changed what they describe. Never commits.
---

# Close up shop

Purpose: leave a durable, accurate record of the work just done, so the next
session can start from the files rather than from `/compact`.

This skill writes only to three tracked files at the repo root:

- `LOG.md`: dated work log, **newest first**. Each `## YYYY-MM-DD` heading
  holds one `### <task>` entry per unit of work. This is where the session
  record goes.
- `DECISIONS.md`: one `D-0xx` entry per decision, newest at the bottom, each
  with a status (Accepted / Proposed / Open / Superseded).
- `CLAUDE.md`: what's true now: layout, how to run things, conventions,
  working rules. Kept current and edited in place, never appended to as a
  history.

The session record never goes in `docs/` or `archive/`.

## Scope: summarize since the last LOG entry, not the whole session

1. Read the top of `LOG.md`. The newest entry is the first `###` under the
   first `##` date heading. Note its date, its title (which says whether the
   work was committed and which D-IDs it touched), and its **Open threads** (in
   older entries, the **Next:** line).
2. Establish what happened since then. Most work here is **uncommitted**: the
   owner commits, rarely and only when asked. So:
   - `git status --short` and `git diff --stat` for uncommitted changes to
     tracked files. Read the full diff where it matters. Untracked files show
     only in `git status`; read the new ones.
   - `git log --oneline --since="<entry date>"` for any commits since.
   - Data work leaves no trace in git. For download or pipeline runs, take the
     facts from the outputs: the Comtrade manifests (`COMTRADE_MANIFEST` and
     `COMTRADE_WORLD_MANIFEST` in `config.R`), the run logs, and file times.
     For the DuckDB, query a scratch copy in read-only mode, never the live
     file.
   - Combine this with the conversation. Prefer the files to memory: the
     session may already have been compacted, and the diff and manifests are
     ground truth.
3. If the newest entry is today's and covers the same task (say, Day 2 of a
   download that Day 1's entry started), update that entry in place. Do not add
   a near-duplicate. Otherwise write a new entry.

## Write the LOG entry

Put it directly under today's `## YYYY-MM-DD` heading, above any entries
already there. If today has no heading yet, add one at the top, just below the
`---` that follows the file header.

```
### <Task name> (<uncommitted | committed in abc1234>; D-0xx, D-0yy)

**Task:** one line: what this unit of work was aimed at.

**What changed**
- Concrete changes: files, functions, tables, data runs.

**Why / decisions**
- ...

**What failed**
- ...

**Open threads**
- ...

**Notes for picking this up**
- ...
```

- **What changed:** name the key files and functions. For data and model work,
  give the numbers: rows, months covered, calls, test counts, a small table.
  The owner wants evidence for every claim about the data or the models, not
  adjectives.
- **Why / decisions:** the reasons behind non-obvious choices, and the
  constraints and trade-offs that shaped them. Keep what was *decided* apart
  from what was only *suggested or explored*. Do not promote a suggestion the
  owner reacted well to into a decision. Point to the D-ID wherever one
  applies.
- **What failed:** each approach tried and rejected, with its reason, so a
  later session doesn't explore the same dead end again. This is one of the
  most valuable sections; never skip it when there were dead ends.
- **Open threads:** the complete list of what's unfinished. Carry forward
  every thread from the previous entry's Open threads (or **Next:** line) that
  is still open, drop the ones this session closed, and add new ones.
  Background jobs still running and anything waiting on the owner (approvals,
  `.env` changes, commits) belong here. The newest entry's list should be all a
  fresh session needs to know what's pending.
- **Notes for picking this up:** non-obvious gotchas that fit nowhere else.
- Leave out any section with nothing to say, except **Open threads**.

Match the existing entries: short bullets, tables for numbers, plain words.
Write "legacy branding" instead of the former organization's name (D-001), and
never include secrets or anything read from `.env`.

## What not to capture

The test: reading only the code and the commit messages, would a stranger
understand this? If yes, skip it. If no, capture it. Specifically skip:

- Changes self-evident from the diff.
- Decisions already documented in code comments or DECISIONS.md. Reference
  them by ID instead.
- Standard patterns that need no explanation.

Prefer accuracy to completeness. If something is uncertain, say so rather
than invent detail.

## Update DECISIONS.md

- When the owner explicitly agreed to something new, add an entry at the
  bottom with the next free ID and the usual fields: **Date**, **Status**,
  **Context**, **Decision**, **Consequences**.
- When something was recommended but not agreed, record it as **Proposed**.
- When the session carried out, changed or replaced an existing decision,
  edit that entry's **Status** line. Examples: "Implemented in `R/x.R`, not yet
  committed"; "Superseded by D-0xx".

## Update CLAUDE.md

Edit in place, and only if the session changed something it describes as
current: a path, a script or how to run it, a convention, a working rule, or a
gotcha every session needs. History stays in LOG.md. Keep CLAUDE.md short.

## Before writing

Show the owner the drafted LOG entry and any DECISIONS.md or CLAUDE.md edits
for a quick review, and apply their changes. You remember more of the session
than they want to reconstruct, so they should be editing a draft, not
rebuilding it.

## After writing

Report the LOG entry's heading, the D-IDs added or updated, and any CLAUDE.md
lines changed. Do not commit: leave everything in the working tree for the
owner. Do not run `/compact`; leave that to the owner.
