# Keep Static Edge Policy out of Live Editor State

## Context

The source-clean renderer had two interaction regressions after its initial
quality gate.

First, every reprojection copied the old mark position and restored it with
`set-mark`. Emacs treats mark position and `mark-active` as separate state,
but `set-mark` changes both. A showcase width key therefore turned an old
inactive mark into a highlighted region.

Second, the live renderer correctly skipped glue justification on the
point-containing line, then reused static paragraph-edge cleanup. The KP
plan excludes leading/trailing whitespace from its content bounds, so that
cleanup assigned `display ""` to a newly typed edge space. The source
character existed immediately, but the user saw it only after typing the
next glyph.

## Decision

Editor state is restored by its actual owners:

- set the existing mark marker's position directly, then restore
  `mark-active` independently;
- retain static edge hiding for committed lines;
- never hide source-edge whitespace on the active live line;
- when the first planned line becomes active, clear projection from the
  span's source start so previously hidden leading whitespace becomes
  natural too.

No showcase-specific selection workaround and no space self-insert advice
is added. Both symptoms are fixed in `ekp-buffer.el`, where reprojection and
live-line presentation are owned.

## Why the Initial Tests Missed It

The live typing test inserted `" x"` as one operation. By assertion time,
the space was interior and no longer eligible for edge cleanup. The GUI
matrix also tested an intentionally active region, but not an existing
inactive mark.

The replacement gates stop at the temporal boundary:

- inspect state immediately after one space, before another glyph;
- cover leading/trailing space, tab, CJK, deletion-exposed whitespace,
  consecutive spaces, newline, yank, and undo;
- test inactive and active mark states separately;
- record the real showcase width command and command-loop space insertion
  in a clean full-screen GUI run.

## Consequences

One space now moves point/cursor immediately and carries no EKP replacing
display. Backspace cannot re-hide the exposed whitespace. Width changes
preserve point, mark position, and region activation exactly.

Committed static paragraphs still hide stripped edge whitespace, so this
correction does not change completed KP layout or the source-clean
representation contract.
