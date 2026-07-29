# Let Live Editing Consume the Whole Hard-Line Plan

> Superseded note (2026-07-29): the whole-hard-line planning decision
> remains current, but `20260729-editing-frontier-not-point.md` supersedes
> this record's point-driven projection-boundary statements.
>
> Later supersession (2026-07-29):
> `20260729-stable-live-transaction.md` preserves whole-hard-line planning at
> structural commits but supersedes replanning after every source edit.

## Context

The native progressive live model fixed the most visible editing problems:
typing before native wrap stayed natural, narrow split windows soft-wrapped,
and live mode no longer published discretionary hyphens or replacing
breaks into the active tail.

User testing then exposed the next owner mistake. A hard line that naturally
wraps into several visual rows still needs the rows before point to be
decisions from one Knuth-Plass plan. The native-row model aligned rows after
Emacs had already wrapped them, then treated those row boundaries as local
facts. That cannot produce global KP behavior because later source text can
change earlier optimal breaks and glue.

The failed assumption was subtle: "native redisplay owns the active edit
tail" does not imply "native rows are the durable planning unit." Native
redisplay should still own the point-containing unfinished area, but the
completed prefix should be projected from the semantic KP plan for the
whole current hard line.

## Decision

Keep the core algorithm unchanged. `ekp-layout-plan` already computes the
correct plan for complete text. `ekp-buffer` must consume that plan for the
current hard line and choose the editable projection boundary.

The live pipeline is:

1. Read the complete source text of the current hard line.
2. Call the existing `ekp-layout-plan` with the authoritative width and
   current layout context.
3. Find the semantic plan line that contains point.
4. Project only complete plan lines before that line.
5. Leave the point-containing plan line and every later source character
   completely natural.
6. Compare old and new semantic line signatures so unchanged prefixes stay
   installed and changed suffixes are removed/reinstalled atomically.

This state belongs entirely to `ekp-buffer`: hard-line bounds, source and
context signatures, the latest plan, line signatures, installed prefix
range, generation, and a small buffer-local history cache for undo/redo and
width/text round trips.

## Boundaries

- Do not change core DP semantics.
- Do not add continuation DP, live DP, or final-line special cases.
- Do not change the C ABI, DP schema, or core/Elisp algorithm contract.
- Do not copy the DP into `ekp-buffer`.
- Do not add buffer, point, window, marker, or redisplay state to the core
  planner.
- Do not use overlays.
- Do not insert source spaces, source newlines, or source hyphen
  characters.

The completed-paragraph planner remains the owner of optimal breaks and
glue. The buffer layer owns edit-time projection, invalidation, and
redisplay safety.

## Superseded Model

`20260729-native-progressive-live-editing.md` remains useful history for
two decisions that still stand:

- the point-containing active area must remain natural;
- full static KP projection is allowed after hard-paragraph completion.

It is superseded for the planning unit. Completed native visual rows are
not stable committed rows. They are only a symptom that there is now a
semantic prefix before point that may be safe to project.

## Consequences

- Earlier displayed rows in the same hard line can change together when
  later input changes the optimal KP plan.
- Moving point into an earlier semantic line immediately makes that line
  and the following source natural again.
- Point movement without text change still matters because it changes the
  projection boundary.
- Plan cache hits are possible when undo/redo, deletion, width changes, or
  point movement revisit a known hard-line text/context signature.
- Projection failure must not advance a tail marker or leave half of a KP
  prefix installed; the affected hard line returns to native display.
- First-line underfilled editing remains indistinguishable from ordinary
  Emacs because there is no complete semantic prefix before point.

## Verification

The implementation proves the behavior through public editing paths, not
by stubbing row lists:

- `self-insert-command`, yank, delete, real undo/redo, point movement,
  hard newline, resize, and major-mode change;
- point in last, middle, and first semantic plan line;
- early plan breaks changing after later edits, proving rows are not frozen;
- plan-cache hits and zero property writes when the semantic plan is
  unchanged;
- clean GUI dynamic evidence with exact source text, zero overlays,
  `hscroll=0`, natural point line, aligned prefix lines, and no delayed
  idle snap.

## Implementation Outcome

`task028` implemented this decision entirely in `ekp-buffer`. The core DP,
C ABI, DP schema, and `ekp-layout-plan` contract were not changed. The live
state now uses whole-hard-line plans, a 16-entry buffer-local history LRU,
semantic line signatures, common-prefix differential publication, and
transactional fail-closed cleanup.

Default and seed-`20260729` ERT pass 181/181, all 181 tests pass in isolated
Emacs processes, and C/Elisp fuzz passes 300/300. The reviewed main dynamic
GUI run `/private/tmp/ekp-semantic-live-v4-vFZTkr` and 44-column split run
`/private/tmp/ekp-semantic-split-v3-uPwuOi` both return PASS with exact
source text, zero overlays, `hscroll=0`, a natural point line, and no
temporal snap or stale projection.

The original C-backend run recorded append p99 6.399 ms with GC excluded.
A later task029 audit could not reproduce that number with the current
checked-in benchmark: the synthetic 80-pixel workload instead records one
plan per unique append and roughly 33–85 ms p99. That separate performance
debt is now `issue018`/`task030`; see
`20260729-narrow-live-append-replanning.md`. The implementation does not
hide it with stale reuse, debounce, skipped publication, global GC changes,
or a timer workaround.

Final independent code review returns APPROVE and independent architecture
review returns CLEAR, including the theme-disable and frame-font
invalidation delta.

Developer verification does not close `issue016`; the user must still
confirm the editing experience personally.

## Rollback

Revert the semantic-prefix live implementation as one unit if it fails its
public-path or GUI gates. Do not restore native-row freezing as the final
model; it is known to be the wrong planning boundary for global KP layout.
