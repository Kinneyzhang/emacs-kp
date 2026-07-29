# Separate Stable Editing From Global KP Commitment

## Context

`task028` fixed the planning unit by consuming one whole-hard-line KP plan.
`task029` then removed point-only invalidation by preserving a source-edit
frontier. User testing exposed that the combined model still gives the
frontier three jobs: latest edit position, natural-suffix boundary, and
projection reuse boundary.

That ownership cannot deliver stable editing. Replanning the complete hard
line after every key permits the globally optimal breakpoint set to change
after every key. Line-signature diffing can reduce property writes only after
those new decisions exist; it cannot make the decisions stable. Clearing the
touched line and its entire suffix before every edit also destroys anchors
that were not causally affected.

The narrow unique-append cost recorded by `issue018` is therefore downstream
of the correctness model, not the first problem to optimize.

## Decision

Use two edit-time layers and one completed-layout owner:

1. A committed projection owns the last published source, core plan, line
   signatures, projected spans, and break anchors.
2. A dirty edit transaction snapshots that complete baseline and
   naturalizes only the affected local island.
3. The existing core DP remains the sole owner of globally optimal committed
   and completed layout.

Ordinary input inside the same native visual row does not run whole-hard-line
DP and does not rewrite unaffected properties. Existing glue and native soft
wrapping absorb small local changes. If the dirty row no longer fits, native
wrapping moves the necessary local words while following committed break
anchors remain fixed.

A transaction commits only when:

- input crosses a native soft-wrap boundary;
- a hard newline/paragraph end is inserted or removed;
- the next real source edit occurs outside the dirty island;
- the user explicitly refills the paragraph; or
- width, font, or layout context changes.

Point motion is never a commit event. On a soft-wrap commit, the complete
hard-line source may be replanned once and the completed prefix is updated as
one silent publication; the new current row remains natural. On hard
paragraph completion, the existing full-quality KP path remains unchanged.

If an edit restores the baseline source, the transaction restores the saved
owned projection and committed state directly. Exact restoration is a state
transition, not a hope that recomputation happens to reproduce equivalent
properties.

## Superseded Decisions

This record supersedes:

- `20260729-whole-hard-line-live-prefix.md` where it permits earlier
  projected rows to change after every source edit;
- `20260729-editing-frontier-not-point.md` where the edit frontier also owns
  the natural suffix and immediate post-edit publication.

It preserves their valid decisions:

- the core DP and layout-plan contract stay unchanged;
- committed prefix lines come from one semantic hard-line plan;
- the current unfinished row is natural;
- point-only motion performs zero work;
- hard-paragraph completion receives full-quality KP layout.

It also restores the useful local-stability principle from the earlier
bounded-flow work without restoring its core continuation API, helper stack,
or per-key DP control flow.

## Consequences

- Editing stability and final optimality have different, explicit triggers.
- Unaffected visual rows retain their break anchors during continuous local
  editing.
- Reversible source edits restore the exact projection immediately.
- Per-key unique-state planning disappears from ordinary same-row append; the
  remaining `task030` surface must be measured after this change.
- The live buffer layer owns more baseline state, but the core algorithm,
  source-clean representation, and projection primitives remain unchanged.

## Verification

Public hook/command paths must prove ordinary same-row no-plan behavior,
local dirty-island ownership, unaffected-anchor stability, exact
`equal-including-properties` reversal, soft-wrap atomic commit, hard
completion, next-edit-elsewhere commit, zero-work point motion, IME
deferral, width/font/context commit, and fail-closed conflicts. Dynamic GUI
evidence is required because batch tests cannot prove temporal stability or
native word migration.

## Outcome

The implementation confirmed the ownership split. Ordinary same-row edits
and point motion now make zero planner calls; the 291-edit narrow benchmark
contains 15 plans, each tied to a real structural crossing. Middle-row edits
retain unrelated span objects, and exact source reversal restores the saved
projection `equal-including-properties` with the original plan, signatures,
and spans.

The final 48.95-second GUI run records 11 public actions at five phases each.
All 55 checkpoints pass, the run completes in one fullscreen target window,
and temporal review finds no black, blank, split, stale-buffer, or client-
message frame. The surviving structural-commit latency is correctly left to
`task030`; it is no longer evidence against the transaction model.

## Rollback

Revert the transaction change as one unit if the public-path or GUI gates
fail. Do not fall back to debounce, idle whole-paragraph formatting, or the
per-key frontier planner; all three retain the same trigger-ownership error.
