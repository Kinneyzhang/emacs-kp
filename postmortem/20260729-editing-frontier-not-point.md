# Let Source Edits Own the Live Editing Frontier

> Superseded note (2026-07-29): point-only motion remains a zero-work
> invariant. `20260729-stable-live-transaction.md` replaces the frontier's
> natural-suffix and immediate-publication ownership with a saved committed
> projection and a local dirty edit transaction.

## Context

`task028` fixed the planning boundary: all projected rows in one hard line
now come from one complete `ekp-layout-plan`. It nevertheless used current
point to choose the natural suffix whenever `post-command-hook` ran.

That conflated two independent facts:

- point is transient navigation state;
- the editing frontier records which semantic line is currently unfinished.

As a result, cursor motion alone removed and reinstalled projection
properties. It did no DP work, but it still changed the visual document.
Worse, a later width or font reflow could read the cursor's temporary
position and silently move the natural suffix boundary.

## Decision

The latest real source edit owns the editing frontier.

1. A live hard line keeps a source marker for the changed region's new end.
2. Before a real edit, the existing prefix line containing the edit is
   returned to native display together with its suffix.
3. After the edit, the frontier marker moves to the changed region's new
   end and the whole-hard-line plan is recomputed or reused once.
4. The active semantic index is derived from that marker, never from
   transient point.
5. Point-only motion within the active hard line does nothing.
6. Width, font, and layout-context changes preserve the marker and map it
   into the new plan.
7. Leaving the hard line still completes the old hard line with the
   existing static KP path.

The core DP remains unaware of point, markers, buffers, and editing state.

## Why This Is Simpler

`active-index` is derived plan state; the frontier marker is the one stable
source fact. There is no point-motion state machine and no attempt to infer
editing intent from navigation. One owner event—source mutation—moves the
frontier.

The change deletes the point-boundary publication path instead of adding
another condition to it.

## Superseded Decision

This record supersedes only the point-driven boundary statements in
`20260729-whole-hard-line-live-prefix.md`. That record's main decisions
remain valid:

- one complete hard line is the planning unit;
- all projected prefix lines come from the same core plan;
- the frontier line and suffix stay natural;
- prior breaks may change together after later source edits.

## Consequences

- Moving point cannot cause visual reflow or text-property writes.
- Editing an earlier projected line still makes that actual edit line and
  its suffix natural before mutation, then republishes from the updated
  whole-hard-line plan.
- Resizing can change breaks and glue, but not because point happened to be
  elsewhere inside the hard line.
- The frontier remains display-layer state. Core DP, the C ABI, and the
  source-clean representation are unchanged.

## Verification

The regression gate compares the complete owned-property projection,
frontier marker, active index, signatures, spans, source characters,
modified tick, undo state, plan calls, cache calls, and property writes
before and after backward/forward point motion. It failed before the change
and now passes; reflow and deferred IME completion also preserve the
frontier instead of reading transient point.

Default, seed-`20260729`, and isolated full ERT pass 182/182; buffer ERT
passes 93/93 and C/Elisp fuzz passes 300/300. Point-motion p99 is
0.033 ms on the C backend with zero plan/cache calls.

Reviewed dynamic GUI evidence at
`/private/tmp/ekp-frontier-live-v3-66WYRW` contains a 26.6-second screen
recording and 39 manifest lines. Backward/forward commands preserve the
projection hash, plan, generation, cache size, active index, and frontier
through every required checkpoint. Delete, yank, real undo, resize,
restore, and hard-newline transitions also pass; source text remains exact,
overlays and horizontal scroll remain zero, and the final report is PASS.

Independent architecture review is CLEAR. Independent code review finds
zero task029 blockers; the separate unique-append performance debt is
recorded as `issue018`/`task030`.

## Rollback

Revert this editing-frontier change as one unit if a public editing or GUI
gate fails. Do not restore point-driven projection changes as a fallback;
navigation is not a layout invalidation event.
