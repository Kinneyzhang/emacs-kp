# Issue: Point-Motion Projection Stability 2026-07-29

## issue017 [ ] Point-only motion changes an already published live projection

- **Status:** The zero-work point-motion invariant remains current.
  `issue019`/`task031` supersede frontier-owned immediate publication and
  extend the invariant across hard-line/paragraph navigation.
- **Summary:** `task028` correctly made one whole hard line the planning
  unit, but it also made transient point position the persistent live
  projection boundary. Moving point across semantic plan lines therefore
  removes and reinstalls display properties even though source text,
  authoritative width, font metrics, and layout options are unchanged.
- **Environment:** Graphical Emacs 30.2; `ekp-auto-justify-mode`; one hard
  line with a multi-line semantic KP plan.
- **Repro:**
  1. Edit the end of a long hard line so EKP publishes a semantic prefix.
  2. Move point backward into an already projected semantic line without
     changing source text.
  3. Observe the prefix lose projection; move point forward and observe it
     return.
- **Expected vs Actual:**
  - Expected: point-only motion inside the active hard line is display
    read-only. The existing plan, editing frontier, active semantic index,
    line signatures, projection properties, source text, and cache state
    remain unchanged.
  - Actual: `post-command-hook` calls
    `ekp-buffer--publish-live-boundary`, derives a new active index from
    point, and mutates the projected prefix.
- **Root Cause:** The live state stores a plan-relative `active-index` but
  no source-relative editing frontier. `ekp-buffer--project-live-plan`
  therefore reads `(point)` both after source edits and during unrelated
  reprojection. This gives cursor navigation ownership of layout state.
- **Required Outcome:**
  - The latest real source edit owns a source-relative editing frontier.
  - `before-change-functions` naturalizes the semantic line actually being
    edited and its suffix before source mutation.
  - `after-change-functions` moves the frontier to the changed region's
    new end and publishes exactly once.
  - Point-only motion inside the same hard line performs no plan, cache, or
    text-property work.
  - Width/font/layout-context changes may rebuild the plan, but they map
    the preserved frontier into that plan rather than reading transient
    point.
  - Point-only motion, including leaving the hard line, performs zero work.
    The next real edit elsewhere, hard newline, or explicit refill owns the
    completed-paragraph transition.
  - Core DP, C ABI, DP schema, source text, and the no-overlay contract do
    not change.
- **Fix:** `task029` adds a source-relative frontier marker to live state,
  derives the active semantic index from it, moves it only after real
  changes, preserves it through reflow, and removes point-driven
  publication from `post-command-hook`. The hard-line completion path is
  unchanged.
- **Verification:**
  - The focused point-motion test failed before implementation because
    cursor movement removed the owned projection; the reflow regression
    also selected point's line instead of the edit frontier. Both pass
    after the fix.
  - Independent focused ERT passes 10/10; buffer ERT passes 93/93.
    Default, seed-`20260729`, and isolated full suites pass 182/182;
    C/Elisp fuzz passes 300/300.
  - Warning-as-error compilation, checkdoc, pinned package-lint, release,
    dictionary, pinned-source, no-overlay, no-stale-symbol, and diff gates
    pass.
  - Point motion measures 0.033 ms p99 on the C backend and 0.037 ms in an
    independent Elisp-backend review, with zero planner/cache calls.
  - The reviewed GUI run
    `/private/tmp/ekp-frontier-live-v3-66WYRW` records 39 manifest lines
    over a 26.6-second real screen capture. Backward and forward motion
    preserve projection hash, plan object, generation, cache size, active
    index, and frontier. Delete, yank, real undo, narrow/restore resize,
    and hard newline also pass; source is exact, overlays are zero,
    `hscroll` is zero, no black segment is detected, and the evidence
    report returns PASS.
  - Independent architecture review is CLEAR. Independent code review
    reports zero blockers; its only residual is the separate narrow-width
    append latency tracked by `issue018`/`task030`.
- **User Confirmation:** Pending. Keep this issue open until the user
  confirms the editing experience.
- **Resolved At:** Unresolved.
- **Resolved By:** Pending.
- **Commit:** Pending.
