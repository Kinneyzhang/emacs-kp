# Issue: Native Soft Wrap 2026-07-29

## issue015 [x] Narrow split windows truncate live text

- **Status:** Closed by user re-audit on 2026-08-13. Implementation and
  independent verification were complete.
- **Summary:** With `ekp-auto-justify-mode` enabled in a narrow
  side-by-side window, typing past the right edge horizontally scrolls the
  line and displays a `$` truncation indicator instead of naturally
  soft-wrapping.
- **Environment:** Graphical Emacs 30.2; side-by-side windows; native
  progressive live layout.
- **Repro:**
  1. Split a graphical frame side by side until the editing window is
     narrower than 50 columns.
  2. Enable `ekp-auto-justify-mode`.
  3. Type a hard line beyond the right edge.
  4. Observe horizontal scrolling and the left `$` indicator.
- **Expected vs Actual:**
  - Expected: the active hard line follows native soft wrapping, point
    moves onto a new visual row, and EKP may align only the row just left.
  - Actual: no visual row is created, so native-row publication cannot
    begin.
- **Root Cause:** Emacs 30.2 defaults
  `truncate-partial-width-windows` to `50`. In narrower side-by-side
  windows that setting overrides `truncate-lines=nil` and enables
  truncation. The auto mode declared native soft wrap as an algorithmic
  precondition but did not own the corresponding display variables.
- **Required Outcome:** While the mode is enabled, force native soft
  wrapping in full-width and partial-width windows. On disable or
  major-mode change, restore both the prior values and whether each value
  was buffer-local.
- **Fix:** `task027` snapshots both truncation variables and their
  buffer-local ownership before enabling native wrapping. Normal disable,
  major-mode teardown, and failed activation restore that state exactly;
  activation errors remain visible after cleanup instead of leaving a
  half-enabled mode.
- **Verification:** The lifecycle regressions pass 3/3; buffer ERT passes
  77/77; default, seed-`20260729`, and isolated ERT pass 166/166; C/Elisp
  fuzz passes 300/300. Warning-as-error compilation, checkdoc, pinned
  package-lint, release, dictionary, pinned-source, and static gates pass.
  Retained dynamic evidence in
  `/private/tmp/ekp-soft-wrap-final-pass-PIUigY` shows per-character typing
  naturally advancing from one to two visual rows in a 44-column split,
  with `hscroll=0`, exact source text, zero overlays, and zero live
  replacing breaks.
- **User Confirmation:** Provided by user re-audit on 2026-08-13.
- **Resolved At:** 2026-08-13.
- **Resolved By:** User re-audit closure; implementation complete.
- **Commit:** — (documentation-only closure).
