# Issue: C Resize Latency 2026-07-29

## issue020 [x] C-backed resize reflow still takes roughly 60–70 ms

- **Status:** Closed by user re-audit on 2026-08-13. Developer verification
  was complete.
- **Summary:** Resizing with the C dynamic module enabled was observed at
  roughly 60–70 ms. That number covered more than the C algorithm: the
  buffer path also rebuilt layout input, crossed the Emacs-module boundary,
  reconstructed layout data, and published text properties.
- **Environment:** Graphical Emacs 30.2 on Apple Silicon; C module 1.6;
  portable production profile unless a benchmark explicitly labels another
  profile.
- **Repro:**
  1. Load the C module and a representative mixed Latin/CJK buffer.
  2. Reflow the same source through a deterministic sequence of uncached
     widths.
  3. Measure complete reflow and its plan, module, DP, and publication
     layers with GC pauses reported separately.
- **Expected vs Actual:**
  - Expected: candidate p95 is at most 50 ms and both p50 and p95 improve by
    at least 20% against the frozen same-machine baseline.
  - Actual: the final four-round evaluator records core p50/p95 of
    15.318/27.687 ms and complete resize p50/p95 of 15.900/27.487 ms.
    Against the frozen baseline this is a 33.25%/34.09% core improvement
    and a 43.51%/41.03% complete-resize improvement.
- **Investigation:** The portable C call averaged roughly 1.8 ms and was
  not the dominant layer. Repeated paragraph resolution, projection-time
  gap measurement, no-op gap records, duplicate property publication, and
  publishing the active paragraph once statically and again as the live
  prefix owned most avoidable work. A real GUI resize can still include an
  Emacs GC pause after `set-window-margins`; that pause is reported
  separately instead of being hidden by a global threshold change.
- **Required Outcome:** Reduce the real owning layer while preserving exact
  frozen-C and Elisp layout parity, exact cache identity, stable live
  transactions, and source-clean projection.
- **Forbidden Shortcuts:** Longer debounce, skipped intermediate widths,
  stale or approximate plan reuse, asynchronous stale publication, and
  global GC changes.
- **Fix:** Resolve each paragraph once per plan, carry prepared DP/gap
  geometry through projection, omit true zero-source/zero-target gaps,
  publish owned properties atomically, and exclude the active paragraph
  from the static resize pass before installing its live prefix.
- **Verification:** Four interleaved frozen-baseline/candidate rounds pass
  the core and complete-resize gates with exact frozen-C/Elisp layout
  parity. The normal and permuted 193-test suites pass, 300 fuzz cases pass,
  warning-as-error byte compilation, package-lint, checkdoc, release checks,
  and the isolated-test run pass. Reviewed GUI evidence at
  `/private/tmp/ekp-c-resize-gui-final6-FMFLxx` records four real width
  changes, 14 checkpoints, no failed assertion or pixel overflow, and a
  maximum EKP mutator time of 22.989 ms; one 74.099 ms total sample contains
  58.792 ms of Emacs GC and 15.307 ms of EKP work.
- **User Confirmation:** Provided by user re-audit on 2026-08-13.
- **Resolved At:** 2026-08-13.
- **Resolved By:** User re-audit closure; developer verification complete.
- **Commit:** — (documentation-only closure).
