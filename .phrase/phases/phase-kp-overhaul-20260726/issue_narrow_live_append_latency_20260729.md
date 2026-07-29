# Issue: Narrow Live-Append Latency 2026-07-29

## issue018 [ ] Unique live appends miss the frame budget at very narrow widths

- **Status:** Re-profiled after `task031`; `task030` is unblocked.
- **Summary:** Stable transactions removed per-key whole-hard-line planning.
  The synthetic 80-pixel workload now performs zero planning on ordinary
  same-row edits, but structural row-crossing commits still make append p99
  exceed the 16 ms frame budget.
- **Environment:** Emacs 30.2 on macOS, C backend 1.6, benchmark width
  fixed at 80 px, `gc-cons-threshold` bound to
  `most-positive-fixnum` so mutator work is measured without GC pauses.
- **Repro:**
  1. Load the C backend.
  2. Run `tests/ekp-buffer-live-bench.el` with GC excluded.
  3. Inspect the `append` row.
- **Expected vs Actual:**
  - Expected: live append p99 remains below the 16 ms interaction budget,
    including narrow windows.
  - Actual: the final task031 GC-excluded runs record only 15 permitted
    structural plans across 291 appends. C measures median 2.177 ms and p99
    51.170 ms; Elisp measures median 2.176 ms and p99 187.499 ms. Same-row
    cache-revisit work records zero plans (C p99 1.627 ms; Elisp p99
    1.502 ms), while point motion records zero plan/cache calls (C p99
    0.017 ms; Elisp p99 0.015 ms).
- **Investigation:**
  - The task029 point-motion change is not the cause. Navigation is now a
    zero-work path and does not enter planning or projection publication.
  - The old workload produced 291 distinct hard-line strings and 291 plan
    calls. `task031` removes that obsolete control flow: only 15 actual
    visual-row crossings invoke `ekp-layout-plan`.
  - The remaining high percentiles coincide with those allowed structural
    commits and must be profiled as commit latency, not average key latency.
  - Wider measurements are materially faster; the pathological result is
    width-sensitive rather than a general point-motion regression.
  - The previously recorded 6.399 ms task028 append p99 is not
    reproducible with the current checked-in benchmark and must not remain
    the current performance claim.
  - Both backends still miss p99 at structural boundaries, so task030 must
    evaluate both rather than optimize only the C wrapper.
- **Required Outcome:**
  - Diagnose and reduce unique-state narrow-width append planning cost
    without changing KP output semantics, the core DP contract, C ABI, or
    source-clean display model.
  - Keep cache identity exact; do not introduce approximate/stale plan
    reuse, timers, skipped edits, global GC changes, or point-based
    invalidation.
  - Preserve the task029 invariant that point-only motion performs zero
    plan, cache, and text-property work.
- **Fix:** Deferred to `task030`; the prerequisite correctness replacement
  is complete. Optimize only the surviving structural-commit path.
- **Verification:** Future work must use a reproducible width matrix,
  profiler evidence, result-equivalence tests, default-GC and GC-excluded
  runs, and GUI input evidence.
- **User Confirmation:** Not applicable until a performance change is
  implemented.
- **Resolved At:** Unresolved.
- **Resolved By:** Pending.
- **Commit:** Pending.
