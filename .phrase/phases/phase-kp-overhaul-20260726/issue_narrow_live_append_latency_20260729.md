# Issue: Narrow Live-Append Latency 2026-07-29

## issue018 [x] Unique live appends miss the frame budget at very narrow widths

- **Status:** Resolved. Native live-append backend and the locked source-fresh
  performance gate are verified.
- **Summary:** Stable transactions removed per-key whole-hard-line planning,
  and task030 incrementally extends paragraph preparation, DP state, and
  layout lines only at structural row crossings. The user-selected
  `ekp-auto-justify-native-append` backend now uses the loaded C 1D DP for
  prepared live appends even when full layout is configured for Elisp.
  Byte-compiled production paths and the source-fresh evaluator are within the
  16 ms frame budget.
- **Environment:** Emacs 30.2 on macOS, C backend 1.6, benchmark width
  fixed at 80 px, `gc-cons-threshold` bound to
  `most-positive-fixnum` so mutator work is measured without GC pauses.
- **Repro:**
  1. For the production-shaped check, byte-compile the four production
     Elisp files into a temporary package root, put that root first on
     `load-path`, and run `tests/ekp-buffer-live-bench.el` once with the C
     module loaded and once with `ekp-use-c-module` nil.
  2. Confirm `ekp--dp-run-1d` is byte code and inspect the `append` and
     `hard-boundary` rows.
  3. For the locked source/instrumentation matrix, run
     `tests/run-live-commit-evaluator.sh` and inspect
     `.omx/goals/performance/narrow-live-commit/latest-report.json`.
- **Expected vs Actual:**
  - Expected: live append p99 remains below the 16 ms interaction budget,
    including narrow windows.
  - Actual production path: three repeated byte-compiled public-command runs
    measure append p99 at 1.158–1.326 ms for C and 1.429–1.438 ms for pure
    Elisp. Hard-boundary p99 is 1.251–1.363 ms and 1.457–1.470 ms.
  - Actual locked evaluator (2026-08-20): four interleaved source-fresh rounds
    across 64/80/96/128/160 px, 2/4/8/16 rows, both engines, and both GC
    modes measure width-80 C at 12.010/14.622 ms p95/p99 and the
    Elisp-configured live path at 10.980/11.194 ms. Paired improvements are
    77.72/74.03% and 94.19/94.57%, respectively.
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
  - Before the native backend, both backends missed p99 at structural
    boundaries, so task030 evaluated both rather than optimizing only the C
    wrapper.
  - The frozen C wrapper measured only 2.615/2.655 ms p95/p99 and the
    candidate 0.697/0.701 ms. Paragraph preparation, DP state reconstruction,
    plan construction, and publication owned the end-to-end cost.
  - Byte compilation removes most of the pure-Elisp interpreter and closure
    overhead. The final source-fresh gate is now independently green; the
    production result is retained as a separate deployment check.
  - Rust is not selected. It would use the same `emacs_env` ABI and cannot
    remove Elisp-owned font measurement, transaction, or projection work.
- **Required Outcome:**
  - Diagnose and reduce unique-state narrow-width append planning cost
    without changing KP output semantics, the core DP contract, C ABI, or
    source-clean display model.
  - Keep cache identity exact; do not introduce approximate/stale plan
    reuse, timers, skipped edits, global GC changes, or point-based
    invalidation.
  - Preserve the task029 invariant that point-only motion performs zero
    plan, cache, and text-property work.
- **Fix:** `task030` retains exact prepared paragraph/context data, extends
  property-free append tails from the last complete-word boundary, resumes
  pure-Elisp DP from a safe reachable state, reuses common layout lines, and
  reconstructs only the live dirty source island. Unsupported contexts take
  the unchanged full path. C int32 validation now performs one extraction.
- **Native backend update (2026-08-20):** `ekp-auto-justify-native-append`
  now routes prepared 1D live append DP through the loaded C module even when
  full layout uses Elisp. The option can be disabled for a pure-Elisp live
  path, and unavailable modules fall back exactly. In a bounded source-fresh
  all-width, 2/4/8/16-row, GC-excluded round first reduced append-DP and left
  a noisy C p99 near the target. The complete four-round locked run then
  measured width-80 total p95/p99 at 12.010/14.622 ms for C and
  10.980/11.194 ms for the Elisp-configured live path, closing the gate.
- **Verification:** Exact baseline/C/Elisp hashes and append-chain
  equivalence pass across 64/80/96/128/160 px, 2/4/8/16-row fixtures,
  default/excluded GC, unsafe fallbacks, and randomized chains. Current
  source-first normal and random-order ERT pass 296/296; the native bridge
  parity regression passes 1/1; 300 fuzz cases, warning-clean production/C
  compilation, C contract tests, release/dictionary checks, and reviewed
  temporal GUI evidence pass. The complete four-round source-fresh evaluator
  also passes every performance condition.
- **User Confirmation:** User selected architecture option 2 (`2`) and the
  complete source-fresh gate now passes.
- **Resolved At:** 2026-08-20.
- **Resolved By:** Codex.
- **Commit:** `3d3dda6` (native backend), `d216c6b` (phase record), and this
  closure-record commit (complete four-round gate closure).
