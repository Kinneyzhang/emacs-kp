# Issue: Narrow Live-Append Latency 2026-07-29

## issue018 [ ] Unique live appends miss the frame budget at very narrow widths

- **Status:** Native live-append backend implemented and verified; the locked
  source-fresh gate remains open for preparation/assembly latency.
- **Summary:** Stable transactions removed per-key whole-hard-line planning,
  and task030 incrementally extends paragraph preparation, DP state, and
  layout lines only at structural row crossings. The user-selected
  `ekp-auto-justify-native-append` backend now uses the loaded C 1D DP for
  prepared live appends even when full layout is configured for Elisp.
  Byte-compiled production paths are within the 16 ms frame budget; the
  source-fresh evaluator still exceeds it in Elisp-owned preparation and plan
  assembly.
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
  - Actual locked evaluator: four interleaved source-instrumented rounds
    measure C at 25.490/25.785 ms p95/p99 and Elisp at
    43.860/47.578 ms. These improve 77.78/78.37% and 92.54/92.03% over the
    frozen baseline but still miss the absolute 16 ms stress target.
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
  - The frozen C wrapper measured only 2.615/2.655 ms p95/p99 and the
    candidate 0.697/0.701 ms. Paragraph preparation, DP state reconstruction,
    plan construction, and publication owned the end-to-end cost.
  - Byte compilation removes most of the remaining pure-Elisp interpreter
    and closure overhead. This explains why the production path passes while
    the frozen source-instrumented stress gate remains red; neither result is
    substituted for the other.
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
  all-width, 2/4/8/16-row, GC-excluded round, width-80 candidate append-DP
  p95/p99 was 2.051/3.127 ms for C and 2.495/3.406 ms for the Elisp-configured
  live path. Total p95/p99 was 14.190/16.495 ms and 13.484/14.334 ms,
  respectively; preparation and assembly remain the active budget owners and
  the C p99 is still just above 16 ms.
- **Verification:** Exact baseline/C/Elisp hashes and append-chain
  equivalence pass across 64/80/96/128/160 px, 2/4/8/16-row fixtures,
  default/excluded GC, unsafe fallbacks, and randomized chains. Current
  source-first normal and random-order ERT pass 296/296; the native bridge
  parity regression passes 1/1; 300 fuzz cases, warning-clean production/C
  compilation, C contract tests, release/dictionary checks, and reviewed
  temporal GUI evidence pass. The bounded source-fresh performance round
  remains red only on the C p99 absolute target.
- **User Confirmation:** Pending; the formal source-instrumented performance
  target also remains open.
- **Resolved At:** Unresolved.
- **Resolved By:** Pending.
- **Commit:** `3d3dda6` (native backend); performance gate remains open.
