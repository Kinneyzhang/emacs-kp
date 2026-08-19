# Issue: Live Evaluator Source Freshness 2026-08-20

## issue028 [ ] Locked live evaluator can measure stale candidate bytecode

- **Status:** Closed by source-fresh evaluator verification on 2026-08-20;
  performance gate remains open under the now-correct source measurement.
- **Summary:** `run-live-commit-evaluator.sh` loaded the candidate by name and
  allowed an ignored local `.elc` to win. A report could therefore claim the
  locked source-instrumented 16 ms gate passed while measuring bytecode.
- **Expected vs Actual:** Every baseline and candidate round must explicitly
  `load-file` the four production source files for its code root. Before the
  bootstrap fix, a narrowed matrix reported `pass:true`; afterward it reports
  source candidate p99 above 16 ms and keeps `issue018` open.
- **Related:** `tests/run-live-commit-evaluator.sh`,
  `tests/ekp-live-commit-evaluator.el`, `issue018`, `task030`.
- **Verification:** Source-load bootstrap, narrowed matrix, and source-path
  live benchmark are complete; full locked matrix remains the task030 gate.
- **Resolved At:** 2026-08-20.
- **Resolved By:** Developer evaluator verification.
- **Commit:** `6a8c7e0`.
