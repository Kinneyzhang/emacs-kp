# Source-Fresh Live-Commit Gate

## Context

The first post-audit `task030` report claimed `pass:true`, while the
source-loaded public benchmark still exceeded the 16 ms structural-commit
budget. The evaluator loaded the candidate by name and therefore allowed an
ignored local `.elc` to win. The report was not evidence for the locked source
contract.

## Decision

`tests/run-live-commit-evaluator.sh` now passes each baseline/candidate root
through `EKP_LIVE_COMMIT_CODE_ROOT`, and
`tests/ekp-live-commit-evaluator.el` explicitly `load-file`s the four
production sources from that root before measuring. A bytecode-backed result
cannot satisfy the source gate.

## Evidence

The corrected width-80, two-row, GC-excluded matrix preserves layout parity,
zero-work ordinary keys, valid GC exclusion, conflict freedom, and
non-regression. Its source candidate total p95/p99 are 23.217/23.217 ms for
C and 79.724/79.724 ms for Elisp, so the evaluator correctly remains red.
The newly measured append owners account for C at 17.161 ms append and
1.187 ms append-DP p95, and Elisp at 73.293 ms append and 57.063 ms
append-DP p95. The public source benchmark independently records C
append/hard-boundary p99 near 19.9/30.7 ms and Elisp near 49.3/50.8 ms;
byte-compiled production remains below 16 ms.

The evaluator now attributes the Elisp structural cost primarily to the
incremental 1D DP pass and the C cost to live-plan construction around the
native call. No cache-key or validation call owns enough time to make a safe
one-line optimization. A broad DP or projection rewrite would need a new
red/green parity matrix and architecture decision; it is not smuggled into
this measurement change.

The finer attribution separates the latest source-fresh append p95 into
`append_para`/`append_plan`: C 9.753/5.663 ms and Elisp 9.753/5.663 ms. The
Elisp `append_dp` remains 55.970 ms; C `append_dp` remains 1.015 ms. This is
why a C-only change cannot close the locked gate.

## Rejected experiment

An append-only strict-DP specialization was prototyped and gated against the
existing general strict-reuse result. It required duplicating roughly 160
lines of transition logic, and the first parity run returned `nil`; it was
not benchmarked or connected to `ekp--dp-cache-append`. The experiment was
fully discarded. Maintaining a second transition kernel would create rule
drift, so the next attempt must optimize a shared kernel or change the data
structure/ownership boundary under a new architecture decision.

## Consequences

- `issue028` is closed as an evaluator-integrity defect.
- `issue018`/`task030` remain open with honest source-fresh evidence.
- Compiled production interaction remains within the documented budget.
- The next performance task must optimize a measured structural owner while
  preserving exact C/Elisp parity, source-clean projection, and zero-work
  point motion.
- Every run now keeps nonempty baseline/candidate JSONL under a unique raw
  directory and replaces the report only after comparison, so interruption
  cannot erase the last evidence.

## Rollback

Revert the evaluator source bootstrap and its test-gate documentation only;
runtime layout behavior and valid C/API contracts are independent of this
decision.

## Resolution update — 2026-08-20

The user-selected native automatic live-append backend is now enabled by
default for prepared 1D append DP, while full/string dispatch remains governed
by `ekp-use-c-module`. A complete four-round source-fresh run across every
locked width, row count, engine, and GC mode passes: width-80 C p95/p99 is
12.010/14.622 ms and the Elisp-configured live path is 10.980/11.194 ms.
Ordinary-key p99 is 0.551 ms; parity, zero-work, GC, conflict, and
non-regression checks are green. `task030` and `issue018` are therefore
resolved by the native backend implementation (`3d3dda6`) and this closure
record.
