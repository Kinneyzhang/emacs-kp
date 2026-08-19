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
non-regression. Its source candidate p95/p99 are 23.294 ms for C and 78.593
ms for Elisp, so the evaluator correctly remains red. The public source
benchmark independently records C append/hard-boundary p99 near 19.9/30.7 ms
and Elisp near 49.3/50.8 ms; byte-compiled production remains below 16 ms.

Profiling attributes the Elisp structural cost primarily to the incremental
1D DP pass and the C cost to live-plan construction/publication around the
native call. No single cache-key or validation call owns enough time to make
a safe one-line optimization. A broad DP or projection rewrite would need a
new red/green parity matrix and architecture decision; it is not smuggled into
this correctness hardening change.

## Consequences

- `issue028` is closed as an evaluator-integrity defect.
- `issue018`/`task030` remain open with honest source-fresh evidence.
- Compiled production interaction remains within the documented budget.
- The next performance task must optimize a measured structural owner while
  preserving exact C/Elisp parity, source-clean projection, and zero-work
  point motion.

## Rollback

Revert the evaluator source bootstrap and its test-gate documentation only;
runtime layout behavior and valid C/API contracts are independent of this
decision.
