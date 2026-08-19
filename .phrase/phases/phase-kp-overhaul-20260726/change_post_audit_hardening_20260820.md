# Change: Post-Audit Hardening 2026-08-20

## 2026-08-20 — Lock follow-up goals and task boundaries

- **Add** `plan_post_audit_hardening_20260820.md` with the evidence-backed
  scope, non-goals, acceptance gates, and stop condition for the post-audit
  defects.
- **Add** `task_post_audit_hardening_20260820.md` with `task037`–`task042`
  and the remaining `task030` performance gate.
- **Add** issue records for each user-visible or public-contract defect before
  implementation, including `issue028` for the stale-bytecode evaluator
  false-green.
- **Behavior/Risk:** documentation-only planning. Closed historical issues
  remain closed; `issue018` remains open until its locked gate is met.

## Verification

- Next task ID is `task037`; no task ID is reused.
- The plan explicitly separates buffer ownership, live semantics, dictionary
  lookup, C validation, test gates, integration lifecycle, and performance.
- No production or test source is changed in this planning pass.

## 2026-08-20 — Implement correctness and gate hardening

- **Modify** `ekp-buffer.el` and `tests/ekp-buffer-tests.el` for preflight
  width validation, backward-delete live stability, and no-projection
  integration cleanup (`task037`, `task038`, `task042`; `issue022`,
  `issue023`, `issue027`).
- **Modify** `ekp-hyphen.el` and `tests/ekp-tests.el` for exact normalized
  locale lookup (`task039`, `issue024`).
- **Modify** `ekp_c/ekp.c` and `tests/ekp-c-tests.el` for sorted, unique,
  in-range break-position validation in direct and batch APIs (`task040`,
  `issue025`), with the 15-field ABI unchanged.
- **Modify** the showcase, source loader, test runners, Windows CI entry,
  and complete ERT selection to remove stale-bytecode false greens and
  restore automatic inline-code verification (`task041`, `issue026`).
- **Modify** the live evaluator script/bootstrap to load source from the
  selected baseline/candidate root (`task030`, `issue028`). The corrected
  narrowed matrix still exceeds the source 16 ms target, so `issue018` and
  `task030` remain open.
- **Commit:** `6a8c7e0`.

## 2026-08-20 — Record source-fresh performance boundary

- **Add** `postmortem/20260820-source-fresh-live-commit-gate.md` to explain
  why the stale-bytecode evaluator result was invalid and why the corrected
  source gate remains an open `task030` performance target.
- **Evidence:** width-80/two-row/GC-excluded source candidate p95/p99 are
  23.294 ms (C) and 78.593 ms (Elisp), with parity, zero-work, GC,
  conflict, and non-regression checks true. The high-row full matrix was
  stopped after a source DP cell exceeded two minutes of CPU; it is not
  claimed as complete evidence.
- **Commit:** `60b299b` and `postmortem` follow-up commit.

## 2026-08-20 — Attribute incremental append cost and preserve raw evidence

- **Modify** `tests/ekp-live-commit-evaluator.el` to time
  `ekp-layout-plan-append` and `ekp--dp-cache-append` separately in each raw
  sample and report them beside total/publication timings.
- **Modify** `tests/run-live-commit-evaluator.sh` to write each run into a
  unique raw directory and atomically replace the report after comparison.
- **Evidence:** source-fresh width-80/two-row/GC-excluded candidate p95 is
  17.161 ms C append / 1.187 ms C append-DP, and 73.293 ms Elisp append /
  57.063 ms Elisp append-DP; raw JSONL is nonempty and parity/zero-work/GC/
  conflict/non-regression remain true. No production optimization is claimed
  yet because the measured DP/append owners require an exact redesign.
- **Commit:** `db6e6b9`.

## 2026-08-20 — Split append preparation, assembly, and DP timings

- **Modify** the evaluator to time `ekp--append-para` and
  `ekp--layout-plan-from-para` independently, fixing a wrapper-name collision
  found by the first red run.
- **Evidence:** latest source-fresh candidate p95 is C append/append-DP
  16.953/1.015 ms and Elisp append/append-DP 71.896/55.970 ms; both engines
  spend about 9.753 ms in append preparation and 5.663 ms in plan assembly.
  The report remains red, raw JSONL is nonempty, and the wrapper itself now
  passes the same matrix without argument errors.
- **Commit:** Pending measurement commit.

## Verification

- Source-first normal ERT: 295/295.
- Source-first seeded random ERT: 295/295, including all nine GUI verifier
  tests previously omitted by the name filter.
- Per-test isolated ERT: 294/294 process runs logged `0 unexpected`; the
  later batch-position test is covered by focused isolated C ERT.
- C focused ERT: 15/15; C portable build warning-clean; source fuzz:
  300/300 with zero failures; checkdoc, release, dictionary, and shell gates
  pass.
- Corrected narrowed source evaluator: parity, zero-work, GC, conflicts, and
  width non-regression pass, but source candidate p99 remains above 16 ms;
  the performance issue is intentionally not closed.
