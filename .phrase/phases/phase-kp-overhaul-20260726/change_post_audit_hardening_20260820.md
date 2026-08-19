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

## Verification

- Source-first normal ERT: 294/294.
- Source-first seeded random ERT: 294/294, including all nine GUI verifier
  tests previously omitted by the name filter.
- Per-test isolated ERT: 294/294 process runs logged `0 unexpected`.
- C focused ERT: 15/15; C portable build warning-clean; source fuzz:
  300/300 with zero failures; checkdoc, release, dictionary, and shell gates
  pass.
- Corrected narrowed source evaluator: parity, zero-work, GC, conflicts, and
  width non-regression pass, but source candidate p99 remains above 16 ms;
  the performance issue is intentionally not closed.
