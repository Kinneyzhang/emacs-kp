# Change Log: ERT State and Order Isolation 2026-07-28

## task006

- **Modify** — `tests/ekp-tests.el`
  - Dynamically scoped and restored all isolated EKP tunables.
  - Replaced the mislabeled internal predicate assertion with a public
    parshape dispatch test and added a fixture-restoration control.

- **Add/Modify** — test runners and `.github/workflows/ci.yml`
  - Added seeded permutation and fresh-process-per-test runners using public
    ERT APIs.
  - Added the reproducible permuted-order lane to CI.

- **Modify** — changelog, developer, audit, phase, and postmortem records
  - Documented the commands, invariant, evidence, and `issue004` closure.

## Validation

- Focused red: isolated C-loaded parshape test 0/1.
- Focused green: public dispatch, fixture restoration, and C-parameter
  cases 3/3; isolated parshape 1/1.
- Reproducibly permuted full ERT: 108/108.
- Every ERT in a fresh Emacs process: 108/108.

## Behavior and Risk

- Production code and public API are unchanged.
- CI gains one full ERT invocation on Emacs 30.1.
- The slower fresh-process runner is an explicit local/release diagnostic,
  not part of every CI job.
