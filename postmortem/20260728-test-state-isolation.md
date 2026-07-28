# Make Test State Ownership Explicit

## Context

The test named as a parshape C-bypass check actually configured first-line
indent, which C 1.5 supports. It passed in the alphabetic full suite only
because an earlier C-parameter test left `ekp-use-c-module` disabled.
The shared fixture restored a hand-picked penalty subset rather than the
state its callers could mutate.

## Decision

The fixture dynamically binds every public EKP tunable plus spacing state,
so arbitrary assignment inside a test is automatically scoped. Dispatch
regressions go through the public formatter and use an observable forbidden
C call, rather than asserting only an internal eligibility predicate.

Keep three complementary entry points: normal order for speed, a seeded
permutation for cross-test leakage, and one fresh process per test for
absolute isolation. The runners use the public `ert-select-tests` selector
contract.

## Alternatives Rejected

### Reset more variables to hard-coded defaults

That overwrites caller configuration, must be updated whenever defaults
change, and repeats the partial-ownership mistake.

### Test only `ekp--c-available-p`

It does not prove that the installed public dispatcher uses the same input
shape or filtering logic.

### Depend on alphabetical order

Stable order is useful for reproducibility, not as a correctness boundary.

## Consequences

- Leaked test assignments are restored to their incoming values.
- Parshape bypass is proven through the same path users call.
- A failed randomized seed can be reproduced with `EKP_TEST_SEED`.
- Full process isolation remains intentionally slower and opt-in.

## Verification

The original isolated C-loaded control failed 0/1. The focused repaired
matrix passed 3/3, a non-default permutation passed 108/108, and all 108
tests passed one-by-one in fresh Emacs processes.

## Rollback

Restore the old fixture and test, then remove the two runners and CI lane.
No production or persisted data needs migration.
