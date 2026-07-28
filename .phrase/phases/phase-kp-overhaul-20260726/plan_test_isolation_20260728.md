# Plan: ERT State and Order Isolation

## Scope

Resolve `issue004`: make the parshape dispatch regression truthful, restore
all test-owned EKP configuration, and make order dependence observable.

## Resolution Path

1. Reproduce the named parshape test alone with the C module loaded.
2. Drive the public formatter with a real parshape and an observable C-call
   negative control.
3. Dynamically bind every mutable EKP option and spacing state owned by the
   shared clean-state fixture.
4. Add public-ERT-API runners for reproducible permutation and one fresh
   Emacs process per test.
5. Run the focused matrix, full permutation, full isolation, compilation,
   and checkdoc.

## Non-goals

- No test framework dependency.
- No reliance on private ERT functions.
- No production formatter change.

## Rollback

Restore the partial fixture and default runner, then remove the new
permutation/isolation entry points and CI step. No persisted data changes.
