# Issue: Showcase Contract and Test Freshness 2026-08-20

## issue026 [ ] Showcase loses automatic inline policy and runners can false-green

- **Status:** Runtime/test fix verified by `task041`; user-visible
  confirmation remains pending.
- **Summary:** The showcase's automatic inline-code face was commented out,
  so clean source runs fail the GUI policy contract. Local stale ignored
  `tests/*.elc` files can mask the regression, and random/isolated selectors
  omit nine GUI verifier tests.
- **Expected vs Actual:** Clean, source-fresh normal/random/isolated runners
  should execute the complete ERT inventory and retain the automatic inline
  example. Current source has 3 failing showcase tests; stale local bytecode
  reports a false green.
- **Related:** `tests/ekp-showcase.el`, `tests/run-tests.sh`,
  `tests/run-tests-random-order.el`, `tests/run-tests-isolated.sh`,
  `task041`.
- **Fix:** The automatic inline face is restored; one source loader uses
  `load-file` for every production/test source and all runners select the
  complete ERT registry.
- **Verification:** Source-first showcase 3/3, normal/random/isolated 294-test
  inventories, and GUI contract checks pass.
- **Resolved At:** Pending user-visible confirmation.
- **Resolved By:** Developer implementation and verification.
- **Commit:** `6a8c7e0`.
