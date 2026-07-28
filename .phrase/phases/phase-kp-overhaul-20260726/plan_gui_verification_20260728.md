# Plan: Fail-Closed GUI Verification

## Scope

Resolve `issue007`: retain the explicitly loaded developer GUI matrix while
making any failed row a nonzero automation result.

## Resolution Path

1. Add a batch-safe forced-failure control for the missing assertion
   boundary.
2. Preserve each case as structured data and format all rows centrally.
3. Print the full table, then exit 1 in batch mode when any row fails.
4. Document the tests-only loading boundary and automation status.
5. Run focused/full ERT, a non-default permutation, and the live seven-case
   GUI matrix; inspect a clean guarded screenshot.

## Non-goals

- No headless approximation of real pixel rendering.
- No promotion of showcase/matrix code into the runtime package.
- No GUI CI provider or new dependency.

## Rollback

Restore string-only case results and table rendering, then remove the two
batch-boundary tests. No persisted data changes.
