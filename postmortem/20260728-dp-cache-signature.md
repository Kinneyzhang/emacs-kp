# Complete DP Cache Signatures Instead of Invalidation Watchers

## Context

`ekp-para` caches width-dependent DP and rendered results. The old key was
only the line width, with a special `(width . looseness)` cons when looseness
was non-zero. Two failures followed:

- changing a cost parameter reused a result computed under the old value;
- the cons key never hit an `eql` hash table when reconstructed.

The DP reads six cost parameters at runtime, so paragraph identity alone
cannot make those cached results valid.

## Decision

Use one flat signature containing line width, looseness, and all six runtime
cost parameters. Compare keys with structural `equal`.

Paragraph-construction inputs remain in `ekp--para-key`; DP-only inputs
remain in `ekp--dp-key`. No cache-clearing side effect crosses that boundary.

## Alternatives Rejected

### Variable watchers that clear caches

Watchers would spread ownership across seven variables and discard
width-independent paragraph work. Forgetting the next parameter would
silently recreate the bug.

### An integer hash of all parameters

A hash collision would become a correctness failure. Structural keys make
collisions harmless because equality is still checked.

### A memoized global signature object

It saves small list allocations but adds mutable global state and comparison
logic. A 100,000-hit microbenchmark measured approximately 0.37 seconds for
the complete key versus 0.17 seconds for the old scalar key: about two
microseconds per lookup, below the cost threshold that would justify another
state owner.

## Consequences

- Algorithm changes take effect on the next call without manual flushing.
- Returning to a previous parameter set can reuse its prior cached result.
- Non-zero looseness now receives real cache hits.
- Cache lookup performs structural hashing over eight small scalar values.
- Each distinct parameter set can retain its own result in the paragraph
  cache; normal interactive configuration has low cardinality, but a caller
  that sweeps parameters should clear caches between experiments.
- No public API, C ABI, saved-file format, or paragraph representation
  changed.

## Verification

- The old model failed both the parameter-isolation and identical-signature
  tests.
- The new model passes all six parameter cases and the cache-identity check.
- Full ERT, C/Elisp fuzz, warning-clean byte compilation, and checkdoc pass.

## Rollback

Revert the key/table change and these regression tests. No migration or data
cleanup is necessary because caches are process-local.
