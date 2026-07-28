# Plan: Property-Sensitive Paragraph Fast Path 2026-07-28

## Scope

Fix `issue010` only: mutating layout-relevant text properties on an already
cached string object must not reuse stale paragraph preprocessing.

## Resolution Path

1. Warm the public paragraph resolver with the exact string object.
2. Mutate `ekp-no-break` on that object and prove the current fast path
   returns stale boxes/break permissions.
3. Make the fast path compare the same complete structural paragraph key as
   the hash cache instead of maintaining a partial parallel signature.
4. Remove invalidation watchers that become redundant once one key owns both
   paths.
5. Verify the mutated object matches a fresh computation across CJK and
   Latin-with-space inputs, then rerun every final gate.

## Ownership

`ekp--para-key` is the sole owner of paragraph preprocessing identity. The
most-recent lookup may bypass the hash table, but it may not bypass or
partially reimplement that key.

## Non-goals

- No new cache layer or eager global invalidation.
- No public API, rendering, C ABI, or serialized-data change.
- No attempt to observe arbitrary font/theme mutation; the documented
  `ekp-clear-caches` boundary remains unchanged.

## Rollback

Restore the partial fast-path tuple and its variable watchers. No persisted
state needs migration, but the same-object property regression returns.
