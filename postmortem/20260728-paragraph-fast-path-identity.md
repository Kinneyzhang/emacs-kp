# Give Both Paragraph Cache Paths One Identity Owner

## Context

The paragraph hash cache used `ekp--para-key`, including filtered
text-property intervals and every paragraph preprocessing input. The
one-entry `ekp--last-para` path bypassed that key and compared a smaller
tuple. Mutating `ekp-no-break` on the exact cached string object therefore
returned stale boxes and break permissions.

## Decision

Compute `ekp--para-key` before either lookup. The one-entry path may skip the
hash-table operation when both string identity and that complete key match,
but it does not own a second signature. Remove style-variable watchers whose
only purpose was to compensate for the partial fast-path tuple.

## Alternatives Rejected

### Add Text Properties to the Partial Tuple

That repairs one omission while preserving two independently maintained
identity rules. The next paragraph input would recreate the bug.

### Clear All Paragraph Caches on Property Mutation

Standalone strings have no general mutation hook, and eager global
invalidation would discard unrelated paragraphs.

### Remove the One-Entry Path

That is correct but needlessly adds a hash-table lookup to repeated resolution
inside one formatting call. Comparing the already-owned key keeps the narrow
optimization without duplicate semantics.

## Consequences

- Same-object text-property mutations now miss both lookup paths.
- Style, language, spacing, display context, and text properties have one
  paragraph identity owner.
- Six watcher registrations and their anonymous invalidation function are
  removed.
- The fast path still avoids the hash lookup, but no longer avoids computing
  the correctness key.

## Verification

The new regression failed 0/1 before the change. It passes for both CJK and
Latin-with-space strings after the change, and the related cache/style/
display-context matrix passes 6/6.

## Rollback

Restore the partial tuple and watchers. No persisted data needs migration,
but the same-object property regression returns.
