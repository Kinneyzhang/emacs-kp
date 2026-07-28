# One Spacing Signature for Both Paragraph Cache Paths

## Context

Automatic spacing derives most values from the paragraph's measured font,
but reads `ekp-default-cws-stretch-pixel` directly. The paragraph hash stored
only `auto`, and `ekp--last-para` bypassed that hash entirely. Changing the
default therefore returned a paragraph created with the old CJK stretch.

## Decision

Use one `ekp--spacing-signature` at both paragraph lookup boundaries.
Explicit mode records all nine spacing values. Automatic mode records the
CJK stretch default; font-derived inputs remain represented by the existing
font and display-context fields.

## Alternatives Rejected

### Add another variable watcher

A watcher would repair only the one-entry fast path. The paragraph hash
would still alias old and new automatic values, leaving the root model wrong.

### Clear all paragraph caches on change

Eager invalidation would discard unrelated font measurement and paragraph
analysis. A structural signature lets each valid configuration reuse its
own paragraph.

### Store every derived automatic value in the key

Computing those values requires measurement and would duplicate paragraph
construction work during lookup. Their actual inputs are already in the
font, text-property, and display-context identity.

## Consequences

- Automatic CJK spacing changes apply on the next call.
- Direct explicit-spacing mutations also cannot bypass the fast path.
- Unchanged signatures retain hash and one-entry fast-path hits.
- No public API, C ABI, saved-file format, or new watcher was added.

## Verification

Two old-model regressions fail with stale `:cws-stretch` value 2 after the
default changes to 9; the unchanged-signature control already passes.
After the fix, all three pass.
The complete suite passes 99/99, C/Elisp fuzz passes 300/300, and
warnings-as-errors byte compilation plus checkdoc are clean.

## Rollback

Revert the shared signature and regression tests. No persisted data is
affected.
