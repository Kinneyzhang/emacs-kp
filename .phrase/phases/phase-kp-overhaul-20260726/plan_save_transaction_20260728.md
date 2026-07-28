# Plan: Non-mutating Save Serialization

## Scope

Resolve `issue002`: saving justified content must write logical text while
the display buffer remains unchanged on success, filesystem failure,
encoding failure, or interruption.

## Resolution Path

1. Drive `save-buffer` through real failure and `quit` paths and prove the
   old before/after-hook transaction leaves the buffer unformatted.
2. Stop mutating the source buffer during save.
3. Use Emacs's `write-region-annotate-functions` buffer-switch contract to
   serialize a logical copy at the actual write boundary.
4. Preserve later annotation/coding processing and clean the copy on
   success, retry, integration removal, or source-buffer teardown.
5. Verify success, three failure classes, retry, full ERT, fuzz,
   warnings-as-errors compilation, and checkdoc.

## Non-goals

- No global advice around `save-buffer` or `write-region`.
- No replacement implementation of Emacs file saving.
- No copy-filter composition or manual-unjustify lifecycle change.

## Rollback

Restore the before/after save hooks and remove the logical write-buffer
tests. No file format or persisted metadata changes.
