# Plan: Complete Automatic Spacing Cache Identity

## Scope

Finish `issue001` by fixing the auto-CWS paragraph-cache defect only.
Preserve the completed DP signature work in `task002`.

## Resolution Path

1. Prove the paragraph hash and same-string fast path both reuse stale
   automatic spacing after `ekp-default-cws-stretch-pixel` changes.
2. Give paragraph preprocessing one spacing-signature owner shared by both
   lookup paths.
3. Keep automatic font-derived values represented by the existing
   font/display-context identity; add the remaining CJK default explicitly.
4. Prove an unchanged signature still hits instead of disabling caching.
5. Run focused and full ERT, fuzz, warning-clean compilation, and checkdoc.

## Non-goals

- No variable watcher for the CJK default.
- No DP-key or C ABI change.
- No editor-integration changes.

## Rollback

Revert the spacing-signature helper, the two lookup uses, and the three
regression tests. Caches are process-local, so no data migration is needed.
