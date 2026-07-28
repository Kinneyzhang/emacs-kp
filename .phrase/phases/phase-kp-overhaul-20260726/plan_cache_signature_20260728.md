# Plan: Complete DP Cache Signature

## Scope

Fix `issue001` P1-01 only: DP results must not alias when any
algorithm/cost parameter changes. The auto-CWS paragraph-key defect remains
open for a separate atomic task.

## Resolution Path

1. Drive the public `ekp-dp-cache` path with cached-then-changed parameters.
2. Prove each scenario differs from its baseline and matches a fresh
   computation.
3. Replace the width/looseness special-case key with one complete flat
   signature.
4. Use structural equality for DP-cache keys and prove identical signatures
   still hit the cache.
5. Run focused tests, the complete suite, C/Elisp fuzz, byte compilation,
   checkdoc, and diff/static checks.

## Data Ownership

The `ekp-para` DP cache owns reuse. Its key must therefore contain every
runtime value read by the DP that is not already frozen into that paragraph:

- line width and looseness;
- line, hyphen, adjacent-fitness, and consecutive-hyphen penalties;
- last-line minimum ratio and short-line penalty.

Paragraph-construction inputs remain owned by `ekp--para-key`.

## Non-goals

- No cache watchers or eager cache clearing.
- No C ABI change.
- No save/copy integration changes.
- No fix for the separate auto-CWS paragraph cache.

## Rollback

Revert the key/table change and the associated regression tests. No stored
file format or public API changes.
