# Plan: Dictionary Syntax and Provenance 2026-07-28

## Goal

Make the 50 bundled dictionaries reproducible and accurately described:
every byte has a pinned source/checksum/license trail, and the parser's
handling of replacement/alternative patterns is explicit and tested.

## Scope

1. Confirm upstream replacement syntax and golden words for Hungarian,
   Catalan, and Albanian before changing the parser.
2. Add failing tests for the chosen affected-language contract and updater
   manifest validation.
3. Pin the LibreOffice dictionaries commit and replace GNU-specific,
   moving-HEAD update commands with a fail-closed POSIX workflow.
4. Generate a deterministic manifest containing source path, SHA-256,
   syntax flags, and local/upstream license evidence for every dictionary.
5. Document the supported syntax honestly.  Implement replacement semantics
   only if they fit the fixed-width box/DP model without corrupting logical
   text or creating a second typography engine.
6. Run golden tests, manifest verification, a double-run reproducibility
   comparison, full ERT, byte compilation, and checkdoc.

## Non-goals

- Do not add automatic language detection.
- Do not silently treat replacement patterns as ordinary Liang patterns.
- Do not move font measurement, tokenization, or rendering ownership into
  the dictionary parser.
- Do not add a parser dependency.

## Decision Gate

Replacement patterns conditionally rewrite glyphs at a selected break.  If
correct support requires break-specific box widths across Elisp DP, C
marshalling, and rendering, preserve the safe skip behavior, expose/count it,
add language golden limitations, and document that subset.  Do not emit
linguistically incorrect breaks merely to increase pattern coverage.

## Rollback

Restore the previous parser and bundle together.  A manifest must never claim
checksums or source paths for bytes that are not present.
