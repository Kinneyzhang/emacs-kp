# Plan: Core Rule Ownership 2026-07-28

## Goal

Close audit P3-02 with the smallest net-value refactor: centralize only rules
that are currently duplicated across the hot DP/render paths, without
splitting files or wrapping workflows.

## Existing Owners

- DP reuse identity is already owned by `ekp--dp-key` and paragraph
  construction identity by `ekp--para-key`/`ekp--spacing-signature`.
- Ideal line width is already owned by `ekp--line-ideal-pixel` and covered
  by a brute-force test.

## Missing Owners

1. Edge-space exclusion is repeated in 1D DP, loose DP, line-glue rendering,
   and ideal-width reconstruction.
2. The five layout-marker property names are repeated between the renderer
   and region inversion detection.

## Resolution Path

1. Add red direct-rule tests against independent edge-space recomputation
   and the complete marker vocabulary/nonstickiness contract.
2. Add one inline edge-space rule and reuse it at all four call sites.
3. Add one marker property constant and reuse it in the renderer and region
   detection.
4. Retain existing exact round-trip, DP/C parity, fuzz, compile, and checkdoc
   gates.

## Non-goals

- Do not split `ekp.el` or shrink the `ekp-para` struct cosmetically.
- Do not allocate a line-metrics object in the DP inner loop.
- Do not add accessors, adapters, or change the C ABI.

## Rollback

Inline the two rules again.  Keep the direct invariants if a measurable
source-mode regression makes the inline helper unacceptable.
