# Centralize Rules, Not File Shapes

## Context

`ekp-para` necessarily carries the width-independent data shared by Elisp
DP, the C boundary, and rendering.  Splitting the struct or `ekp.el` would
mostly add declarations and navigation.  Two actual rules, however, were
duplicated: edge-space exclusion appeared in four hot paths, and the
lossless marker vocabulary appeared in renderer and region integration.

Cache identity and ideal-line measurement already had direct owners and
brute-force regressions from earlier work.

## Decision

Extract one allocation-free inline function for edge-space exclusion and
one constant for layout marker properties.  Reuse them at every existing
call site and test each rule directly.  Keep the coherent paragraph struct
and workflows in place.

## Alternatives

- Splitting `ekp.el` by “core,” “common,” or “helpers” was rejected because
  it moves code without moving a stable responsibility.
- Returning a new line-metrics object per DP candidate was rejected because
  it adds hot-loop allocation to remove textual duplication.
- Leaving the formulas inline was rejected because four copies had already
  crossed DP/reconstruction/render ownership boundaries.

## Consequences

The most drift-prone edge rule and marker vocabulary now change in one
place.  Cache keys, line metrics, and marker inversion each have direct
tests.  Large workflows remain visible instead of being hidden behind
pass-through helpers.

## Rollback

Inline the function and constant if profiling or compatibility evidence
requires it, while retaining the independent rule tests.  Do not replace
them with a broader abstraction unless it removes additional real
duplication.
