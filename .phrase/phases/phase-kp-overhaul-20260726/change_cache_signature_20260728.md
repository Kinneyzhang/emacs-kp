# Change Log: Complete DP Cache Signature 2026-07-28

## task002

- **Modify** — `ekp.el`
  - Replaced the width/looseness special-case key with one flat signature
    containing every runtime DP input not frozen into `ekp-para`.
  - Changed each paragraph's DP cache from `eql` to structural `equal`
    comparison.
  - Result: parameter changes cannot alias stale results, and structurally
    identical non-zero-looseness signatures now hit the cache.

- **Modify** — `tests/ekp-tests.el`
  - Added six parameter-by-parameter cached-vs-fresh regressions through
    `ekp-dp-cache`.
  - Added an identity/count assertion proving the fix preserves real cache
    hits instead of merely forcing recomputation.

- **Modify** — public and developer documentation
  - Removed the temporary manual-cache-clear workaround.
  - Documented the complete signature and added an Unreleased changelog
    entry.
  - Preserved the dated audit evidence while recording the follow-up status.

- **Add** — `postmortem/20260728-dp-cache-signature.md`
  - Recorded why a complete structural key was chosen over watchers,
    collision-prone hashes, or another mutable signature owner.

## Validation

- Focused red: 0/2 expected tests passed on the old key.
- Focused green: 2/2 passed on the complete signature.
- Full source ERT: 96/96.
- Full compiled ERT: 96/96.
- C/Elisp fuzz: 300/300.
- Byte compilation with warnings as errors: clean.
- checkdoc: clean.
- Public rendered-output probe: cached and fresh results match in both
  Elisp and C modes after a penalty change.
- 100,000 cache-hit microbenchmark: approximately 0.37 seconds versus
  0.17 seconds for the old scalar key (about 2 microseconds per lookup).

## Risk

- Cache keys are small structural lists instead of scalar/cons `eql` keys.
- Deliberately sweeping many parameter combinations retains one result per
  signature until the paragraph cache is cleared.
- No public API, C ABI, paragraph format, or saved-file format changed.
