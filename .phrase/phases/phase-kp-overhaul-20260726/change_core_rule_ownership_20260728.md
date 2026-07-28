# Change Log: Core Rule Ownership 2026-07-28

## task014

- **Modify** — `ekp.el`, `ekp-region.el`
  - Centralized the edge-space exclusion used by both Elisp DPs, C-result
    reconstruction, and line-glue rendering in one inline pure rule.
  - Centralized the five lossless layout marker properties used by renderer
    nonstickiness and region logical-string detection.

- **Modify** — `tests/ekp-tests.el`
  - Added an independent brute-force cross-check for every candidate
    line's excluded edge-space width.
  - Added a direct marker vocabulary/noninheritance contract.

- **Modify/Add** — developer/audit/changelog records and
  `postmortem/20260728-core-rule-ownership.md`
  - Recorded why two narrow owners have net value while a file/struct split
    does not.

## Validation

- Direct-rule red/green: 0/2 → 2/2; focused related invariants: 4/4.
- Full ERT: 129/129; C/Elisp fuzz: 300/300.
- Warning-as-error production compilation, checkdoc, release, dictionary,
  and diff gates: pass.

## Behavior and Risk

- Public behavior and C ABI are unchanged.
- The edge-space helper is `defsubst`; it adds no byte-compiled inner-loop
  allocation.
- No wrapper ladder, module, or data object is introduced.
