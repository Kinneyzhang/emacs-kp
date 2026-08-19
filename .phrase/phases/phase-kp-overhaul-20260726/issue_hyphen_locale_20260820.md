# Issue: Hyphenation Locale Resolution 2026-08-20

## issue024 [ ] Hyphenation accepts common locale spelling but selects the wrong dictionary

- **Status:** Runtime fix verified by `task039`; user-visible confirmation
  remains pending.
- **Summary:** `de-CH` and `de-DE` normalize to underscore forms but are
  looked up in the short-code registry, where they can resolve to the first
  arbitrary German dictionary rather than the exact locale.
- **Expected vs Actual:** Equivalent `de_CH`/`de-CH` spellings should select
  `hyph_de_CH.dic`; current `de-CH` selects `hyph_de_AT.dic`.
- **Related:** `ekp-hyphen.el`, `plan_dictionary_governance_20260728.md`,
  `task039`.
- **Fix:** Raw and normalized exact locale keys are checked before the
  documented short-code fallback.
- **Verification:** Locale-equivalence ERT 11/11 and full dictionary gates
  pass.
- **Resolved At:** Pending user-visible confirmation.
- **Resolved By:** Developer implementation and verification.
- **Commit:** `6a8c7e0`.
