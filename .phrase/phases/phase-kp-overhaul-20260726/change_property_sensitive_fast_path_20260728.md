# Change Log: Property-Sensitive Paragraph Fast Path 2026-07-28

## task016

- **Modify** — `ekp.el`
  - Store the complete structural paragraph key beside the most-recent
    string and paragraph.
  - Compare the same key used by the paragraph hash cache before reusing the
    fast-path result.
  - Delete six style-variable watchers that compensated for the former
    partial identity.

- **Modify** — `tests/ekp-tests.el`
  - Warm one string object, mutate its `ekp-no-break` property in place, and
    require the next lookup to match a fresh property-aware paragraph for
    CJK and Latin-with-space inputs.
  - Retain distinct-string property, language, style, and unchanged-key hit
    controls.

- **Modify/Add** — README, developer, audit, issue, phase, and postmortem
  records
  - Document one cache-identity owner and close `issue010`.
  - Record the wrong-layer compensation in
    `postmortem/20260728-paragraph-fast-path-identity.md`.

## Validation

- Same-object property mutation red/green: 0/1 → 1/1.
- Focused paragraph-cache matrix: 6/6.
- Default and seeded-permuted full ERT: 130/130 each.
- Fresh-process isolation: every one of 130 ERT tests passed.
- C/Elisp fuzz: 300/300.
- Warning-as-error production compilation, checkdoc, pinned package-lint,
  four C profiles, release/dictionary/static gates, and GUI matrix 7/7:
  pass.
- Independent architecture review: `CLEAR` for the shared-key ownership.

## Behavior and Risk

- Public APIs, saved-file formats, C ABI, and Emacs 29.1 baseline are
  unchanged.
- Cache hits now require the complete text/configuration identity already
  owned by `ekp--para-key`.
- The change removes invalidation branches instead of adding watchers or a
  second identity model.
