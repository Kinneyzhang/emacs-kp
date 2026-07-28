# Change Log: Automatic Spacing Cache Identity 2026-07-28

## task003

- **Modify** — `ekp.el`
  - Added one spacing signature used by the paragraph hash and
    `ekp--last-para`.
  - Automatic mode records `ekp-default-cws-stretch-pixel`; explicit mode
    records all nine spacing values.
  - Kept font-derived automatic inputs owned by the existing font and
    display-context identity.

- **Modify** — `tests/ekp-tests.el`
  - Added separate regressions for paragraph-table reuse and the same-string
    fast path.
  - Added an identity/count control proving unchanged signatures still hit.

- **Modify** — public, developer, audit, and phase documentation
  - Documented the complete spacing identity and closed `issue001`.
  - Added the decision record
    `postmortem/20260728-paragraph-spacing-signature.md`.

## Validation

- Focused red: 1/3 passed; both stale-value regressions returned 2 instead
  of the new default 9.
- Focused green: 3/3.
- Full ERT: 99/99.
- C/Elisp fuzz: 300/300.
- Byte compilation with warnings as errors: clean.
- checkdoc: clean.

## Behavior and Risk

- Changing automatic CJK stretch takes effect on the next call without
  clearing caches.
- Returning to an earlier signature may reuse its still-valid paragraph.
- No public API, C ABI, saved-file format, dependency, or watcher changed.
