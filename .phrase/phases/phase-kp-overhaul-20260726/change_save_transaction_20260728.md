# Change Log: Non-mutating Save Serialization 2026-07-28

## task004

- **Modify** — `ekp-region.el`
  - Removed the before/after-save unjustify/rejustify transaction.
  - Added one logical write-buffer owner using the documented
    `write-region-annotate-functions` buffer-switch contract.
  - Added bounded cleanup on success, retry, integration removal, and source
    buffer teardown.

- **Modify** — `tests/ekp-region-tests.el`
  - Added real filesystem failure, forced encoding failure with successful
    retry, and `quit` regressions through `save-buffer`.
  - Preserved the existing successful logical-write assertion.

- **Modify** — public, developer, audit, and phase documentation
  - Documented failure-safe visible behavior and closed `issue002`.
  - Added `postmortem/20260728-nonmutating-save-serialization.md`.

## Validation

- Focused red: 0/2; failure and interruption both removed the layout.
- Focused green: 2/2.
- Save matrix: 4/4 for success, filesystem failure, encoding retry, and
  interruption.
- Full ERT: 102/102.
- C/Elisp fuzz: 300/300.
- Byte compilation with warnings as errors: clean.
- checkdoc: clean.

## Behavior and Risk

- The display buffer is never unformatted during saving.
- Later write annotations and coding conversion operate on logical text.
- A failed write may retain one hidden logical copy until retry or teardown;
  it never grows beyond one per source buffer.
- No global advice, saved-file format, public API, or dependency changed.
