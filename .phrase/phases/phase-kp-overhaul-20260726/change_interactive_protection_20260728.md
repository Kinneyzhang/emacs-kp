# Change Log: Interactive Protection Workflow 2026-07-28

## task013

- **Modify** — `ekp-region.el`
  - All four protection commands now share one property/feedback owner.
  - Interactive calls report the affected character count and
    current-buffer-session lifetime.
  - Auto-justify mode help and a standard EKP menu expose the existing
    format, protection, removal, and diagnostic commands.

- **Modify** — `tests/ekp-region-tests.el`
  - Drive no-break and verbatim mark/clear commands through
    `call-interactively`.
  - Verify behavior through the real public formatters, visible feedback,
    mode help, and menu discovery.

- **Modify/Add** — bilingual README, changelog, audit and
  `postmortem/20260728-session-local-protection.md`
  - State that manual text properties do not survive plain-text save/reopen
    and route persistent syntax through faces/predicates.

## Validation

- Focused public-path red/green: 0/3 → 3/3.
- Full default and seeded-permuted ERT: 127/127 each.
- Fresh-process isolation: every one of 127 ERT tests passed.
- Warning-as-error production byte compilation, checkdoc, release,
  dictionary, and diff gates: pass.

## Behavior and Risk

- No file format, persistent metadata, or global key binding is added.
- Programmatic callers remain quiet unless they request the optional
  announcement; interactive callers always receive feedback.
- Existing commands and arguments remain compatible.
