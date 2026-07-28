# Change Log: Composable Buffer Integration Lifecycle 2026-07-28

## task005

- **Modify** — `ekp-region.el`
  - Saved and restored the previous local/inherited substring-filter owner.
  - Composed prior filtering with EKP logical inversion while retaining
    DELETE behavior.
  - Split structural unjustify from the public lifecycle boundary.
  - Removed integrations when the final span disappears outside auto mode,
    including external delete paths.

- **Modify** — `tests/ekp-region-tests.el`
  - Added local/inherited filter restoration, copy and DELETE composition,
    final-unjustify cleanup, and auto-mode-disable cases.

- **Modify** — public, developer, audit, and phase documentation
  - Documented composition order and lifecycle ownership.
  - Closed `issue003` and added
    `postmortem/20260728-buffer-integration-ownership.md`.

## Validation

- Focused red: 0/2 for prior-filter output and final-span cleanup.
- Focused green: 2/2; expanded integration matrix 5/5.
- Full ERT: 107/107.
- C/Elisp fuzz: 300/300.
- Byte compilation with warnings as errors: clean.
- checkdoc: clean.

## Behavior and Risk

- Existing mode/user substring filters remain active under EKP.
- Manual unjustify, DELETE, and mode shutdown restore exact ownership.
- A prior filter that deliberately strips all text properties also removes
  EKP's structural markers; such a filter owns that returned representation.
- No dependency, public API, or saved-file format changed.
