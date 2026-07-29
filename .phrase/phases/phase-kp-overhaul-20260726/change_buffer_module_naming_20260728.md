# Change Log: Buffer Module Naming 2026-07-28

## task017

- **Delete/Add** — `ekp-region.el` → `ekp-buffer.el`
  - Renamed the editor integration file and provided feature around the
    buffer lifecycle owner.
  - Renamed module configuration and private symbols from `ekp-region*` to
    `ekp-buffer*`.
  - Attached the buffer customization subgroup to the top-level `ekp`
    group and made the protrusion-reserve helper explicitly private.
  - Kept public commands whose names correctly describe region, buffer, or
    mode behavior.

- **Delete/Add** — `tests/ekp-region-tests.el` →
  `tests/ekp-buffer-tests.el`
  - Renamed the test feature, fixtures, and ERT names.
  - Updated default, random-order, isolated, GUI, and showcase loaders.

- **Modify** — CI, contribution, public/developer/audit documentation
  - Updated compilation, lint, checkdoc, test, and Windows CI paths.
  - Replaced the documented require/configuration surface and file maps.
  - Added the breaking migration to `CHANGELOG.md`.
  - Recorded the ownership decision in
    `postmortem/20260728-buffer-module-naming.md`.

## Cleanup Review

- Naming mismatch: resolved at the owning module/feature/configuration/test
  boundary.
- Dead code, duplication, error handling, dependency, and UI: unchanged and
  outside this pass.
- Fallback findings: only the existing selected-window width fallback,
  classified as a grounded undisplayed-buffer boundary.
- No masking fallback, swallowed error, compatibility shim, new dependency,
  new abstraction, or unrelated refactor was introduced.

## Validation

- Pre-change behavior lock: ERT 130/130.
- Focused buffer ERT after rename: 44/44.
- Default and seeded-permuted ERT: 130/130 each.
- Fresh-process isolation: every one of 130 selected ERT tests passed.
- C/Elisp property fuzz: 300/300.
- Warning-as-error production byte compilation: pass.
- Checkdoc and pinned package-lint: pass.
- Release invariants, shell syntax, diff check, and active-surface stale-name
  scan: pass.
- Mechanical runtime/test equivalence against the approved rename table:
  pass.

## Behavior and Risk

- Layout, serialization, search, copy, undo, and automatic reflow behavior
  are unchanged.
- This is an intentional source-level breaking rename: users must require
  `ekp-buffer` and rename any `ekp-region-*` settings.
- No saved-file format, dependency, Emacs baseline, C ABI, or release
  artifact changed.
- Generated `.elc` verification artifacts were removed after compilation;
  they can be regenerated from the source.
