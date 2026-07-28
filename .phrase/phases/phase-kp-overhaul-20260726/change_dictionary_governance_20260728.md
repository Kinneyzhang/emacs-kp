# Change Log: Dictionary Syntax and Provenance 2026-07-28

## Planning

- Added `task011` and a decision gate before parser changes: replacement
  syntax could only be implemented if the fixed-width Elisp/C DP and
  lossless renderer could represent it correctly.

## task011

- **Modify** — `ekp-hyphen.el`, `ekp.el`
  - Count and reject slash/replacement dictionaries with a typed,
    cached condition.
  - Propagate unsupported syntax through the public formatter while still
    treating a missing optional dictionary as “hyphenation unavailable.”

- **Add/Modify/Delete** — `dictionaries/`
  - Added a 49-entry manifest with pinned source paths, SHA-256, syntax
    counts, and license evidence.
  - Replaced the moving/GNU-specific updater with fail-closed POSIX
    `check` and deterministic `export` modes.
  - Retained Basque explicitly as a verified legacy byte.
  - Removed `hyph_sa_IN.dic`; the pinned upstream snapshot has no
    authoritative license statement for that exact hyphenation data.

- **Add** — affected-language and inventory tests
  - Locked libhyphen golden expectations for Hungarian, Catalan, and
    Albanian, plus the Esperanto slash-pattern boundary.
  - Added the offline 49-entry checksum/inventory/license/syntax gate.

- **Modify** — CI, release, public/developer/audit documentation
  - Added offline and pinned-upstream dictionary gates.
  - Replaced “every bundled dictionary works” with the exact supported
    ordinary-pattern contract.

- **Add** — `postmortem/20260728-dictionary-contract.md`
  - Recorded why parser-only or renderer-only replacement support would be
    incorrect and why the future boundary crosses the DP/C ABI.

## Validation

- Affected languages red/green: 0/2 → 2/2.
- Offline manifest: 49/49 pass.
- Pinned upstream check: 49/49 normalized bytes pass.
- Two fresh exports: byte-identical, 49 dictionaries each.
- Full ERT: 121/121.
- Warning-as-error byte compilation: pass.
- Production checkdoc, updater/check shell syntax, workflow YAML, and diff
  checks: pass.

## Behavior and Risk

- `eo`, `ca`, `hu_HU`, and `sq_AL` now signal instead of silently using a
  linguistically incomplete pattern subset.
- Forty-five ordinary-pattern dictionaries remain directly usable.
- Sanskrit support is deliberately removed until exact license evidence
  exists; no compatibility shim or unverified license assumption remains.
