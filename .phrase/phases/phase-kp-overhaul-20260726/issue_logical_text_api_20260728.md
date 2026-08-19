# Issue: Logical Text API Boundary 2026-07-28

## issue011 [x] Direct Elisp buffer APIs observe physical layout text

- **Status:** Closed by user re-audit on 2026-08-13. Runtime fix and
  complete repository gate were implemented by `task019`–`task022`.
- **Summary:** The former buffer renderer wrote synthesized glue spaces,
  soft newlines, discretionary hyphens, and hidden payload text into the
  real buffer. Boundary adapters made save/copy/search appear logical, but
  ordinary Elisp character APIs still observed the physical layout.
- **Environment:** Text-property renderer and synchronous live flow after
  `task021`; Emacs 30.2.
- **Historical Repro:**
  1. Insert text into a temporary buffer.
  2. Run the pre-`task020` `ekp-justify-region` with a narrow width.
  3. Compare the original text with `buffer-string` or
     `buffer-substring-no-properties`.
  4. Compare the same region through `filter-buffer-substring`.
- **Historical Evidence:** The pre-fix batch probe produced a 39-character
  physical representation from a 30-character logical string.
  `buffer-string` did not equal the original, while the boundary-filtered
  substring did.
- **Expected vs Current:**
  - **Expected:** Ordinary Elisp buffer-text APIs observe the original
    character sequence while EKP's justified presentation remains visible
    and editable.
  - **Current implementation:** `buffer-string`,
    `buffer-substring-no-properties`, direct character access, ordinary
    search, syntax, markers, and saving operate on the unchanged source
    characters. APIs that preserve or inspect text properties may still
    observe EKP's display metadata; copy/kill strips that owned metadata.
- **Investigation:**
  - The former `ekp-justify-region` deleted source text and inserted the
    physical rendered string.
  - Reversible `ekp-glue`, `ekp-soft-break`, `ekp-soft-hyphen`, and
    `ekp-hidden` markers made restoration possible but did not keep direct
    buffer reads logical.
  - Save/copy/isearch adapters compensated at individual boundaries and
    could not cover arbitrary Elisp consumers.
  - The correct owner is the buffer representation: source characters must
    remain authoritative, and layout must be presentational state only.
- **2026-07-29 Design Evidence:**
  - The user ruled out overlays and selected text properties on existing
    source characters as the only projection mechanism.
  - `(min-width ((PIXELS)))` can add an exact absolute-pixel minimum width
    without adding buffer characters.
  - `(space-width FACTOR)` can shrink or stretch existing ASCII spaces.
    Combined with `min-width`, it rendered a natural 7px space at exact 3px
    and 20px targets in a clean GUI Emacs 30.2 probe while source text
    remained `"a b"`.
  - `line-prefix` can provide display-only indentation for
    non-continuation lines. Native continuation lines still require
    `wrap-prefix`.
  - A replacing `display` string on an existing grapheme can show the
    grapheme followed by a discretionary hyphen and visual newline. A GUI
    probe displayed `d-\n` from the source `d` in `abcdefgh`; the source
    stayed eight characters, two screen lines were produced, one
    `vertical-motion` landed on source position 5, and a 20px
    `line-prefix` applied to both rows.
  - The proposed Latin hyphen owner is the complete grapheme immediately
    before the selected break. Its display string reproduces that grapheme,
    appends the hyphen and newline, and uses the `cursor` property to keep
    point on the source glyph.
  - Full reasoning, source links, limitations, and verification gates are
    recorded in `tech-refer_text_property_layout_20260729.md`.
- **Root Cause:** Logical document state and rendered layout shared one
  mutable character sequence. Reversible markers made the physical
  representation recoverable, not logical.
- **Required Outcome:** Remove the leak at the representation owner rather
  than adding per-command adapters. Ordinary Elisp character consumers must
  see logical content without losing justified display, exact round-trip,
  or seamless editing.
- **Implemented Design:**
  - Logical source characters stay in the real buffer.
  - The buffer renderer creates no overlay and inserts no layout character.
  - Existing ASCII spaces use combined `space-width` and absolute-pixel
    `min-width`; zero-source CJK/mixed glue uses `min-width` on the
    preceding complete grapheme; indentation uses `line-prefix`.
  - CJK breaks and Latin discretionary hyphens use replacing display
    strings on an existing complete grapheme.
  - Projection values have exact EKP owner properties, are nonsticky, and
    update under `with-silent-modifications`.
  - Foreign `display`, `line-prefix`, `wrap-prefix`, `composition`, and
    `invisible` owners make only the affected hard paragraph natural and
    diagnostic.
  - One buffer uses the narrowest live window as its authoritative width;
    simultaneous independent per-window plans are not claimed.
  - Live editing uses a natural unfinished line, bounded overflow/pullback,
    a stable active-flow anchor, line-signature convergence, IME deferral,
    stale-generation rejection, and bounded automatic paragraph work.
- **Fix:** `ekp.el` now exposes a reusable semantic layout plan while
  retaining the compatible returned-string API. `ekp-buffer.el` consumes
  that plan as a source-clean text-property projection and runs a distinct
  synchronous live-flow state machine.
- **Verification to Date:**
  - Buffer ERT passes 79/79, including unchanged source/tick/undo/modified
    state, direct logical reads, save/copy/isearch, foreign ownership,
    no-overlay checks, exact property forms, overflow, deletion pullback,
    exact type/delete break restoration, immediate active-line edge
    whitespace, active/inactive mark preservation, anchor lifecycle, IME,
    undo, resize, multiwindow width, and overload behavior.
  - A clean Emacs 30.2 static probe passes exact 1–64px ASCII and
    zero-source CJK glue, display-only breaks/hyphens, `line-prefix`, point,
    vertical motion, region, mouse, unchanged source, and zero overlays.
    The seven-case scale/remap/fringe/width matrix passes.
  - A 31.71s dynamic recording passes before/immediate/settled overflow and
    deletion checkpoints. Immediate and settled source/projection hashes
    match; deletion restores the original source/projection hashes and
    break count; every assertion is true and temporal review passes.
  - Final-source default and seeded-permuted ERT pass 170/170; every one of
    the 170 tests passes in a fresh Emacs process; C/Elisp fuzz passes
    300/300.
  - Warning-as-error Elisp compilation, checkdoc, pinned package-lint,
    release/dictionary/static gates, all four C build profiles, full diff
    review, and anti-slop cleanup pass.
  - Independent final code review returns `APPROVE` with no findings;
    independent architecture review returns `CLEAR`.
- **User Confirmation:** On 2026-07-28, the user explicitly defined the
  target as clean text through Elisp APIs and requested recording only,
  with implementation deferred. On 2026-07-29, the user resumed design,
  excluded overlays, selected text properties, identified `min-width`,
  `space-width`, and `line-prefix`, requested a non-polluting hyphen
  implementation, then explicitly requested complete implementation under
  the persistent goal model. Final visible-behavior acceptance was provided
  by the user re-audit on 2026-08-13.
- **Resolved At:** 2026-08-13.
- **Resolved By:** User re-audit closure; runtime work previously implemented.
- **Commit:** — (documentation-only closure).
- **Related:** `ekp.el`, `ekp-buffer.el`, `task017`–`task022`,
  `spec_text_property_layout_engine_20260729.md`,
  `tech-refer_text_property_layout_20260729.md`,
  `postmortem/20260729-text-property-live-layout.md`.
