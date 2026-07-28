# Changelog

All notable changes to emacs-kp are documented here.  The format
follows [Keep a Changelog](https://keepachangelog.com/), and the
project aims to follow [Semantic Versioning](https://semver.org/).

## [Unreleased]

### Fixed

- Changing any Knuth-Plass cost parameter now selects a correctly keyed
  DP/render result immediately; cached paragraphs no longer require
  `ekp-clear-caches`. Structurally equal non-zero-looseness signatures now
  reuse their cached result instead of missing an `eql` cons key.
- Changing `ekp-default-cws-stretch-pixel` in automatic spacing mode now
  invalidates both paragraph-cache lookup paths immediately. Unchanged
  spacing signatures still reuse the cached paragraph.
- Saving justified buffers now writes from a logical-text copy instead of
  temporarily unformatting the display buffer. Filesystem, encoding, and
  interruption failures therefore leave the visible layout intact and
  retryable.
- Kill/copy integration now composes with and restores an existing
  `filter-buffer-substring-function`, including DELETE operations. Removing
  the final layout span outside auto mode also removes unused save/search/
  copy integrations.
- Dictionary syntax no longer degrades silently: files containing
  libhyphen replacement/slash patterns fail with an explicit condition
  because the fixed-width DP cannot model their conditional rewrites.
- Mutating layout-relevant text properties on an already cached string now
  invalidates the same-string paragraph fast path. It reuses the complete
  structural cache key instead of a partial parallel signature.

### Changed

- The bundled dictionary inventory is now 49 reproducible entries with a
  pinned LibreOffice commit, per-file SHA-256/source/license manifest, and a
  portable verifier/exporter. Sanskrit was removed because the pinned
  upstream data has no authoritative license statement for that exact
  hyphenation file.
- Auto-justify mode now exposes its existing formatting, protection, and
  diagnostic commands in an EKP menu. Interactive no-break/verbatim changes
  report that their text properties last only for the current buffer
  session.

### Tests

- ERT fixtures now restore every mutable EKP option they isolate. The
  parshape C-bypass regression drives the public formatter, and reusable
  permuted-order plus per-test fresh-process runners prevent alphabetical
  execution from hiding leaked state.
- The GUI fit matrix now exits with status 1 when any row fails. A
  batch-safe forced-failure control locks the automation contract while the
  real seven-case matrix remains an explicitly loaded developer tool.
- Release invariants now have one local gate. CI action inputs are immutable,
  Windows runs the Elisp baseline, and the `.phrase` decision source is
  versionable instead of ignored.

### Performance

- Tokenization now accumulates fragments and joins once per emitted box;
  dense hyphen insertion likewise joins original word slices once.  On the
  1,000–8,000-character adversarial benchmark, the 8,000-character cases
  improved from 3.133 s to 1.100 s and from 0.945 s to 0.013 s respectively.
- Cached “no hyphen” results now use an explicit miss sentinel, so repeated
  lookups do not rerun the dictionary computation.

### Internal

- DP reuse identity, edge-space exclusion, and the lossless marker
  vocabulary now each have one directly tested rule owner. The consolidation
  removes formula/property-list drift without adding modules or hot-loop
  allocations.
- Final cleanup removed nine dead pass-through accessors and replaced direct
  use of Emacs's private substring-filter dispatcher with the public
  `filter-buffer-substring` path while preserving DELETE lifecycle ownership.
- The paragraph fast path no longer needs style-variable watchers: both the
  one-entry path and hash cache have one structural identity owner.

### C module (1.6)

- The 15-field single and batch APIs now preflight vector shape, lengths,
  scalar types, and signed 32-bit input range before extraction. Caller
  errors signal `ekp-c-invalid-input`; allocation/no-result still returns
  nil. Enabled-backend signals are no longer swallowed by the Elisp
  dispatcher.
- Line metric, flexibility, and remaining-space intermediates are now
  64-bit, eliminating signed overflow when valid int32 widths and
  protrusions combine.
- The default C build is portable (`PROFILE=portable`). Native CPU/LTO,
  debug, and sanitizer flags are explicit profiles, and
  `ekp-c-module-build` now passes argv directly to `make` from the module
  directory instead of interpolating a shell command.

## [1.0.0] - 2026-07-27

First tagged release.  Highlights of the work leading up to it:

### Editor integration

- Saving an auto-justified buffer now writes the **logical** text to
  disk; soft line breaks, glue spaces and break hyphens are layout,
  not content, and no longer reach the file.
- `isearch` searches the logical text, so CJK phrases and hyphenated
  words are found across the justified layout.
- Copying/killing puts the logical text on the kill ring (via a
  buffer-local `filter-buffer-substring-function`).
- Justification no longer flips `buffer-modified-p` on an unmodified
  buffer — no stray lock files or auto-saves — and re-flow no longer
  fights `undo`.
- Text typed adjacent to synthesized glue/hyphens no longer inherits
  the marker properties (`rear-nonsticky`), so it is never mistaken
  for layout and deleted on the next re-flow.
- Switching major mode or turning the minor mode off restores the
  logical text cleanly, widening first so narrowing leaves no
  justified orphans.
- Paragraphs containing `field`/`read-only` text (comint prompts) are
  left verbatim.

### New commands and options

- `ekp-justify-buffer` / `ekp-unjustify-buffer`.
- `ekp-justify-region` / `ekp-unjustify-region` act on the paragraph
  at point when no region is active (DWIM).
- `ekp-org-setup` / `ekp-markdown-setup` wire the verbatim-protection
  presets in one call; `ekp-auto-justify-mode` applies the matching
  one automatically in Org/Markdown buffers.
- `fill-paragraph` is remapped to `ekp-refill-paragraph` while the
  mode is on.
- User options are now `defcustom`s under the `ekp` / `ekp-region`
  groups.
- `ekp-cjk-no-line-start-extra`: Japanese line-start prohibition for
  small kana, the prolonged sound mark and iteration marks (JIS X
  4051).
- `ekp-auto-justify-tick-budget`: time budget per background re-flow
  tick.

### Typography

- First-line indent (`ekp-first-line-indent`) now runs on the fast 1D
  dynamic program and the C engine, instead of the heavy 2D path.
- Dictionary `LEFTHYPHENMIN` / `RIGHTHYPHENMIN` are honored (English
  now keeps ≥2 before and ≥3 after a break, per its dictionary).

### Performance

- Session-global box-width cache eliminates cross-paragraph duplicate
  measurement (≈40% of `string-pixel-width` calls on the Chinese
  sample); byte-compiled justify of that sample dropped ~40%.
- Cache keys ignore volatile `fontified` bookkeeping, restoring the
  paragraph-cache hit rate in font-locked buffers.
- Large-buffer re-flow: huge edits chunk in the background, scrolled
  regions get priority, background ticks honor a time budget.

### Fixed

- The DP cache no longer returns a stale layout after `ekp-looseness`
  changes (results are keyed by looseness).
- Changing a style variable (`ekp-alignment`, …) invalidates the
  same-string fast path.
- Missing `dictionaries/` directory only disables hyphenation instead
  of breaking `(require 'ekp)`.

### C module (1.4 → 1.5)

- **Breaking:** the unused experimental C tokenization path
  (`ekp-c-break-lines`, `ekp-c-hyphenate`, `ekp-c-load-hyphenator`,
  `ekp-c-set-spacing`, and the `ekp_paragraph.c` / `ekp_hyphen.c`
  sources) is removed.  It carried a heap overflow reachable from
  Lisp.  Rebuild with `make -C ekp_c clean all`.
- `ekp-c-break-with-arrays` gains a 15th argument, `FIRST-LINE-WIDTH`,
  supporting first-line indent in C.
- Thread pool sized to the machine's cores, created lazily; a full
  queue blocks instead of dropping tasks.
- Allocation failures and bad arguments fail the call cleanly (Elisp
  fallback) instead of silently producing a different layout;
  extracted integers are clamped to int32.

### Packaging

- Added `COPYING` (GPL-3.0-or-later) and license headers to all
  sources; real author/maintainer metadata.
- Autoload cookies on all interactive commands.
- Removed internal working files and an unused demo GIF from the repo.

## Historical

Earlier work (the core KP overhaul and the P1/P2 typography wave —
optimal line breaking, CJK kinsoku, hyphenation, pixel-exact
justification, alignment modes, hanging punctuation, parshape,
no-break atoms, verbatim protection, the C module) predates this
changelog; see the git history.
