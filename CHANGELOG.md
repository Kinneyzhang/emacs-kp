# Changelog

All notable changes to emacs-kp are documented here.  The format
follows [Keep a Changelog](https://keepachangelog.com/), and the
project aims to follow [Semantic Versioning](https://semver.org/).

## [Unreleased]

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
