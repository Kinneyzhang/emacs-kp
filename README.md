# Emacs-KP: Knuth-Plass Line Breaking for Emacs

Source layout: runtime code lives in `lisp/`; add that directory to `load-path`
when using a checkout. Update older checkout configurations from the repository
root to its `lisp/` subdirectory.

[中文文档](README.zh-CN.md) | [Developer Guide](docs/architecture.md)

Emacs-kp implements the Knuth-Plass optimal line breaking algorithm with
full support for CJK (Chinese, Japanese, Korean) and Latin mixed text
typesetting, entirely inside Emacs.

## Features

- **Optimal line breaking** — the Knuth-Plass dynamic program finds the
  globally optimal set of breaks for a paragraph, not greedy first-fit.
- **CJK support** — every CJK character is a breakable box; kinsoku rules
  keep punctuation attached (`，。` never start a line, `「《` never end
  one); dedicated inter-CJK and CJK↔Latin spacing.
- **Hyphenation** — Frank Liang's algorithm (the TeX algorithm) with 49
  checksum-pinned Hunspell pattern dictionaries bundled.
- **Pixel-accurate justification** — one semantic layout plan drives both
  renderers.  The string API uses pixel spaces; buffer layout combines
  `space-width` with absolute-pixel `min-width`, so it works with
  variable-width fonts without inserting layout characters.
- **Clean editable buffers** — buffer commands create no overlays and add
  no glue spaces, soft newlines, or discretionary hyphens to the character
  stream.  `buffer-string`, `char-after`, search, syntax, save, and ordinary
  Elisp text consumers see the source characters.
- **Text properties preserved** — faces, colors and other properties
  survive justification; inserted hyphens inherit the face of the word
  they break.
- **Robust on hard input** — unprotected overlong tokens (URLs, long
  words at narrow widths) degrade to emergency breaks instead of losing
  text; every input produces output.
- **Optional C module** — a dynamic module runs the DP in C with a
  thread pool that processes paragraphs in parallel (see benchmarks).

## Requirements

- Emacs **29.1+** (uses `string-pixel-width` and `object-intervals`)
- Optional, for the C module: a C11 compiler and pthreads

## Installation

Clone the repository and add its `lisp/` directory to `load-path`.
Keep `dictionaries/` and `native/` at the repository root:

```elisp
(add-to-list 'load-path "/path/to/emacs-kp/lisp")
(require 'ekp)
(require 'ekp-buffer)   ; buffer/region commands
```

Byte-compiling is strongly recommended — the Elisp engine is about
10× faster compiled.

## Quick Start

```elisp
(require 'ekp)

;; Justify a paragraph to 600 pixels
(insert (ekp-pixel-justify "Your paragraph text here..." 600))

;; Find the best width in a range; returns (justified-text . width)
(ekp-pixel-range-justify "Your text" 400 800)
```

Multiline strings are treated as one paragraph per line; blank lines are
preserved.

### C module (recommended for long texts)

```bash
cd native && make PROFILE=portable # default; produces ekp.dylib/.so/.dll
```

```elisp
(ekp-c-module-load)     ; prints "ekp-c module loaded (version 1.6, N threads)"
(ekp-c-module-build)    ; prompts for portable/native/debug/sanitize
```

Once loaded (and since `ekp-use-c-module` defaults to `t`), all
justification calls automatically use the C engine.  The Elisp and C
engines produce **identical output**; Elisp is the always-available path
when no module is enabled or C returns no result. An enabled module signal
is surfaced as a backend contract failure. If the module on disk is older
than the Elisp code expects, loading refuses with a message asking you to
rebuild.

Automatic live append has a separate `ekp-auto-justify-native-append`
switch, enabled by default. When a compatible module is already loaded,
auto-mode may use it for the prepared append DP even if
`ekp-use-c-module` is nil; full string/buffer layout still follows
`ekp-use-c-module`. Set the new switch to nil to force pure-Elisp live
append, or when the module is unavailable it falls back automatically.

[Complete usage and interface contracts](docs/manual.md).

## Development

After cloning, run `make setup-hooks`. Before submitting a change, run `make check`; `make structure-check` is the fast organization gate. The shared rules are in [AGENTS.md](AGENTS.md).

[docs/architecture.md](docs/architecture.md) · [CHANGELOG.md](CHANGELOG.md)

`make check` runs structure checks, compilation and the public acceptance scenarios listed in [tests/acceptance.json](tests/acceptance.json). `make test` runs the broader regression suite; GUI and performance checks remain explicit targets. The acceptance inventory is selected by user-visible contracts, not by current pass/fail status.
