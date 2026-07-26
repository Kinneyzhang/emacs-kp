# Emacs-KP: Knuth-Plass Line Breaking for Emacs

[中文文档](./readme_zh.md) | [Developer Guide](./DEVELOPER.md)

Emacs-kp implements the Knuth-Plass optimal line breaking algorithm with
full support for CJK (Chinese, Japanese, Korean) and Latin mixed text
typesetting, entirely inside Emacs.

## Features

- **Optimal line breaking** — the Knuth-Plass dynamic program finds the
  globally optimal set of breaks for a paragraph, not greedy first-fit.
- **CJK support** — every CJK character is a breakable box; kinsoku rules
  keep punctuation attached (`，。` never start a line, `「《` never end
  one); dedicated inter-CJK and CJK↔Latin spacing.
- **Hyphenation** — Frank Liang's algorithm (the TeX algorithm) with 70+
  Hunspell pattern dictionaries bundled.
- **Pixel-accurate justification** — every justified line renders at
  exactly the requested pixel width, using `display (space :width ...)`
  properties; works with variable-width fonts.
- **Text properties preserved** — faces, colors and other properties
  survive justification; inserted hyphens inherit the face of the word
  they break.
- **Robust on hard input** — unbreakable overlong tokens (URLs, long
  words at narrow widths) degrade to emergency breaks instead of losing
  text; every input produces output.
- **Optional C module** — a dynamic module runs the DP in C with a
  thread pool that processes paragraphs in parallel (see benchmarks).

## Requirements

- Emacs **29.1+** (uses `string-pixel-width` and `object-intervals`)
- Optional, for the C module: a C11 compiler and pthreads

## Quick Start

```elisp
(add-to-list 'load-path "/path/to/emacs-kp")
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
cd ekp_c && make        # requires C11 compiler, produces ekp.dylib/.so/.dll
```

```elisp
(ekp-c-module-load)     ; prints "ekp-c module loaded (version 1.4, N threads)"
```

Once loaded (and since `ekp-use-c-module` defaults to `t`), all
justification calls automatically use the C engine.  The Elisp and C
engines produce **identical output**; Elisp is the always-available
fallback.  If the module on disk is older than the Elisp code expects,
loading refuses with a message asking you to rebuild.

## Interactive Use (buffer & region)

`ekp-region.el` turns the string API into buffer-level commands:

```elisp
(require 'ekp-region)
```

- `M-x ekp-justify-region` — justify the region to the window text
  width (with a numeric prefix argument, to that many pixels).
- `M-x ekp-unjustify-region` — restore the original text **exactly**,
  including collapsed whitespace runs.  Justification is lossless: every
  synthesized space, soft line break, and soft hyphen carries the
  original text it replaced, so restoring is a structural transform that
  also works after you edited the justified text.
- `M-x ekp-auto-justify-mode` — keep the whole buffer justified to the
  window width.  Re-flows (debounced by
  `ekp-auto-justify-resize-delay`) when the window width changes, and
  after edits re-justifies only the touched paragraphs
  (`ekp-auto-justify-edit-delay`), so unchanged paragraphs hit the
  paragraph cache.  Turning the mode off restores the buffer exactly.

`ekp-region-margin-pixel` (default 2) is subtracted from the window
width as a rounding safety margin.

Large buffers (over `ekp-auto-justify-lazy-threshold` characters,
default 20 000) re-flow visible-first: the portion on screen updates
synchronously (~15 ms) and the rest follows in idle background chunks.

Mode presets for verbatim protection:

```elisp
(add-hook 'org-mode-hook
          (lambda ()
            (setq-local ekp-region-skip-faces ekp-region-org-skip-faces)))
(add-hook 'markdown-mode-hook
          (lambda ()
            (setq-local ekp-region-skip-faces ekp-region-markdown-skip-faces)))
```

### Protecting code and other verbatim text

- Block level: paragraphs carrying the `ekp-verbatim` text property
  (`M-x ekp-verbatim-region`), wearing a face listed in
  `ekp-region-skip-faces` (e.g. `org-block`, `markdown-code-face`), or
  matched by the buffer-local function `ekp-region-skip-predicate`
  pass through completely untouched.
- Inline level: spans carrying `ekp-no-break`
  (`M-x ekp-no-break-region`) become rigid atoms — never broken,
  never hyphenated, spacing kept literal — ideal for inline code,
  product names, or numbers with units.

## Typography

- **Alignment** — `ekp-alignment`: `justify` (default),
  `ragged-right`, `ragged-left`, or `center`.  Non-justify modes keep
  word spacing natural while Knuth-Plass still minimizes raggedness
  within `ekp-ragged-stretch-pixel` (≈2 em) per line.
- **Hanging punctuation** — set `ekp-protrusion` to `t` and line-final
  punctuation (。、」 as well as periods, commas and break hyphens)
  hangs past the flush edge by `ekp-protrusion-ratios`.  The 0.5
  default for fullwidth closers is visually equivalent to CLREQ
  line-end punctuation compression.  `ekp-auto-justify-mode` reserves
  the protrusion width automatically.
- **Paragraph shapes** — `ekp-first-line-indent` (`t` = 2 em) for the
  CJK paragraph convention, or full TeX-style `ekp-parshape` with
  per-line `(INDENT . WIDTH)`.  Both are Elisp-only paths (the C
  module is bypassed, as with `ekp-looseness`).
- **Unbreakables** — NO-BREAK SPACE, NARROW NBSP, FIGURE SPACE and
  WORD JOINER characters keep their neighbors together out of the box.
- Kinsoku covers full- *and* halfwidth punctuation: a line never
  starts with `。、」!?` or a lone `.,;:!?`, never ends with `「(` etc.

Limitations worth knowing: mid-line CLREQ punctuation *compression*
(e.g. 「字。下」 squeezed inside a line) cannot be rendered — Emacs
cannot shrink a glyph's advance — which is why line-edge compression
is delivered via protrusion instead; left-edge protrusion is likewise
not renderable (text cannot start before the line origin).

## Configuration

### Hyphenation language

```elisp
(setq ekp-latin-lang "de_DE")   ; default "en_US"
```

Any `dictionaries/hyph_<lang>.dic` works; short codes like `"de"`
resolve to the first matching dictionary.

### Spacing parameters

Three glue classes control spacing (all values in pixels):

| Group   | Between                    |
|:--------|:---------------------------|
| `lws-*` | two Latin words            |
| `mws-*` | a Latin word and a CJK char|
| `cws-*` | two CJK characters         |

Each class has an ideal width, a maximum stretch and a maximum shrink:

```elisp
(ekp-param-set lws-ideal lws-stretch lws-shrink
               mws-ideal mws-stretch mws-shrink
               cws-ideal cws-stretch cws-shrink)
;; e.g. (ekp-param-set 7 3 2  5 2 1  0 2 0)
```

- If you never call `ekp-param-set`, defaults are derived automatically
  from the font of each string.
- Explicit parameters **persist** until you call `ekp-param-reset`,
  which returns to automatic per-string defaults.

### Algorithm parameters

| Variable                        | Default | Meaning |
|:--------------------------------|:--------|:--------|
| `ekp-line-penalty`              | 10      | Base cost per line; higher prefers fewer lines |
| `ekp-hyphen-penalty`            | 50      | Cost of a hyphenated break (added as penalty²) |
| `ekp-adjacent-fitness-penalty`  | 100     | Cost when adjacent lines differ in tightness by >1 class |
| `ekp-consecutive-hyphen-penalty`| 100     | Multiplier for runs of hyphenated lines (× count²) |
| `ekp-last-line-min-ratio`       | 0.5     | Minimum fill ratio for the last line |
| `ekp-last-line-short-penalty`   | 50      | Cost multiplier for a too-short last line |
| `ekp-looseness`                 | 0       | Target line count offset: +1 = one line more than optimal, −1 = one fewer |

All parameters take effect with both engines: the Elisp side syncs them
to the C module before every call.  `ekp-looseness` is handled by a
dedicated Elisp path (the C module is bypassed automatically while it
is non-zero).

### Caching

Tokenization, measurement, and DP results are cached per paragraph.

- `ekp-para-cache-limit` (default 256): max cached paragraphs; the
  cache is flushed when the limit is reached.
- `M-x ekp-clear-caches` clears everything (use after changing fonts or
  themes that affect glyph widths).

## Performance

Measured on the bundled sample texts (`tests/ekp-bench.el`), batch
Emacs 30.2, Apple Silicon; see DEVELOPER.md for methodology:

| Case (text-zh.txt ≈ 3.6 KB)  | Elisp (byte-compiled) | C module |
|:-----------------------------|----------------------:|---------:|
| justify, width 200px         |                 96 ms |    57 ms |
| optimal-width search 340–380 |                294 ms |    75 ms |
| DP only, width 400px         |                 15 ms |   1.3 ms |

**Byte-compile the package** — the Elisp engine is ~10× faster
compiled.  Both engines produce identical output; the C module pays
off most for optimal-width search and long multi-paragraph texts.

## Known Limitations

- Widths are computed from the string's own text properties. If the
  destination buffer remaps faces (different `:height`, themes), widths
  may differ; justify with the same properties you will display.
- One font is assumed per Latin/CJK script per paragraph when computing
  spacing defaults; mixed-font paragraphs work but spacing defaults come
  from the first font found.
- `ekp-pixel-range-justify` minimizes average demerits with a ternary
  search plus a local scan; cost is not perfectly unimodal in width, so
  the result is a very good, but not guaranteed global, optimum.
- In batch/tty Emacs, pixel widths degrade to character columns (the
  full pipeline still works; useful for testing).

## Testing

```bash
tests/run-tests.sh /path/to/emacs     # 67 ERT tests, all batch-safe
```

## Credits

- **Core algorithm**: ["Breaking Paragraphs into Lines"](https://gwern.net/doc/design/typography/tex/1981-knuth.pdf) by Donald E. Knuth and Michael F. Plass (1981)
- **Hyphenation**: Frank Liang's algorithm, adapted from [Pyphen](https://github.com/Kozea/Pyphen)
- **Dictionaries**: [Hunspell hyphenation patterns](https://github.com/Kozea/Pyphen)
