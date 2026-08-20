# Emacs-KP: Knuth-Plass Line Breaking for Emacs

[中文文档](./readme_zh.md) | [Developer Guide](./DEVELOPER.md) | [Repository Audit](./Docs/REPOSITORY_AUDIT_20260728.md)

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

Clone the repository and add it to your `load-path` (the
`dictionaries/` directory must sit next to the `.el` files):

```elisp
(add-to-list 'load-path "/path/to/emacs-kp")
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
cd ekp_c && make PROFILE=portable # default; produces ekp.dylib/.so/.dll
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

## Interactive Use (buffer & region)

`ekp-buffer.el` turns the string API into buffer-level commands:

```elisp
(require 'ekp-buffer)
```

- `M-x ekp-justify-region` — justify the region to the window text
  width (with a numeric prefix argument, to that many pixels).  With
  no active region, it justifies the paragraph at point.
- `M-x ekp-justify-buffer` — justify the whole buffer.
- `M-x ekp-unjustify-region` / `ekp-unjustify-buffer` — remove EKP's
  display projection.  The source text does not need restoration because
  buffer layout never replaced it.
- `M-x ekp-auto-justify-mode` — keep completed hard paragraphs justified
  while ordinary typing remains stable. The active hard line has a committed
  projection plus one local edit transaction. Typing within the same native
  visual row does no whole-line planning and leaves unaffected projected rows
  untouched. Editing a projected middle row naturalizes only that row's dirty
  island, so later break anchors stay in place and native wrapping handles
  local word migration. When input naturally crosses into the next visual
  row, EKP runs or reuses one complete `ekp-layout-plan` and atomically
  publishes all completed rows; the new row remains natural. Deleting and
  reinserting the same source restores the saved projection exactly,
  including text properties.
  Point motion never plans or writes layout properties, even when point
  leaves the paragraph. Global commits happen only at a visual-row crossing,
  hard newline/paragraph completion, the next real edit elsewhere, explicit
  refill, or a width/font/layout-context change. There is no edit-idle
  whole-paragraph snap. Window resize re-flow is debounced by
  `ekp-auto-justify-resize-delay`.
  The mode temporarily disables both explicit line truncation and Emacs's
  narrow partial-window truncation, so a side-by-side editing window still
  soft-wraps normally. Disabling the mode restores the prior buffer-local
  or global ownership of both settings.
  A leading or trailing space/tab on the active line is visible in the
  same input turn; deleting a following glyph does not hide that source
  whitespace. Reprojection also preserves an inactive mark as inactive, so
  width changes do not create an accidental selection.
  While active, the standard **EKP** menu exposes formatting, protection,
  and window-fit diagnostic commands; `C-h m` describes the same workflow.

The projection uses text properties on existing source graphemes only:

- Existing ASCII spaces receive
  `((space-width FACTOR) (min-width ((TARGET-PIXELS))))`.
- A CJK or mixed gap with no source space adds `min-width` to the preceding
  complete grapheme; the target is its natural advance plus the glue.
- `line-prefix` supplies indentation.  A break or discretionary hyphen is
  a replacing display string on an existing complete grapheme.
- EKP never creates an overlay.  It also never steals a foreign replacing
  `display`, `line-prefix`, `wrap-prefix`, `composition`, or `invisible`
  owner; that hard paragraph stays natural and `M-x ekp-diagnose` reports
  the conflict.

Consequently:

- **Elisp APIs and saving** see the original character sequence.  Visual
  spaces, newlines, and hyphens cannot reach disk or syntax/search logic.
  `buffer-substring` can still carry the EKP display properties because it
  preserves text properties; `buffer-substring-no-properties` is the plain
  source string.
- **Searching** (including isearch) operates directly on source text, so a
  word remains one word across a visual discretionary break.
- **Copying and killing** strip EKP-owned projection properties while
  composing with any existing substring filter.  Pasted text contains only
  the logical content and its non-EKP properties.
- Projection updates run inside `with-silent-modifications`: enabling,
  editing, resizing, and disabling layout do not create layout-only undo
  entries, modified-state changes, or character-modified ticks.

`ekp-buffer-margin-pixel` (default 2) is subtracted from the window
width as a rounding safety margin.

Large buffers (over `ekp-auto-justify-lazy-threshold` characters,
default 20 000) re-flow visible-first: the portion on screen updates
synchronously and the rest follows in idle background chunks, with a
per-tick time budget (`ekp-auto-justify-tick-budget`) and priority
for whatever you scroll to.

Automatic planning is also bounded per hard paragraph.
`ekp-auto-justify-paragraph-limit` defaults to 2 048 characters.  A longer
single paragraph stays naturally wrapped and fully editable instead of
blocking input in an unbounded Knuth-Plass pass; `M-x ekp-diagnose` reports
the reason.  Run `M-x ekp-refill-paragraph` when you explicitly want the
unbounded full-quality pass for that paragraph.

Mode presets for explicit local protection — one call each:

```elisp
(add-hook 'org-mode-hook      #'ekp-org-setup)
(add-hook 'markdown-mode-hook #'ekp-markdown-setup)
```

`ekp-auto-justify-mode` consults `ekp-buffer-mode-policy-alist`
automatically in Org and Markdown buffers.  It does not copy profile
values into buffer-local variables unless you explicitly call the setup
functions above.

### Protecting code and other verbatim text

- Block level: paragraphs carrying the `ekp-verbatim` text property
  (`M-x ekp-verbatim-region`), wearing a face listed in
  `ekp-buffer-skip-faces` (e.g. `org-block`, `markdown-code-face`), or
  matched by the buffer-local function `ekp-buffer-skip-predicate`
  pass through completely untouched.
- Automatic inline level: faces listed in `ekp-buffer-inline-faces`
  (Org `org-code`/`org-verbatim`, Markdown inline code by default through
  mode profiles) use `ekp-inline-code-policy`.  The default `no-hyphen`
  keeps source spaces literal and suppresses dictionary hyphenation, but it
  may still wrap at legal source boundaries.  Automatic `no-break` spans
  downgrade to `no-hyphen` when wider than the effective measure.
- Explicit hard atom level: spans carrying `ekp-no-break`
  (`M-x ekp-no-break-region`) become rigid atoms — never broken,
  never hyphenated, spacing kept literal — ideal for inline code,
  product names, or numbers with units.  An atom wider than the measure stays
  intact but is not guaranteed a line of its own: the final pass may place it
  on one overflow line with preceding ordinary content. Ordinary underfull
  candidates use fixed emergency stretch and normal K-P costs. If an
  overfull candidate would otherwise extinguish every active final-pass path,
  the core preserves the last path with TeX-style artificial demerits. There
  is no CJK-orphan, unit, or screenshot-specific rule.

Manual properties are deliberately **current-buffer-session only**:
plain-text saving and reopening do not persist them. Use
`M-x ekp-allow-break-region` / `ekp-clear-verbatim-region` to remove them.
For protection derived from persistent document syntax, use mode faces or
the buffer-local `ekp-buffer-skip-predicate` (the Org/Markdown presets do
this automatically).

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
  per-line `(INDENT . WIDTH)`.  First-line indent runs on the fast 1D
  path and the C engine; only full `ekp-parshape` and `ekp-looseness`
  fall back to the Elisp-only 2D dynamic program.
- **Unbreakables** — NO-BREAK SPACE, NARROW NBSP, FIGURE SPACE and
  WORD JOINER characters keep their neighbors together out of the box.
- Kinsoku covers full- *and* halfwidth punctuation: a line never
  starts with `。、」!?` or a lone `.,;:!?`, never ends with `「(` etc.
  Japanese line-start prohibition also covers small kana, the
  prolonged sound mark and iteration marks (`っ ょ ー 々`), configurable
  via `ekp-cjk-no-line-start-extra`.

Limitations worth knowing: mid-line CLREQ punctuation *compression*
(e.g. 「字。下」 squeezed inside a line) cannot be rendered — Emacs
cannot shrink a glyph's advance — which is why line-edge compression
is delivered via protrusion instead; left-edge protrusion is likewise
not renderable (text cannot start before the line origin).

## Configuration

### Break policy and measure

The break-policy defaults are intended to make code readable without making
every code-looking span a hard atom:

| Option | Default | Scope | Effect |
|:-------|:--------|:------|:-------|
| `ekp-inline-code-policy` | `no-hyphen` | global, profile, local | `normal`, `no-hyphen`, or automatic fitting `no-break` for inline-face spans |
| `ekp-hyphenation` | `auto` | global, profile, local, region via command | `auto`/`on` use dictionaries when available; `off` suppresses discretionary hyphens |
| `ekp-token-break-policies` | URL/path/identifier `no-hyphen`, number-unit `no-break` | global, profile, local | per-token automatic policies; local/profile values merge by token category |
| `ekp-number-unit-suffixes` | common CSS, time, data, frequency, and metric units | global, profile, local | suffixes recognized by the compact number-unit classifier |
| `ekp-kinsoku-profile` | `common` | global, profile, local | `common`, `zh`, `ja`, `off`, or `custom` CJK line-start/end prohibitions |
| `ekp-cjk-no-line-start-extra` / `ekp-cjk-no-line-end-extra` | `""` | global, profile, local | additions used by the `custom` profile |
| `ekp-overlong-token-policy` | `emergency` | global, profile, local | `emergency`, `overflow`, or `natural` for ordinary overlong Latin-like tokens |
| `ekp-buffer-measure` | `narrowest-window` | global, profile, local | `narrowest-window`, fixed pixel integer, or `(max . PIXELS)` |
| `ekp-buffer-skip-faces` | profile-dependent | global, profile, local | paragraph-level verbatim faces |
| `ekp-buffer-inline-faces` | profile-dependent | global, profile, local | exact inline spans using `ekp-inline-code-policy` |
| `ekp-buffer-mode-policy-alist` | Org and Markdown profiles | global/local safe value | mode profiles consulted by automatic and manual buffer layout |

Effective precedence is deterministic: explicit region text properties
first, then explicit buffer/file/dir-local values, then the first matching
major-mode profile, then global defaults.  `ekp-break-policy` region values
are `normal`, `hyphenate`, and `no-hyphen`; they override automatic token
or inline policies for the exact region but never create a hard atom.
`ekp-no-break` remains the only explicit hard-atom property and wins over
every automatic policy.

All listed variables have closed safe-local predicates where file/dir local
configuration is supported.  `ekp-diagnose` reports the requested measure,
the narrowest live window, the effective measure, overflow risk, conflict
count, and the active inline/hyphenation/kinsoku/overlong policy summary.
The EKP menu exposes diagnose plus region commands for normal break,
hyphenation on, hyphenation off, clear break policy, no-break, and verbatim.

### Hyphenation language

```elisp
(setq ekp-latin-lang "de_DE")   ; default "en_US"
```

Short codes like `"de"` resolve to the first matching dictionary.  Each
supported dictionary's own
`LEFTHYPHENMIN` / `RIGHTHYPHENMIN` are honored (English keeps ≥2
letters before and ≥3 after a break); pass explicit margins to
`ekp-hyphen-create` to override.

EKP supports ordinary Liang patterns.  It fails closed with
`ekp-hyphen-unsupported-pattern` for `eo`, `ca`, `hu_HU`, and `sq_AL`
because those files contain slash/replacement rules that conditionally
rewrite glyphs at a chosen break. Treating them as ordinary positions would
produce linguistically wrong text and incorrect DP widths.  The exact
inventory, SHA-256 checksums, pinned source paths, and license evidence live
in `dictionaries/MANIFEST.tsv` and `dictionaries/LICENSES.md`.

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

Both engines implement these parameters: before an actual C computation,
the Elisp side synchronizes their current values.  The DP cache signature
includes every parameter in the table, so changes take effect on the next
call without manually clearing caches.
`ekp-looseness` is part of the cache key and uses a dedicated Elisp path
(the C module is bypassed automatically while it is non-zero).

### Caching

Tokenization, measurement, and DP results are cached per paragraph;
box widths are additionally cached session-wide, so a glyph shared
across paragraphs is measured only once.
Both explicit spacing values and the automatic
`ekp-default-cws-stretch-pixel` input participate in paragraph cache
identity. The same complete structural key also governs the same-string
fast path, so adding or removing layout properties such as `ekp-no-break`
takes effect immediately on an already cached string object.

- `ekp-para-cache-limit` (default 256): max cached paragraphs; the
  cache is flushed when the limit is reached.
- `M-x ekp-clear-caches` clears everything (use after changing fonts or
  themes that affect glyph widths).

## Performance

Measured on the bundled sample texts (`tests/ekp-bench.el`), batch
Emacs 30.2, Apple Silicon; see DEVELOPER.md for methodology:

| Case (text-zh.txt ≈ 3.6 KB)  | Elisp (byte-compiled) | C module |
|:-----------------------------|----------------------:|---------:|
| justify, width 200px         |                150 ms |    41 ms |
| optimal-width search 340–380 |                529 ms |   106 ms |
| DP only, width 400px         |                 30 ms |   2.5 ms |

**Byte-compile the package** — the Elisp engine is ~10× faster
compiled.  Both engines produce identical output; the C module pays
off most for optimal-width search and long multi-paragraph texts.
(Absolute numbers vary with machine and power state; the ratios are
the point.)

## Known Limitations

- Text properties are buffer-wide, so one buffer cannot carry independent
  plans for windows of different widths.  EKP uses the narrowest live
  window as the authoritative width; wider windows can show unused space
  but never overflow.
- EKP-owned layout properties are visible to APIs that explicitly inspect
  text properties.  The character stream is clean; copy/kill removes the
  owned projection metadata.
- Tabs and non-ASCII whitespace cannot be shrunk with `space-width`.  If an
  exact plan would require that operation, EKP leaves the affected hard
  paragraph natural and reports the conflict.
- Measurement follows the current buffer's face remappings
  (`text-scale-mode`, themes, `ekp-org-setup`-style tweaks) and
  reserves the truncation-indicator column in windows without
  fringes, so justified lines fit the real display.  If lines ever
  look truncated or short in an exotic setup, run `M-x ekp-diagnose`
  in that buffer — it reports whether measurement matches rendering.
  The full fit matrix is a developer tool in `tests/ekp-gui-verify.el`;
  load that file before invoking `M-x ekp-gui-verify`.
- One font is assumed per Latin/CJK script per paragraph when computing
  spacing defaults; mixed-font paragraphs work but spacing defaults come
  from the first font found.
- `ekp-pixel-range-justify` minimizes average demerits with a ternary
  search plus a local scan; cost is not perfectly unimodal in width, so
  the result is a very good, but not guaranteed global, optimum.
- In batch/tty Emacs, pixel widths degrade to character columns (the
  full pipeline still works; useful for testing).

## Interactive Demo

```bash
emacs -Q -L /path/to/emacs-kp -l tests/ekp-showcase.el -f ekp-showcase
```

One buffer, live keys: `-`/`+` change the pixel width (per-reflow time
in the header line), `d` runs an animated width sweep with an fps
report, `a` cycles alignment, `p` toggles hanging punctuation, `i`
first-line indent, `s` a wedge parshape, `c` compares the C engine
with pure Elisp, `w` follows the window width via
`ekp-auto-justify-mode`.  The sample text includes a protected code
block, an inline no-break atom and NBSP-joined numbers.

## Testing

```bash
tests/run-tests.sh /path/to/emacs     # batch-safe ERT suite

# Full interactive GUI fit matrix
emacs -Q -L /path/to/emacs-kp -L /path/to/emacs-kp/tests \
  -l /path/to/emacs-kp/tests/ekp-gui-verify.el \
  -f ekp-gui-verify-matrix
```

The matrix prints every row and exits with status 1 if any fit check fails,
so the same command can gate local release automation. The verifier is a
developer tool under `tests/`; it is not loaded by `(require 'ekp-buffer)`.

## Credits

- **Core algorithm**: ["Breaking Paragraphs into Lines"](https://gwern.net/doc/design/typography/tex/1981-knuth.pdf) by Donald E. Knuth and Michael F. Plass (1981)
- **Hyphenation**: Frank Liang's algorithm, adapted from [Pyphen](https://github.com/Kozea/Pyphen)
- **Dictionaries**: hyphenation patterns from the [LibreOffice dictionaries](https://github.com/LibreOffice/dictionaries); see `dictionaries/MANIFEST.tsv`, `dictionaries/LICENSES.md`, and the bundled per-dictionary notices for exact source, checksum, and license evidence.
