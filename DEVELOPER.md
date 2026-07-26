# Developer Documentation for Emacs-KP

This document describes the internal architecture, algorithms and APIs of
`emacs-kp`, as implemented.  It is intended for contributors and advanced
users.

## 1. Pipeline Overview

A justification call flows through five stages:

```
 string
   │
   ▼
 ① Tokenize          ekp-split-to-boxes           (ekp-utils.el)
   │                 Latin words / CJK chars / space runs → boxes,
   │                 kinsoku attachment of CJK punctuation
   ▼
 ② Hyphenate         ekp--split-with-hyphen        (ekp.el + ekp-hyphen.el)
   │                 Latin word boxes → syllable boxes (Liang patterns)
   ▼
 ③ Measure & index   ekp--make-para                (ekp.el)
   │                 pixel widths, glue types, prefix-sum arrays
   │                 → cached `ekp-para` struct
   ▼
 ④ Break (DP)        ekp--dp-run-1d / C module     (ekp.el / ekp_c/)
   │                 Knuth-Plass dynamic program → break positions
   ▼
 ⑤ Render            ekp-line-glues, ekp--pixel-justify
                     distribute glue pixels, strip edge space boxes,
                     attach hyphens → lines joined with "\n"
```

`ekp-pixel-justify` splits its input on `"\n"` and runs each non-blank
segment through this pipeline as an independent paragraph (in parallel
via the C batch API when available).

## 2. Data Structures

### `ekp-para` (the paragraph cache entry)

Everything the DP and renderer need, computed once per paragraph:

| Field | Contents |
|:------|:---------|
| `string`, `latin-font`, `cjk-font` | source text and detected fonts |
| `boxes` | vector of box strings |
| `boxes-widths` | pixel width per box (measured with deduplication) |
| `boxes-types` | `(START-TYPE . END-TYPE)` per box: `latin`/`cjk`/`cjk-punct`/`space` |
| `glues-types` | glue class *before* each box: `lws`/`mws`/`cws`/`nws` |
| `hyphen-pixel`, `hyphen-positions` | hyphen width; sorted vector of box indices after which a hyphen may be inserted |
| `ideal/min/max-prefixs` | prefix sums of box+glue widths at ideal / max-shrunk / max-stretched (n+1 elements) |
| `glue-ideals/shrinks/stretches` | leading-glue values per box index (n elements) — also passed verbatim to C |
| `lws/mws/cws-prefixs` | prefix **counts** of each stretchable glue class → O(1) gap counting per candidate line |
| `lead-spaces` | `lead-spaces[i]` = width of the space-box run starting at box i; index 0 forced to 0 (first-line indentation is kept) |
| `trail-spaces` | `trail-spaces[k]` = width of the space-box run ending at box k−1 |
| `glue-params` | plist snapshot of the nine spacing values at creation time |
| `dp-cache` | hash: line-width → dp-result plist |

The paragraph cache (`ekp--para-cache`) is keyed with `equal` on a
structured key — string content, printed text-property intervals,
detected fonts, the hyphenation language (`ekp-latin-lang`), and
either the nine explicit spacing values or the symbol `auto`.
Structured keys make hash collisions harmless (they were possible with
the previous `sxhash`-integer scheme).  The cache is flushed when it
exceeds `ekp-para-cache-limit`.  A one-entry fast path
(`ekp--last-para`, checked by string `eq` + language) covers the many
same-string lookups inside one justification call.

### dp-result

`(:rests R :gaps G :breaks B :cost C :line-count N)` where `breaks` are
exclusive end indices per line, `rests[i]` = line-width − line-ideal
(the pixels the glue must absorb), `gaps[i]` = `(lws-count mws-count
cws-count)` for glue distribution (nil for single-box and last lines).

## 3. Line Metrics

For a candidate line spanning boxes `[i, k)`:

```
raw       = prefix[k] − prefix[i] − leading-glue(i)
space-w   = min(raw, lead-spaces[i] + trail-spaces[k])
width     = raw − space-w  (+ hyphen-pixel if box k−1 hyphenates)
```

computed for ideal, min and max in O(1).  Space-box runs at the line
edges are excluded because the renderer strips them; the DP and the
renderer therefore agree exactly, and every justified line renders at
precisely the target width (`ekp-test-justify-line-width-invariant`).

## 4. The Knuth-Plass DP

`ekp--dp-run-1d` relaxes positions left to right.  For each reachable
start `i` it scans end positions `k` until the line's minimum width
exceeds the target.  A break at `k` is valid when
`min ≤ target ≤ max`, or for the last line when `ideal ≤ target`.

**Demerits** (per line, matching `ekp_c/ekp_kp.c` exactly):

```
demerits = (line-penalty + badness)²
         + penalty²                       ; hyphen-penalty at hyphen breaks
         + adjacent-fitness-penalty       ; if |fitness − prev-fitness| > 1
         + consecutive-hyphen-penalty × run²
badness  = min(10000, 100·|adjustment/flexibility|³)
```

Fitness classes (tight/decent/loose/very-loose) follow the TeX ratio
thresholds.  Special cases: single-box lines use flexibility 1 and
fitness decent; the last line pays `(line-penalty + short-badness)²`
where `short-badness = last-line-short-penalty × (1 − fill)` when the
fill ratio is below `ekp-last-line-min-ratio`.

Deviations from the 1981 paper, by design: penalties are always added
as `+p²` (no negative/flagged penalties), there is no `q`/looseness in
the main pass (see §6), and adjacent-fitness is a flat constant.

### Two-pass emergency strategy

Some inputs admit no valid layout: an unbreakable box wider than the
line, or a rigid (all-`nws`) region that cannot stretch to the target.
A strict pass runs first; if the paragraph end is unreachable, a second
pass additionally allows **emergency breaks** — single-box lines with
demerits `(line-penalty + 10000)² + rest²`, at least as bad as any
regular line.  This guarantees, by induction over positions, that every
input produces output (regression: narrow CJK used to return an empty
string), while the common case pays nothing and keeps pure K-P
optimality.  Both engines implement the identical strategy.

## 5. Rendering

`ekp-line-glues` turns each line's `rest` into per-glue pixel values:

- rest > 0 → stretch, distributed latin → mixed → CJK; CJK gaps absorb
  any leftover beyond nominal capacity (emergency spreading).
- rest < 0 → shrink, same priority order, never below the per-class
  shrink limit; glue widths are clamped at ≥ 0.
- Last lines are ragged-right (ideal glues + trailing filler);
  single-box lines get a trailing filler clamped at ≥ 0.

`ekp--pixel-justify` then strips leading space boxes (except on the
first line — indentation) and trailing space boxes, and appends a
hyphen — propertized like the word it breaks — where a line ends at a
hyphenation point.  Stripped widths are *not* redistributed: the DP
already excluded them (§3).

Glues become `(space :width (N))` display properties, so justification
is pixel-exact in GUI Emacs and column-exact in batch/tty.

The output is **lossless**: boxes are located in the source string
(`ekp--box-offsets`), and every synthesized or hidden piece records the
original text it stands for —

| property          | on                    | value / meaning                |
|-------------------|-----------------------|--------------------------------|
| `ekp-glue`        | synthesized glue space| original text it replaced      |
| `ekp-soft-break`  | inserted `\n`         | whitespace swallowed at break  |
| `ekp-soft-hyphen` | inserted hyphen       | marker only                    |
| `ekp-hidden`      | paragraph-edge text   | kept verbatim, `display ""`    |

Zero-width glue with a non-empty original renders as the hidden
original itself, so no character is ever dropped.  `ekp-region.el`
inverts these four structurally (`ekp-unjustify-region`) — exact even
after the justified text was edited — and builds
`ekp-justify-region` / `ekp-auto-justify-mode` on top.

### 5.1 Break permissions, alignment, protrusion, shapes

- **Break permissions**: every CJK char (punctuation included) is its
  own box; `ekp-para-breaks-allowed` forbids gaps per kinsoku (full-
  and halfwidth), `ekp-no-break' spans and NBSP-family joiners.
  Forbidden gaps carry no glue.  The DP skips them as candidates while
  the line keeps extending; the emergency fallback treats a run with
  no permitted inner break as atomic.  C receives the sparse
  `forbidden-positions` vector.
- **Alignment** (`ekp-alignment`): non-justify modes zero the glue
  stretch/shrink arrays and class params; the DP widens `max_w` by an
  extra per-line stretch R (`ekp-c-set-penalties` arg 7), so badness =
  100·(shortfall/R)³.  The renderer places each line's leftover per
  mode (trailing / split / leading).
- **Protrusion** (`ekp-protrusion`): per-gap `tail-protrudes[k]` (the
  last non-space box's allowance, looked through trailing spaces) and
  a `hyphen-protrude` scalar widen each candidate's effective target
  (`lw = width + release`) in the DP, in `ekp-line-glues', and in the
  C-result reconstruction — all three must stay in lockstep.
- **Per-line widths** (`ekp-parshape' / `ekp-first-line-indent'):
  resolved by `ekp--line-spec' (line-index → INDENT . WIDTH); they
  require the (position × line-count) DP and bypass C, like
  looseness.  Indents render as leading `ekp-glue' spacers.

C module 1.4: `ekp-c-break-with-arrays` takes 14 args
(…, forbidden-positions, tail-protrudes, hyphen-protrude); batch
vectors have 14 elements; `ekp-c-set-penalties` takes 4–7.

Performance after the feature wave (byte-compiled + C, Apple
Silicon, batch): justify zh w=200 ≈ 54 ms, range zh ≈ 117 ms —
justify at parity with the pre-feature numbers, range ≈ +55 % from
the larger box count.  Hot-path caches: per-char `ekp--str-type'
memo, interned glue strings, per-(paragraph, width) rendered-output
cache in the dp-cache (capped at 64 widths).  Continuous-reflow
reality check (60-paragraph, 26 k-char article, region layer
included): ≈ 73 ms per width change cold, less on revisit;
incremental single-paragraph re-justify after an edit ≈ 17 ms.

## 6. Looseness

`ekp-looseness` ≠ 0 switches to `ekp--dp-run-loose`, a full
(position × line-count) DP that keeps the best path *per line count*,
then picks the final count closest to (optimal + looseness), breaking
ties by demerits.  This is heavier than the 1D pass and is Elisp-only;
`ekp--c-available-p` returns nil while looseness is active so both
engines never disagree.

## 7. C Module Integration

The C module (`ekp_c/`, version 1.1) runs only stage ④.  Elisp remains
the source of truth for all font-dependent data.

- `ekp-c-break-with-arrays` (11 args): the para's prefix arrays, glue
  arrays, hyphen data, line width and the two space-run arrays.
  Returns `(breaks . cost)`.
- `ekp-c-break-batch`: a vector of 11-element vectors, processed in
  parallel by a pthread pool — one task per paragraph (that is the
  correct granularity; the DP itself is sequential by nature).
- `ekp-c-set-penalties` (4–6 args): called by `ekp--c-sync-params`
  before *every* C entry, so `ekp-line-penalty` & friends always take
  effect (regression: they were never synced before).
- `ekp-c-module-load` refuses modules older than
  `ekp-c-module-required-version` and falls back to Elisp, preventing
  arity mismatches after upgrades.

Any C failure (NULL result) silently falls back to the Elisp engine.
The two engines are verified to produce byte-identical output by
`ekp-test-c-parity-simple` / `ekp-test-c-parity-files`.

`ekp-c-break-lines` (C-side tokenization via `ekp_paragraph.c` and
`ekp_hyphen.c`) is an experimental, self-contained path that ekp.el
does not use; see `ekp_c/README.md`.

## 8. Hyphenation (ekp-hyphen.el)

Liang's pattern algorithm, Pyphen-compatible:

- `dictionaries/hyph_*.dic` are compiled to a pattern hash on first
  use and cached per path.  Files may be UTF-8 or ISO-8859 (Emacs
  auto-detects; verified by `ekp-test-hyphen-de-iso8859-dict`).
- `ekp-hyphen-create LANG` resolves exact codes, then progressively
  shorter prefixes (`"de_CH" → "de"`).
- Margins default to 2 characters on each side of a break.

Word boxes are matched against
`^[left-punct]* (latin-word) [right-punct]*$` so that punctuation-
wrapped words (`(word)`, `word!`, `»word«`) still hyphenate; the
punctuation stays glued to the first/last syllable box.

## 9. Testing & Benchmarks

```bash
tests/run-tests.sh [emacs]        # 36 ERT tests, batch-safe
emacs -Q --batch -L . --eval '(setq ekp-use-c-module nil)' -l tests/ekp-bench.el
emacs -Q --batch -L . --eval '(progn (require (quote ekp)) (ekp-c-module-load))' \
      -l tests/ekp-bench.el
```

Key invariants under test: rendered line width == target (pixel-exact
justification), no content loss at any width, brute-force cross-checks
of the O(1) prefix machinery, Elisp/C parity on the bundled texts, and
parameter persistence/sync regressions.

Benchmark results (batch Emacs 30.2, Apple Silicon M-series,
`tests/text-zh.txt` ≈ 3.6 KB Chinese + samples; min of 3 cold-cache
runs) — before is the pre-rewrite implementation, interpreted:

| Case                     | Before (Elisp) | After (Elisp, interpreted) | After (Elisp, compiled) | After (C) |
|:-------------------------|---------------:|---------------------------:|------------------------:|----------:|
| justify zh w=200         |        7547 ms |                    1780 ms |                   96 ms |     57 ms |
| justify zh w=400         |        2928 ms |                     815 ms |                   71 ms |     57 ms |
| justify mixed w=300      |        5540 ms |                    1275 ms |                   53 ms |     23 ms |
| range-justify zh 340–380 |       29696 ms |                    8937 ms |                  294 ms |     75 ms |
| range-justify mix 280–320|       68534 ms |                   14552 ms |                  480 ms |     34 ms |
| DP only, zh w=400        |        2382 ms |                     591 ms |                   15 ms |    1.3 ms |

("After (C)" columns measured with byte-compiled Elisp around the C
calls.  For reference, the pre-rewrite C module measured 197 ms /
430 ms / 25 ms on justify-zh-200 / range-zh / DP-only — the rewrite
also sped up the C path 3–19× via prebuilt per-para glue arrays, an
`eq' fast path in the para cache, and O(1) rest/gap reconstruction.)

The dominant wins: O(1) line metrics via prefix arrays (the old inner
loop allocated O(n) subsequences per candidate, O(n³) total), the
two-pass emergency strategy (keeps the DP sparse), box-measurement
deduplication, and per-para glue arrays reused across C calls.

## 10. File Map

```
ekp.el            Core: para struct, caching, DP (1D + looseness),
                  glue distribution, rendering, public API
ekp-utils.el      Tokenizer (boxes, kinsoku), font detection with
                  batch/tty fallbacks, C module loading
ekp-hyphen.el     Liang hyphenation + dictionary registry
ekp_c/            C dynamic module (see ekp_c/README.md)
dictionaries/     Hunspell hyphenation patterns (from Pyphen)
tests/            ekp-tests.el (ERT), ekp-bench.el, ekp-demo.el,
                  sample texts, run-tests.sh
```
