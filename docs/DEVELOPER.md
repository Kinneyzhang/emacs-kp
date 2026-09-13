# Developer Documentation for Emacs-KP

This document describes the internal architecture, algorithms and APIs of
`emacs-kp`, as implemented.  It is intended for contributors and advanced
users.

Current repository health and prioritized follow-up work are recorded in
the [2026-07-28 system audit](./Docs/REPOSITORY_AUDIT_20260728.md).

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
| `dp-cache` | equal-keyed hash: complete DP signature → dp-result plist |

The paragraph cache (`ekp--para-cache`) is keyed with `equal` on a
structured key — string content, printed text-property intervals,
detected fonts, the hyphenation language (`ekp-latin-lang`), and
either the nine explicit spacing values or the automatic CJK stretch
default. The other automatic values are derived from font measurement,
which is already represented by the font and display-context fields.
Structured keys make hash collisions harmless (they were possible with
the previous `sxhash`-integer scheme).  The cache is flushed when it
exceeds `ekp-para-cache-limit`.  A one-entry fast path
(`ekp--last-para`) bypasses only the hash lookup: it compares the same
complete structural key before reuse. In-place changes to layout-relevant
text properties therefore miss both paths and match a fresh paragraph.

The DP signature is separate from the paragraph key.  It contains line
width, looseness, and all six runtime cost parameters; therefore a
parameter change selects a new result without flushing width-independent
paragraph data.  Structural `equal` comparison also lets non-zero
looseness signatures hit the cache.

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
`ekp--line-stripped-space-pixel` owns this exclusion rule for the 1D/loose
DP, C-result reconstruction, and renderer.

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
thresholds.  Special cases: single-box lines use flexibility 1 in the
strict pass; the final pass uses the same finite emergency stretch as
ordinary underfull lines.  The last line pays
`(line-penalty + short-badness)²` where
`short-badness = last-line-short-penalty × (1 − fill)` when the fill ratio
is below `ekp-last-line-min-ratio`.

Deviations from the 1981 paper, by design: penalties are always added
as `+p²` (no negative/flagged penalties), there is no `q`/looseness in
the main pass (see §6), and adjacent-fitness is a flat constant.

### Two-pass emergency strategy

Some inputs admit no valid layout: an unbreakable box wider than the
line, or a rigid (all-`nws`) region that cannot stretch to the target.
A strict pass runs first; if the paragraph end is unreachable, a second
pass adds a finite background emergency stretch to ordinary underfull
candidates and still scores them through the same adjustment ratio,
badness, fitness, and demerits used by the strict pass.  That keeps
underfull final-pass choices inside the global K-P DP instead of forcing
them through a separate fixed-cost path.

Separately, the final pass implements TeX's `artificial_demerits` safeguard.
When an overfull candidate would otherwise remove the last active path to a
breakpoint and no non-overfull candidate survives there, the best provisional
path is installed with tight fitness and zero incremental demerits. This is a
reachability rule, not a hard-atom scoring shortcut: it never competes while a
normal active path survives, and it does not inspect CJK, units, or token
classes. Thus every input still produces a complete plan (regression: narrow
CJK used to return an empty result), while ordinary underfull lines compete
by normal K-P cost. The Elisp 1D, looseness/parshape, and C engines implement
the identical strategy.

## 5. Rendering

`ekp-line-glues` turns each line's `rest` into per-glue pixel values:

- rest > 0 → stretch, distributed latin → mixed → CJK; CJK gaps absorb
  any leftover beyond nominal capacity (emergency spreading).
- rest < 0 → shrink, same priority order, never below the per-class
  shrink limit; glue widths are clamped at ≥ 0.
- Last lines are ragged-right (ideal glues + trailing filler);
  single-box lines get a trailing filler clamped at ≥ 0.

`ekp-layout-plan` is the representation boundary between layout and
rendering.  It combines the paragraph boxes/source offsets, DP breaks,
per-line glue targets, indentation, stripped edges, and discretionary
hyphen decisions into `ekp-layout-plan`, `ekp-layout-line`, and
`ekp-layout-gap` records.  The plan contains no buffer positions or display
mechanism.  `ekp-render-layout-string` consumes it for the public string API;
the buffer integration can consume the same decisions without re-running or
reinterpreting the KP algorithm.

Cached semantic plans are immutable to the cache owner.  A cache hit returns
a consumer-owned copy of every plan-owned mutable payload: source string,
context tree, box vector and box strings, source offsets, line records,
line glues, gap records, and line signatures.  `ekp-layout-plan-para` is the
intentional exception: paragraph-cache ownership predates the semantic-plan
cache, and append planning depends on stable paragraph identity.
Layout context snapshots and returned plan contexts use the same recursive
copier for conses, vectors, and strings, so mutable policy inputs cannot alias
the cache key or a later consumer plan.

The two consumers deliberately have different representation rights.

#### String renderer

`ekp-render-layout-string` preserves the public string API.  It strips
leading/trailing space boxes, synthesizes display spaces and visual
newlines, and appends a propertized discretionary hyphen where selected.
The returned string remains lossless through four private markers:

| property          | on                     | value / meaning               |
|-------------------|------------------------|-------------------------------|
| `ekp-glue`        | synthesized glue space | original text it replaced     |
| `ekp-soft-break`  | synthesized `\n`       | swallowed boundary whitespace |
| `ekp-soft-hyphen` | synthesized hyphen     | marker only                   |
| `ekp-hidden`      | paragraph-edge text    | source retained, display empty|

This physical representation exists only in the returned string.  It is
kept for API compatibility and is not installed into a source buffer.
`ekp--layout-marker-properties` owns its marker vocabulary and
non-inheritance contract.

#### Buffer renderer

`ekp-buffer.el` keeps the buffer's character sequence untouched and
projects the same plan with text properties on existing source characters:

- source ASCII spaces:
  `((space-width FACTOR) (min-width ((TARGET-PIXELS))))`;
- zero-source CJK/mixed glue: `min-width` on the preceding complete
  grapheme, targeting its natural advance plus glue;
- indentation: `line-prefix`;
- a source-whitespace break: the first boundary character displays as
  newline and the rest as empty;
- a CJK or discretionary-hyphen break: a replacing display string
  reproduces the existing complete grapheme, appends the optional hyphen,
  then a visual newline.

`ekp-buffer--display` and `ekp-buffer--line-prefix` record exact ownership.
Removal clears the public property only when its value is still identical
to EKP's owner value, so a later foreign change is not erased.  Paragraphs
with foreign `display`, `line-prefix`, `wrap-prefix`, `composition`, or
`invisible` ownership stay verbatim.  Exact shrink of tabs/non-ASCII
whitespace is also refused because `space-width` affects ASCII spaces only.

All installation/removal runs inside `with-silent-modifications`, and owned
properties are nonsticky.  Buffer characters, point/mark, modified state,
undo, character-modified tick, and external change hooks therefore remain
source-owned. Reprojection restores the mark marker without calling
`set-mark`, then restores `mark-active` independently; an inactive mark
cannot become a region as a layout side effect. No EKP buffer path creates
an overlay.

Saving, ordinary search, syntax, and direct Elisp character APIs need no
logical-text adapter: the real buffer is already logical.  Copy filtering
remains necessary because `buffer-substring` intentionally preserves text
properties. EKP composes with the previous
`filter-buffer-substring-function`, then removes only its projection
metadata from the copied string.

#### Live flow

Live editing uses the ordinary whole-text Knuth-Plass plan without giving
editing state to the core planner:

1. Live state owns the last committed hard-line source, normal
   `ekp-layout-plan`, semantic signatures, projected spans, and stable break
   anchors at the narrowest-window authoritative width.
2. The first real change opens one edit transaction. It snapshots that
   committed state, then removes EKP properties only from the smallest
   projected span range containing the edit. Span objects and unaffected
   anchors remain registered.
3. Further changes inside the dirty island use native soft wrapping and do
   no whole-hard-line planning. If the logical source returns exactly to the
   snapshot, EKP restores the saved owned-property runs and marker offsets
   directly; the state, plan, signatures, and spans retain object identity.
4. Crossing a native visual-row boundary is a commit. EKP computes or reuses
   one whole-hard-line plan, derives every completed semantic row, and
   publishes their changed suffix silently as one command-loop transition.
   The new current row remains natural.
5. The other commits are a hard newline/paragraph completion, the next real
   edit outside the dirty island, explicit refill, and width/font/face/theme
   or layout-context change. Point motion is never a commit, even across hard
   paragraphs; there is no live `post-command-hook`.
6. Stable line signatures minimize property writes at a commit, while the
   16-entry buffer-local LRU reuses recent text/context plans. Neither
   mechanism decides when layout is allowed to change; the transaction owns
   that policy.
7. IME preedit, foreign display ownership, unsupported shrink, oversized
   hard lines, stale generations, or publication errors fail closed to
   native display. A partial projection is rolled back and the original
   error is surfaced.

Native wrapping is a state-machine precondition, not a user preference the
mode can merely hope is enabled. On activation, the mode snapshots the
values and local-binding ownership of `truncate-lines` and
`truncate-partial-width-windows`, then makes both buffer-local and nil.
Teardown restores local values or removes the temporary bindings so global
ownership resumes. This prevents Emacs's default 50-column partial-window
threshold from silently turning a narrow split into horizontal scrolling.

This state model needs no live lookahead, push/pull convergence, per-key
whole-line planner, or idle formatter. A real edit after point motion may
commit the previous active hard line, including when narrowing makes that
line inaccessible; point motion itself remains a strict no-op.

No edit-idle whole-paragraph formatter exists. Resize/background work is
generation-owned. Large buffers are processed visible-first in hard-
paragraph chunks; a single hard paragraph above
`ekp-auto-justify-paragraph-limit` remains natural during automatic work
and requires explicit `ekp-refill-paragraph` for an unbounded quality pass.
Because text properties are buffer-wide, the narrowest live window supplies
the one authoritative width.

### 5.1 Break permissions, alignment, protrusion, shapes

- **Break permissions**: every CJK char (punctuation included) is its
  own box; `ekp-para-breaks-allowed` forbids gaps per kinsoku (full-
  and halfwidth), `ekp-no-break' spans and NBSP-family joiners.
  Forbidden gaps carry no glue.  The DP skips them as candidates while
  the line keeps extending.  In the final pass, legal underfull candidates
  receive finite emergency stretch and are scored by normal
  badness/demerits. If an overfull candidate would extinguish the last active
  final-pass path, TeX-style artificial demerits preserve that path with zero
  incremental cost. C receives the sparse `forbidden-positions` vector.
  Explicit hard atoms forbid only breaks inside their interval; an otherwise
  legal boundary immediately before or after an atom remains legal. Atom
  adjacency therefore receives no special scoring or break prohibition.
- **Configurable policy compilation**: buffer and core policy variables are
  resolved before tokenization into private structural intervals.  Region
  `ekp-break-policy` wins first, explicit buffer/file/dir locals win over
  mode profiles, and profiles win over global defaults.  Token policies are
  the only merged category map; scalar and face-list options replace the
  lower scope.  The core may use private properties such as
  `ekp--face-break-policy`, `ekp--no-hyphen`, and
  `ekp--literal-spacing` on an analysis copy, but cache keys, plan strings,
  boxes, rendered strings, and `ekp--last-para` must contain only public
  source properties.  Automatic face no-break is measured over the
  contiguous private face-policy span and downgrades to no-hyphen when
  overwide; explicit `ekp-no-break` never downgrades.
- **Literal inline spaces**: face-derived `no-hyphen` preserves source
  spaces as literal boxes inside a line, forbids a break that would move a
  literal source-space box to line start, and permits the complementary
  break after a source-space box.  When such whitespace is the selected
  visual break, source ownership belongs to the break gap metadata, not to a
  trailing visible line box.  This rule is policy-derived and must not depend
  on whether the source also carries a public `face` property.
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
  resolved by `ekp--line-spec' (line-index → INDENT . WIDTH).  A
  plain first-line indent only changes line 0, and a line begins at
  box 0 exactly when the DP start i = 0, so the 1D pass (and the C
  engine, via `FIRST-LINE-WIDTH') handle it with no extra state.
  Only full `ekp-parshape' and `ekp-looseness' need the
  (position × line-count) DP and bypass C.  Indents render as leading
  `ekp-glue' spacers.

C module 1.6: `ekp-c-break-with-arrays' takes 15 args
(…, forbidden-positions, tail-protrudes, hyphen-protrude,
first-line-width); batch vectors have 15 elements;
`ekp-c-set-penalties' takes 4–8.  Policy compilation feeds the existing
hyphen-position and forbidden-break vectors; it must not add a sixteenth C
argument or batch field without a new architecture decision.

Performance after the feature wave (byte-compiled + C, Apple
Silicon, batch): justify zh w=200 ≈ 54 ms, range zh ≈ 117 ms —
justify at parity with the pre-feature numbers, range ≈ +55 % from
the larger box count.  Hot-path caches: per-char `ekp--str-type'
memo, interned glue strings, per-(paragraph, width) rendered-output
cache in the dp-cache (capped at 64 widths).  Continuous-reflow
reality check (60-paragraph, 26 k-char article, region layer
included): ≈ 73 ms per width change cold, less on revisit;
incremental single-paragraph re-justify after an edit ≈ 17 ms.

Adversarial source-mode builders are measured separately by
`ekp-bench-adversarial-builders`.  From 1,000 to 8,000 characters, the
fragment-based tokenizer grew 6.3× and dense insertion 7.7× (near the
expected linear 8× input growth).  At 8,000 characters they took 1.100 s and
0.013 s, versus 3.133 s and 0.945 s before the change.  The hyphen-position
cache uses an explicit miss sentinel so a legitimate nil result is reusable.

## 6. Looseness

`ekp-looseness` ≠ 0 switches to `ekp--dp-run-loose`, a full
(position × line-count) DP that keeps the best path *per line count*,
then picks the final count closest to (optimal + looseness), breaking
ties by demerits.  This is heavier than the 1D pass and is Elisp-only;
`ekp--c-available-p` returns nil while looseness is active so both
engines never disagree.

## 7. C Module Integration

The C module (`ekp_c/`, version 1.6) runs only stage ④.  Elisp remains
the source of truth for all font-dependent data.

- `ekp-c-break-with-arrays` (15 args): the para's prefix arrays, glue
  arrays, hyphen data, line width, the two space-run arrays, the
  forbidden/protrusion arrays and the first-line width.  Returns
  `(breaks . cost)`.
- `ekp-c-break-batch`: a vector of 15-element vectors, processed in
  parallel by a pthread pool — one task per paragraph (that is the
  correct granularity; the DP itself is sequential by nature).  The
  pool is created lazily on the first multi-paragraph batch and sized
  to the machine's cores; a full queue blocks the submitter rather
  than dropping the task.
- `ekp-c-set-penalties` (4–8 args): called by `ekp--c-sync-params`
  before *every* C entry, so `ekp-line-penalty` & friends always take
  effect (regression: they were never synced before).
- `ekp-c-module-load` refuses modules older than
  `ekp-c-module-required-version` and falls back to Elisp, preventing
  arity mismatches after upgrades.

An unavailable module, an incompatible module version, a nil whole C result,
or a nil per-item/break result falls back to the Elisp engine.  Invalid
direct API input signals `ekp-c-invalid-input`.  Any non-nil malformed
backend result signals `ekp-backend-contract-error`: malformed cons shape,
non-list breaks, non-integer/out-of-range/non-increasing/partial breaks,
nonnumeric cost, or malformed batch result shape.  Any signal from an
enabled backend propagates through the public formatter; the dispatcher does
not catch and hide it.  The module never silently produces a different
layout on partial failure.  The two engines are verified byte-identical by
`ekp-test-c-parity-simple` / `ekp-test-c-parity-files` and the 300-case
property fuzz.

### Future direction: a paragraph-handle API

Each `ekp-c-break-with-arrays` call re-marshals the paragraph's
width-independent arrays (≈18·n `env` extractions).  This is invisible
for a single justify but dominates `ekp-pixel-range-justify`, which
re-marshals the same arrays once per candidate width: with a warm
paragraph cache the C path still costs ≈12 ms per width, most of it
marshal, not DP (the DP is ≈2.5 ms for the whole sample).

The fix is a `make_user_ptr` handle: `ekp-c-para-upload` copies the
arrays into a C struct once and returns a handle with a GC finalizer;
`ekp-c-break (handle, width)` then passes only the two width-dependent
scalars.  It is deliberately **not** part of this release — it is a
breaking (2.0) ABI change introducing C-side object lifetime, and the
common interactive paths (single justify, `ekp-auto-justify-mode`)
already avoid the repeated marshal because they hit the dp-cache.  It
is the clear next step whenever range search or very large batches
become a bottleneck.

## 8. Hyphenation (ekp-hyphen.el)

Liang's ordinary pattern algorithm:

- `dictionaries/hyph_*.dic` are compiled to a pattern hash on first
  use and cached per path.  Files may be UTF-8 or ISO-8859 (Emacs
  auto-detects; verified by `ekp-test-hyphen-de-iso8859-dict`).
- `ekp-hyphen-create LANG` resolves exact codes, then progressively
  shorter prefixes (`"de_CH" → "de"`).
- Margins default to 2 characters on each side of a break.
- A slash in a non-comment pattern is fail-closed.  Libhyphen replacement
  rules change the visible text and width only when their break wins; EKP's
  current fixed-width boxes cannot represent that honestly.  The compiler
  counts the rules and signals `ekp-hyphen-unsupported-pattern`, and the
  public formatter propagates it.
- `dictionaries/MANIFEST.tsv` pins 49 inventory entries to one LibreOffice
  commit (plus one explicitly identified legacy Basque byte), their SHA-256,
  syntax flag, and license evidence.  `tests/check-dictionaries.sh` is the
  offline gate; `dictionaries/update.sh check` verifies normalized bytes
  against the upstream commit on macOS and Linux.

Word boxes are matched against
`^[left-punct]* (latin-word) [right-punct]*$` so that punctuation-
wrapped words (`(word)`, `word!`, `»word«`) still hyphenate; the
punctuation stays glued to the first/last syllable box.

## 9. Testing & Benchmarks

```bash
tests/run-tests.sh [emacs]        # batch-safe ERT suite
tests/run-tests.sh [emacs] --random-order
tests/run-tests-isolated.sh [emacs] # each ERT in a fresh process
tests/check-dictionaries.sh           # offline inventory/checksum gate
dictionaries/update.sh check          # verify pinned upstream bytes
make -C ekp_c PROFILE=portable      # release-portable default
make -C ekp_c PROFILE=native        # local benchmark only
make -C ekp_c PROFILE=debug         # symbols, no optimization
make -C ekp_c PROFILE=sanitize      # ASan + UBSan
emacs -Q --batch -L . --eval '(setq ekp-use-c-module nil)' -l tests/ekp-bench.el
emacs -Q --batch -L . --eval '(progn (require (quote ekp)) (ekp-c-module-load))' \
      -l tests/ekp-bench.el
```

Set `EKP_TEST_SEED` to reproduce or vary the permuted-order run. Test
fixtures dynamically restore all EKP configuration they isolate; tests that
exercise dispatch must use the public formatter rather than only an internal
eligibility predicate.

The GUI matrix is loaded explicitly from `tests/ekp-gui-verify.el`. It
returns status 1 after printing the table when any row fails; the ERT suite
contains a forced-failure control for this boundary.

`M-x ekp-c-module-build` uses the same four profile names and invokes make
as an argv process in `ekp_c/`; it never constructs a shell `cd` command.
Release/CI artifacts use `portable`. Use `native` only for measurements on
the machine that will run the module.

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
ekp-buffer.el     Text-property-only buffer/region projection, synchronous
                  live flow, window lifecycle, copy filtering, diagnostics
ekp_c/            C dynamic module (see ekp_c/README.md)
dictionaries/     Hunspell hyphenation patterns (from LibreOffice)
tests/            ekp-tests.el, ekp-buffer-tests.el (ERT),
                  ekp-fuzz.el (parity fuzz), ekp-bench.el,
                  ekp-demo.el, ekp-showcase.el, sample texts,
                  run-tests.sh
```
