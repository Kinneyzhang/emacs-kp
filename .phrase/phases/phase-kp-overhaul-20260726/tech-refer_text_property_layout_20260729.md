# Technical Reference: Text-Property-Only KP Layout 2026-07-29

## Status and Reader Task

Implemented by `task019` through `task022` for static/source-clean
projection, then refined by later live-editing tasks. `task028` supersedes
the native-row live planning unit with semantic hard-line prefix
projection. This record is the technical reference for the semantic layout
plan, text-property-only buffer projection, live editing boundary, and
their explicit constraints.

## User Constraints

- Do not use overlays.
- Do not insert glue spaces, soft newlines, or discretionary hyphens into
  buffer text.
- Express layout through text properties on existing source characters.
- Keep the source character sequence and all logical buffer positions
  unchanged.
- An unfinished active line and later live-edit behavior remain separate
  product concerns; a static display proof does not establish seamless
  editing.

## Conclusion

Emacs 29.1 and later expose enough display primitives to make a
text-property-only KP renderer plausible:

- `(min-width ((PIXELS)))` gives a source span an absolute minimum display
  width and pads only in the display layer.
- `(space-width FACTOR)` scales existing ASCII space glyphs without
  replacing their source characters.
- Combining `space-width` with `min-width` expresses both shrink and stretch
  while using `min-width` as an exact pixel floor after floating-point
  rounding.
- `line-prefix` supplies indentation for non-continuation display lines.
- A replacing `display` string attached to an existing grapheme can show
  that grapheme followed by a discretionary hyphen and a display-only
  newline. The original grapheme remains in the buffer.

These primitives close the representation gap for the implemented
single-authoritative-width buffer renderer. Foreign ownership, multiple
windows, and seamless incremental editing are governed by the decisions
below rather than inferred from the primitives alone.

The package baseline does not need to move: Emacs `NEWS.29` records
`min-width` as a new `display` property, and the repository's CI exercises
the declared Emacs 29.1 floor.

## Confirmed Emacs Semantics

### Absolute pixel minimum width

The `min-width` display specification has the form
`(min-width (WIDTH))`. The outer one-element list identifies a contiguous
display run by `eq`. A pixel expression of the form `(N)` means exactly
`N` pixels, so the fully nested form for 20 pixels is:

```elisp
(min-width ((20)))
```

Emacs displays the underlying text normally and appends display-only white
space if the rendered span is narrower than the requested minimum.

Each independently padded adjacent span must receive a distinct identity
list. Reusing the same inner list object can make Emacs treat adjacent
spans as one run and add padding only at the end of the combined run.

References:

- [Other Display Specifications](https://www.gnu.org/software/emacs/manual/html_node/elisp/Other-Display-Specs.html)
- [Pixel Specification for Spaces](https://www.gnu.org/software/emacs/manual/html_node/elisp/Pixel-Specification.html)
- [Emacs 30.2 display implementation](https://github.com/emacs-mirror/emacs/blob/emacs-30.2/src/xdisp.c#L5606-L5686)

### Scaling real spaces

`(space-width FACTOR)` multiplies the rendered width of every ASCII space
covered by the property. A factor below 1 shrinks; a factor above 1
stretches.

The Emacs 30.2 implementation adds three boundaries beyond the manual's
summary:

- `FACTOR` must be a number greater than zero.
- The graphical display path checks specifically for ASCII `SPC`.
- The specification is ignored on a non-window-system frame.

It does not affect tabs. NBSP, ideographic spaces, tabs, and other Unicode
space characters therefore need their own policy; they must not be assumed
to behave like an ASCII word space.

References:

- [Other Display Specifications](https://www.gnu.org/software/emacs/manual/html_node/elisp/Other-Display-Specs.html)
- [Emacs 30.2 `space-width` parsing](https://github.com/emacs-mirror/emacs/blob/emacs-30.2/src/xdisp.c#L6039-L6052)
- [Emacs 30.2 ASCII-space scaling](https://github.com/emacs-mirror/emacs/blob/emacs-30.2/src/xdisp.c#L32723-L32729)

### Combining the two specifications

For a real ASCII whitespace run whose measured natural width is `N` and
whose KP target width is `G > 0`, use the conceptual display value:

```elisp
((space-width FACTOR)
 (min-width ((G))))
```

where `FACTOR = G / N`.

`space-width` first makes shrink possible. The display engine rounds the
scaled glyph width to pixels; `min-width` then pads any downward rounding
error to the exact target. For a target at least as wide as the natural
run, `min-width` alone is sufficient, but one combined representation can
keep the renderer uniform.

A zero-width source-space run is not represented by `(space-width 0)`,
because zero is rejected. Line-edge spaces must instead be consumed by the
chosen break representation, retained naturally, or handled by a separately
verified display rule.

### Indentation

The `line-prefix` text property adds a display-only prefix to every
non-continuation line. It must cover the complete affected text range so
redisplay can find it at each line start.

If EKP relies on Emacs's native wrapping, continuation lines use
`wrap-prefix`, not `line-prefix`. If EKP realizes each KP break as a newline
inside a replacing display string, the following row is a non-continuation
display line and `line-prefix` is applicable.

Reference:

- [Special Properties: `line-prefix`](https://www.gnu.org/software/emacs/manual/html_node/elisp/Special-Properties.html)

## Mapping KP Output to Existing Source Text

| KP result | Text-property-only projection |
| --- | --- |
| Ordinary box | Display the original source grapheme or word unchanged. |
| Latin word glue over ASCII spaces | Apply `space-width`; add `min-width` as the exact pixel floor. |
| CJK or mixed glue with no source space | Apply `min-width` to the preceding source box with target `natural-box-width + glue-width`. |
| Leading indentation | Apply `line-prefix`; use `wrap-prefix` only if the selected break backend creates continuation lines. |
| Break at an existing single space | Replace that space on display with a newline while leaving the source space in the buffer. |
| CJK break with no boundary character | Make the preceding grapheme display as `GRAPHEME + NEWLINE`. |
| Latin discretionary hyphen break | Make the preceding grapheme display as `GRAPHEME + HYPHEN + NEWLINE`. |
| Final line | Remove forced-break/hyphen properties and leave it ragged unless alignment requires a leading prefix. |

The current algorithm already computes exact per-gap pixel results for
stretch and shrink. The planned change is a renderer substitution, not a
new KP cost or distribution model.

## Discretionary Latin Hyphen Without Source Pollution

### Proposed break owner

For a KP break between source positions `P` and `P+1` inside a Latin word:

1. Identify the complete grapheme immediately before the break. Never split
   a combining sequence.
2. Leave every source character untouched.
3. Attach a replacing `display` text property to that grapheme.
4. Build the replacement string from the same visible grapheme, followed by
   the configured hyphen glyph and `"\n"`.
5. Copy the grapheme's relevant face/font properties to the replacement
   string, but remove recursive `display` state.
6. Put a `cursor` property on the reproduced grapheme so point on the
   covered source positions is drawn on the grapheme rather than on the
   synthetic hyphen or newline.
7. Include the hyphen width in the KP line measurement exactly as the
   current renderer already does.
8. Remove or move the property atomically when a later plan chooses a
   different break.

Conceptually, breaking `abcdefgh` after `d` displays:

```text
abcd-
efgh
```

while the source characters remain exactly:

```text
abcdefgh
```

The hyphen and newline exist only inside the `display` property's
replacement string. `char-after`, character positions, regexp search, and
`buffer-substring-no-properties` continue to operate on the original word.

### Why the preceding grapheme owns the replacement

Attaching `"-\nE"` to the next grapheme would make point on that source
grapheme default to the hyphen or newline portion. Attaching
`"D-\n"` to the preceding grapheme keeps the visible source glyph at the
property's logical anchor, and the next source grapheme begins naturally on
the following display line.

### Runtime evidence on Emacs 30.2

A clean GUI Emacs 30.2 probe applied a `display` text property containing
`"d-\n"` with `cursor 1` to the source `d` in `abcdefgh`.

Observed result:

- `substring-no-properties (buffer-string)` remained `"abcdefgh"`.
- `buffer-size` remained 8.
- `count-screen-lines` reported 2.
- `vertical-motion` by one display line moved point from position 1 to
  position 5, the source `e`.
- A 20px `line-prefix` placed both visual rows at x=20.

This proves the minimal LTR shape is expressible. It does not yet prove
mouse selection, active-region highlighting, isearch faces, bidi text,
multi-codepoint graphemes, IME composition, or foreign display-property
composition.

References:

- [Display strings](https://www.gnu.org/software/emacs/manual/html_node/elisp/Other-Display-Specs.html)
- [Cursor placement in display strings](https://www.gnu.org/software/emacs/manual/html_node/elisp/Special-Properties.html)
- [Emacs 30.2 cursor handling around display-string newlines](https://github.com/emacs-mirror/emacs/blob/emacs-30.2/src/xdisp.c#L1899-L1956)

## Runtime Evidence for Exact Glue

A clean GUI Emacs 30.2 probe measured the current font's ASCII space at
7px, then applied the combined `space-width` and `min-width` display value
to two real source spaces:

- target 3px rendered as exactly 3px;
- target 20px rendered as exactly 20px;
- the buffer still returned the unchanged source characters `"a b"`.

This proves one-font, one-window shrink and stretch. The production gate
must repeat the probe across text scaling, face remapping, font fallback,
mixed property runs, and window resizing.

## Text API Boundary

This direction removes character pollution but not all property metadata.

- `buffer-substring-no-properties`, direct character access, search,
  syntax parsing, markers, point, and character counts see the original
  source characters.
- `buffer-string` and `buffer-substring` copy text properties into the
  returned string. Lisp that explicitly examines properties can observe
  EKP's display metadata.
- Property-only updates must use `with-silent-modifications` so layout
  refresh does not alter the modified flag, ordinary undo history, or
  external change hooks.
- EKP-owned properties must be nonsticky so new user input does not inherit
  a stale break, hyphen, width, or prefix.
- Copy/yank and insertion of propertized strings require an ownership rule
  that strips or regenerates EKP layout properties rather than carrying a
  stale layout into another location.

References:

- [Text Properties](https://www.gnu.org/software/emacs/manual/html_node/elisp/Text-Properties.html)
- [Examining Buffer Contents](https://www.gnu.org/software/emacs/manual/html_node/elisp/Buffer-Contents.html)
- [Changing Text Properties](https://www.gnu.org/software/emacs/manual/html_node/elisp/Changing-Properties.html)

## Implementation Decisions and Remaining Boundaries

### Chosen break semantics

The replacement-string approach forces the exact KP break and makes
`line-prefix` usable. A native-wrap alternative might avoid replacing the
break-owner grapheme by filling the line exactly with `min-width`, but it
still needs proof that Emacs never wraps at an earlier legal boundary and
that exact-edge stretch glyphs remain on the intended line.

The implementation uses the replacing-string backend because it represents
the chosen KP break explicitly. Native wrapping is not a fallback.

### Foreign `display` ownership

Text has one effective `display` property value at each position. Parallel
non-replacing specifications can coexist in one value, but a foreign
replacing string, image, invisibility rule, composition, or font-lock
manager can conflict with EKP's width or break owner.

EKP rejects the affected hard paragraph with an explicit diagnostic. It
never erases, merges, or silently reorders a foreign replacing
specification.

### Graphemes, bidi, and shaping

A replacing display string is treated as one display object for
bidirectional reordering. The first implementation slice should therefore
target LTR Latin hyphenation and CJK. Arabic, Hebrew, Indic shaping,
ligatures, and multi-codepoint graphemes remain gated until their cursor,
selection, and shaping behavior is proven.

### Multiple windows

Text properties belong to the buffer, not a window. One property set cannot
simultaneously encode two different KP break plans for the same buffer shown
at different widths.

The narrowest live window showing the buffer is authoritative. Wider
windows can retain right-side space but cannot overflow. Independent
simultaneous width plans are explicitly outside this text-property-only
architecture.

### Live editing

The current target policy separates stable editing from global KP
commitment. One committed projection owns the hard-line baseline source,
semantic plan, line signatures, projected spans, and break anchors. One
dirty edit transaction snapshots that baseline and naturalizes only the
affected local island.

Ordinary input inside the same native visual row performs no whole-hard-line
planning. Existing glue and native soft wrapping absorb local changes while
unaffected anchors remain installed. If the row overflows, native wrapping
moves only the required local words. Crossing into a new native visual row
is a structural commit: `ekp-buffer` calls or reuses the existing
whole-hard-line `ekp-layout-plan` once, then old/new semantic signatures
limit one silent prefix publication.

Hard newline/paragraph completion, the next real edit outside the dirty
island, explicit refill, and width/font/layout-context changes are the other
commit events. Point-only motion is not a layout event anywhere. A reversible
edit restores the saved owned projection directly and
`equal-including-properties`.

This remains a buffer-layer policy, not a DP variant. Core DP plans complete
text and receives no point, marker, window, buffer, redisplay, transaction,
or composition state. The C ABI, DP schema, and plan contract do not change.

Static paragraph-edge hiding applies only to committed projected lines. The
dirty edit island retains leading/trailing source whitespace with no
replacing display, so a single space or tab is visible in the same
command-loop turn. Reprojection restores the mark marker and `mark-active`
as separate editor state; `set-mark` is not a valid positional restoration
API because it activates an otherwise inactive mark.

Automatic planning is bounded by
`ekp-auto-justify-paragraph-limit`. An oversized single hard line stays
natural and diagnostic rather than blocking input; the explicit
`ekp-refill-paragraph` command bypasses this limit.

## Rejected Shortcuts

- Insert U+00AD, `-`, whitespace, or newline characters: violates the
  logical-text requirement.
- Overlay `before-string` or `after-string`: explicitly excluded by the
  user.
- Use `min-width` alone for shrink: it only adds padding.
- Use `space-width` alone as an exact-width guarantee: floating-point
  scaling is rounded to pixels.
- Treat tabs or all Unicode whitespace as ASCII spaces: contradicted by the
  documented and source-level behavior.
- Claim the GUI probe proves seamless editing: it covers a static LTR
  display case only.

## Runtime Verification Gate

1. Exact glue:
   - 1px through 64px targets;
   - shrink and stretch;
   - CJK, ASCII space, mixed gap, and repeated spaces;
   - text scaling, face remap, and fallback fonts.
2. Breaks and hyphens:
   - Latin word break with hyphen;
   - CJK break without whitespace;
   - break at one and multiple source spaces;
   - final-line removal and breakpoint movement.
3. Editor semantics:
   - point at every source position;
   - `C-n`/`C-p`, visual beginning/end of line, mouse hit testing;
   - active region, isearch, kill/yank, undo/redo, and narrowing.
4. Ownership:
   - font-lock refontification;
   - a foreign replacing `display` property;
   - composition and IME preedit;
   - exact restoration of pre-existing properties.
5. State invariants:
   - unchanged source characters and `buffer-chars-modified-tick`;
   - unchanged modified flag, undo list, and external change-hook count;
   - no stale property inheritance after insert, delete, split, or join.
6. Architecture decision:
   - explicitly resolve or relax simultaneous different-width windows
     before calling the renderer complete.

## Stop Condition

This research task is complete when the confirmed primitives, proposed
hyphen owner, known limitations, and runtime evidence are recorded.
The original static feasibility and one-buffer/multiple-window gates were
satisfied by `task019`–`task022`.

`task028` proved whole-hard-line plan consumption and semantic signature
publication. `task029` proved that point-only motion must perform zero work.
`issue019`/`task031` supersede their per-edit trigger ownership: a frontier
cannot simultaneously be latest edit, natural suffix, and projection reuse
boundary. Whole-hard-line planning remains valid at structural commits, not
after every key.

The task029 performance audit recorded 291 unique append plans and zero cache
hits at 80 pixels. `task031` now removes ordinary same-row planning: the same
291-edit workload records 15 structural plans, while same-row revisit and
point-motion scenarios record zero plans. The remaining C/Elisp p99 spikes
coincide with permitted commit events; `issue018`/`task030` now own that
unblocked, narrower performance surface.
