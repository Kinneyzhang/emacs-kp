# Spec: Source-Clean Text-Property KP Layout 2026-07-29

## Summary

`ekp-buffer.el` presents Knuth-Plass layout without rewriting the buffer's
character stream. The core computes a reusable semantic layout plan;
manual buffer commands and `ekp-auto-justify-mode` project that plan using
text properties on existing source characters only.

The character sequence remains the document. Visual glue, breaks,
indentation, and discretionary hyphens are display metadata: EKP creates no
overlay and inserts no layout character.

## Goals

1. Keep `buffer-string`, `buffer-substring-no-properties`, direct character
   access, search, syntax, markers, saving, and ordinary editing on the
   original character sequence.
2. Use the same core glue, break, indentation, and hyphen decisions for the
   compatible string renderer and the non-mutating buffer renderer.
3. Make automatic layout stable while editing: committed prefix lines keep
   their projection until a structural commit, the current unfinished visual
   row stays under Emacs redisplay ownership, and a middle-line edit
   naturalizes only its local dirty island while preserving unaffected break
   anchors.
4. Preserve undo, modified state, character-modified tick, point, mark,
   copy/kill, isearch, IME composition, narrowing, and mode lifecycle.
5. Fail closed around display ownership: unsupported or conflicting hard
   paragraphs remain naturally displayed and are diagnosable.

## Non-Goals

- Hiding EKP metadata from Lisp that explicitly inspects text properties.
  The clean-text guarantee concerns the character sequence; copy/kill strips
  EKP-owned projection metadata.
- Independent simultaneous layouts for different-width windows showing the
  same buffer. The narrowest live window is authoritative.
- Shrinking tabs or non-ASCII whitespace with `space-width`.
- Claiming verified bidi, Arabic, Hebrew, Indic shaping, ligatures, or
  arbitrary multi-codepoint grapheme behavior beyond the tested LTR
  Latin/CJK boundary.
- Retaining `(require 'ekp-region)` or old private/configuration names.

## User Flows

### Load and use buffer commands

1. The user adds the package to `load-path`.
2. The user loads `(require 'ekp-buffer)`.
3. `M-x ekp-justify-region` projects the active region, or the hard
   paragraph at point when no region is active.
4. `M-x ekp-justify-buffer` projects the accessible buffer.
5. `M-x ekp-unjustify-region` or `ekp-unjustify-buffer` removes only
   EKP-owned projection properties; source characters do not need
   restoration.

### Edit with automatic layout

1. The user enables `M-x ekp-auto-justify-mode`.
2. A committed projection records the complete source baseline, one
   whole-hard-line `ekp-layout-plan`, line signatures, projected spans, and
   break anchors. Core DP receives no point, marker, window, or edit-state
   input.
3. Ordinary input within the current native visual row performs no
   whole-hard-line planning. Previously committed rows remain
   property-identical and the current row stays natural.
4. Editing a committed middle row starts a dirty transaction. Only the
   affected local island becomes natural; following and preceding unaffected
   break anchors remain installed. Native wrapping absorbs local push/pull.
5. Crossing into the next native visual row atomically replans the completed
   hard-line prefix once. Line-signature diffing limits property writes; the
   new current row remains natural.
6. A hard newline/paragraph end, the next real edit outside the dirty island,
   explicit refill, or width/font/layout-context change commits the
   transaction. Point-only motion anywhere performs zero plan, cache,
   property, or layout work.
7. Restoring the baseline source restores the saved projection immediately
   and exactly, including text properties, plan signatures, and spans.
8. Completed paragraphs use the existing full Knuth-Plass projection.
9. Disabling the mode cancels pending work, detaches markers, restores any
   prior copy filter, and removes every EKP-owned projection.

### Use logical text during projection

1. Search and isearch operate directly on source characters, including
   across display-only breaks and discretionary hyphens.
2. Save and `write-region` serialize source characters; visual layout never
   reaches disk.
3. Copy/kill composes with an existing
   `filter-buffer-substring-function`, removes only EKP-owned projection
   metadata from the copied string, and preserves delete semantics.
4. Lisp that needs a property-free string uses
   `buffer-substring-no-properties`; property-aware APIs may inspect the
   EKP display metadata without seeing synthesized layout characters.

### Diagnose an unsupported paragraph

1. EKP detects foreign `display`, `line-prefix`, `wrap-prefix`,
   `composition`, or `invisible` ownership, or an exact shrink request for
   unsupported whitespace.
2. EKP leaves only that hard paragraph natural instead of stealing or
   approximating the foreign representation.
3. `M-x ekp-diagnose` reports the authoritative width and recorded
   conflicts. The user may remove the conflict or explicitly refill after
   changing the content/configuration.

## Display Contract

- ASCII source spaces use:

  ```elisp
  ((space-width FACTOR)
   (min-width ((TARGET-PIXELS))))
  ```

- A zero-source CJK/mixed gap applies `min-width` to the preceding complete
  grapheme, with a target equal to its natural advance plus the planned
  glue.
- Indentation uses `line-prefix`.
- A source-whitespace break displays the first boundary character as a
  newline and any remaining boundary whitespace as empty.
- A CJK or Latin discretionary break uses a replacing display string on an
  existing complete grapheme. It reproduces that grapheme, appends the
  optional hyphen, then a visual newline.
- Every public projection value has an EKP owner property. Cleanup removes
  a public value only while it is still identical to the owned value.
- All projection updates use `with-silent-modifications`; owned properties
  are nonsticky.

## Edge Cases

- Active IME composition defers projection and retries only for the current
  generation.
- Stale resize/background generations cannot publish.
- A hard paragraph over `ekp-auto-justify-paragraph-limit` remains natural
  during automatic work; explicit `ekp-refill-paragraph` is unbounded.
- Large buffers reflow visible-first in time-bounded hard-paragraph chunks.
- Foreign property conflicts are isolated to their hard paragraph.
- Narrowing does not cause projection cleanup or installation outside the
  intended accessible operation; mode teardown widens to remove all owned
  state.
- Major-mode changes and mode disable leave the logical text and prior
  integrations intact.
- A rigid inline atom that jumps a candidate line from underfull to overfull
  stays intact without forcing the preceding prose through one-box emergency
  lines. In the final pass, ordinary underfull candidates receive finite
  emergency stretch and remain normal K-P cost candidates. An atom wider than
  the measure stays intact but may share an overflow line with preceding
  ordinary content when TeX-style artificial demerits must preserve the last
  active path. Atom adjacency has no special break or scoring rule.
- Inline code is not rigid by default. Known Org/Markdown inline faces use
  the inline policy path, not the paragraph verbatim path. The default policy
  permits legal wrapping at existing boundaries, suppresses discretionary
  dictionary hyphenation, and preserves source whitespace literally. Known
  code-block faces remain paragraph-level verbatim.
- Manual `ekp-no-break` is the only explicit hard-atom owner and is never
  downgraded. Automatic no-break spans, such as compact number-unit tokens,
  downgrade to no-hyphen when wider than the effective measure.
- The previous narrow showcase orphan-glyph failure is now a permanent
  acceptance invariant: the inline-code paragraph must not isolate CJK glyphs
  such as `行`, `内`, or `永` as one-character source lines around automatic
  inline code when a non-emergency legal alternative exists. Dynamic evidence
  must distinguish automatic inline code from explicit hard atoms and block
  verbatim spans.
- The GUI oracle rejects any isolated CJK source line in the showcase
  paragraph at the checked widths. This is a core K-P invariant, not a
  unit-suffix, screenshot-specific, or renderer compensation rule.
- Effective policy precedence is deterministic: region properties, then
  explicit buffer/file/dir-local values, then major-mode profiles, then
  global defaults. Manual text properties are session-local; persistence
  comes from mode syntax/profiles and file/dir locals.
- URL, path, and identifier tokens default to no-hyphen. Compact number-unit
  tokens default to no-break. Hyphenation defaults to `auto`, with `on` and
  `off` overrides. Kinsoku defaults to `common`, with `zh`, `ja`, `off`, and
  custom additions available. Ordinary overlong tokens default to the current
  emergency output, with `overflow` and `natural` alternatives.
- The buffer measure defaults to the narrowest live window. A positive
  integer fixed measure and `(max . PIXELS)` cap are configurable and must be
  reported by diagnostics when they create overflow risk.
- Reprojection preserves point, the mark marker, and `mark-active`
  independently. An inactive historical mark must never become a visible
  selection merely because width or layout options changed.
- Semantic live-prefix editing still requires real Emacs soft wrapping for
  the active natural suffix.
  `ekp-auto-justify-mode` temporarily disables both `truncate-lines` and
  narrow partial-window truncation, then restores their prior values and
  local/global binding state when the mode ends.

## Acceptance Criteria

1. `ekp.el` exposes a semantic layout plan containing source offsets,
   per-line glue targets, break kinds, indentation, and discretionary
   hyphen decisions; the public string API remains
   `equal-including-properties` compatible.
2. No EKP buffer path creates an overlay or inserts glue spaces, soft
   newlines, or discretionary hyphens into source text.
3. ASCII gaps use the exact combined `space-width`/absolute-pixel
   `min-width` form; zero-source gaps, indentation, breaks, and hyphens use
   the display contract above.
4. Source characters, positions, point/mark, modified state, undo history,
   character-modified tick, save output, search, and syntax remain logical.
5. Automatic editing proves committed-projection and dirty-transaction
   ownership: ordinary same-row edits perform no whole-hard-line planning or
   unaffected property writes; middle-row edits preserve unrelated anchors;
   reversible edits restore the complete projection
   `equal-including-properties`; soft-wrap crossing publishes one atomic
   whole-hard-line prefix update; structural commit events use the existing
   `ekp-layout-plan`; and point-only motion performs zero layout work. IME
   deferral, stale resize rejection, and exactly one complete KP pass after
   hard-paragraph completion remain required. Narrow side-by-side windows
   soft-wrap rather than horizontally scroll. A single leading/trailing
   space or tab is visible in the same command-loop turn, including
   whitespace exposed by deletion.
6. Copy/kill, isearch, foreign ownership, multiwindow narrowest-width
   policy, narrowing, resize, major-mode change, and teardown have
   deterministic tests.
7. Exact GUI probes cover 1–64px ASCII and zero-source CJK glue,
   display-only break/hyphen/indentation, point, vertical motion, region,
   mouse, scaling/remapping/fringes, unchanged source, and zero overlays.
8. Dynamic per-keystroke evidence proves mixed Latin/CJK same-row typing
   preserves the committed projection, a real visual-row crossing atomically
   publishes all completed rows from one hard-line plan, exact source
   reversal restores the baseline projection, point-only motion preserves
   every state object and property exactly, and paragraph completion performs
   the full completed-paragraph KP transition.
9. Default, permuted, and isolated ERT; C/Elisp fuzz; warning-as-error
   Elisp/C builds; checkdoc; package/static/release gates; full diff review;
   independent code review; and independent architecture review pass.
10. Elisp, C, semantic-plan, string-renderer, and real GUI paths keep an
    `ekp-no-break` atom intact while proving that its preceding CJK prefix is
    not fragmented into one-glyph lines at narrow measures.
11. Inline and token break policies are configurable at global, mode profile,
    explicit local, and region scopes. The accepted defaults are: inline code
    `no-hyphen`; block code verbatim; URL/path/identifier `no-hyphen`;
    compact number-unit `no-break`; hyphenation `auto`; kinsoku `common`;
    overlong token `emergency`; buffer measure `narrowest-window`.
12. Region `ekp-break-policy` supports `normal`, `hyphenate`, and
    `no-hyphen`, and never creates a second hard-atom representation.
    Overlapping explicit `ekp-no-break` wins over every new policy.
13. The showcase paragraph containing inline code, CJK prose, NBSP-backed
    numbers, and units must have no pathological single-CJK source line in
    the inspected 280px GUI path while retaining exact source text, zero
    overlays, block-code verbatim display, internal source-space inline
    breaks, and C/Elisp plan parity.
