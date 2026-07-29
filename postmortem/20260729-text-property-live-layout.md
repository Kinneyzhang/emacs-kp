# Keep KP Decisions Semantic and Buffer Layout Presentational

> Historical note: the text-property projection decision remains current.
> The partial-KP live state machine described below was replaced by the
> native progressive model in
> `20260729-native-progressive-live-editing.md`.

## Context

The original editor integration inserted a reversible rendered string into
the real buffer. Its marker properties made the transform recoverable, and
save/copy/search adapters hid much of the representation, but arbitrary
Elisp character APIs still observed synthesized spaces, newlines, and
discretionary hyphens.

The required outcome was stronger than reversible formatting: the buffer's
character stream itself had to remain the logical document. The user also
excluded overlays and required existing-character text properties,
specifically absolute-pixel `min-width`, real-space `space-width`, and
`line-prefix`.

This exposed a second mistake in the old live mode. A function that produces
an optimal completed paragraph is not automatically an editing algorithm.
Running it after an idle delay caused unfinished lines to snap, performed
whole-paragraph work for local edits, and had no explicit overflow,
pullback, composition, or convergence policy.

The first deletion implementation also recalculated its look-behind from
each backspace. Once two visual lines merged, the next deletion began at a
new line boundary and moved the anchor left again. The source round trip was
correct, but the projection accumulated an extra break because the
recomputation boundary crept into the formerly stable prefix.

## Decision

Separate layout decisions from representation.

`ekp-layout-plan` owns boxes/source offsets, line ranges, exact glue targets,
indentation, chosen breaks, and discretionary-hyphen decisions. The public
string renderer retains its compatible lossless physical returned string.
The buffer renderer consumes the same plan but has no right to insert a
character.

The buffer projection is:

- source ASCII space:
  `((space-width FACTOR) (min-width ((TARGET-PIXELS))))`;
- zero-source CJK/mixed gap: `min-width` on the preceding complete
  grapheme, targeting natural advance plus glue;
- indentation: `line-prefix`;
- explicit break/hyphen: a replacing display string on an existing
  complete grapheme that reproduces the grapheme, appends the optional
  hyphen, then a visual newline.

Every public property value is mirrored by an EKP owner property. Cleanup
removes the public value only while it is still identical to the owned
value. Updates use `with-silent-modifications`; owned properties are
nonsticky. No buffer path creates an overlay.

Live editing uses a separate bounded state machine:

1. keep one stable source-marker anchor for a continuous editing flow, then
   snapshot and clear only its affected projected suffix;
2. keep the point-containing unfinished line naturally spaced;
3. commit the longest fitting prefix on overflow;
4. pull following material back after deletion;
5. stop when source position and a position-independent line signature
   match the old plan;
6. defer composition and reject stale generation work;
7. allow a complete quality pass only after leaving the paragraph or an
   explicit refill command.

The active anchor is released when the flow ends: leaving the paragraph,
explicit refill, a hard-boundary reflow, conflict abandonment, or mode
teardown. A new deletion starts one line earlier only when it first begins
exactly at a visual-line boundary; subsequent backspaces reuse the same
anchor. This makes overflow followed by deletion projection-identical.

Automatic work has a hard single-paragraph limit. An oversized paragraph
stays naturally editable and diagnostic rather than entering an unbounded
DP during mode enable, paste, or typing. `ekp-refill-paragraph` is the
explicit request to pay that cost.

Text properties are buffer-wide, so the narrowest live window supplies one
authoritative width. Wider windows may show unused space. Simultaneous
different-width KP plans are not claimed.

## Alternatives Rejected

### Keep physical buffer text and add more adapters

No finite adapter list can change what arbitrary `buffer-string`,
`char-after`, syntax, or third-party Elisp reads. This compensates at the
wrong layer.

### Use overlays

The user explicitly excluded them. Keeping an overlay backend would also
create two projection ownership models and make lifecycle behavior harder
to reason about.

### Use `min-width` alone

It can only add width. Existing ASCII spaces sometimes need shrinking;
`space-width` supplies that shrink and `min-width` supplies the exact pixel
floor after rounding.

### Use `space-width` alone

The factor is rounded by redisplay and is not an exact absolute-pixel
guarantee. It also has no effect on tabs.

### Reuse completed-paragraph formatting after every edit

Changing its trigger from idle to synchronous would remove the delay but
still perform the wrong unit of work and would justify an unfinished line.
The editing path needs its own active-line and convergence semantics.

### Promise independent widths in multiple windows

One set of buffer text properties cannot encode two different break plans.
Pretending otherwise would publish an overflowing plan in at least one
window.

## Consequences

- Direct character APIs, search, syntax, save, point, and markers operate on
  source characters.
- APIs that preserve or inspect text properties can still observe EKP
  projection metadata; copy/kill removes that owned metadata.
- Foreign replacing display ownership makes only the affected hard
  paragraph natural and diagnostic.
- Tabs/non-ASCII whitespace remain natural if an exact plan would require
  shrinking them.
- Layout property changes do not create layout-only undo or modified-state
  changes.
- Ordinary edits perform bounded local flow with no delayed
  whole-paragraph snap.
- Very long single paragraphs trade automatic KP quality for bounded input
  latency until the user explicitly requests refill.

## Verification

The architecture requires three evidence layers:

- ERT for core plan/string parity, source and lifecycle invariants, exact
  property forms, live overflow/pullback/convergence, composition, undo,
  ownership, resize, multiwindow policy, and overload behavior;
- clean GUI probes for exact pixel glue, break/hyphen/indent rendering,
  point, region, mouse, and source invariants;
- dynamic before/immediate/settled recording for overflow and pullback,
  including source/projection hashes, zero overlays, pixel-fit assertions,
  and temporal review.

Final evidence and counts are recorded in the phase change/task documents,
not retroactively copied into this decision record.

## Rollback

Revert the text-property buffer renderer and live state machine together.
Do not retain a mixed physical/text-property backend. The semantic plan and
compatible string renderer can remain independently because their boundary
does not depend on buffer projection.
