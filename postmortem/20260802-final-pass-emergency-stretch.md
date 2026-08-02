# Final-Pass Emergency Stretch

Superseded note: the fixed-dimension emergency-stretch decision remains
current, but the historical hard/atomic fixed-cost exception below does not.
The accepted TeX active-path semantics are recorded in
`20260802-final-pass-active-path-preservation.md`.

## Context

The narrow showcase still produced isolated CJK source lines after the
policy work split automatic inline code from explicit hard atoms. That made
the earlier forbidden-run explanation too narrow: the visible failure was not
owned by unit suffix policy, buffer projection, or a screenshot-specific CJK
penalty. It was the final K-P pass assigning ordinary underfull candidates to
a fixed-cost emergency path outside the normal badness and demerits model.

## Decision

Keep the strict pass unchanged. When strict K-P cannot reach the paragraph
end, rerun with a finite background emergency stretch available to ordinary
underfull candidates. Those candidates still compute adjustment ratio,
badness, fitness, and demerits, so the global DP chooses between them by the
same model as ordinary K-P.

Task035 tried making that stretch scale with the line/candidate width. Full
regression falsified that detail. Task036 must use a TeX-style fixed
dimension instead: `ekp-emergency-stretch-pixel` is nil for an automatic
value around three display-font `M` widths, or a non-negative integer for a
fixed pixel value.

Keep the artificial fixed-cost emergency transition only for a truly overfull
first permitted hard or atomic run. That case has no ordinary candidate to
score. The Elisp 1D path, Elisp looseness/parshape path, and C engine use the
same rule.

## Why the old fallback was wrong

The earlier repair treated the symptom as a forbidden-run boundary problem
and remembered a fuller prefix. That explained one rigid-atom screenshot, but
it left the deeper model split in place: some ordinary underfull lines were
still not compared by normal badness, fitness, and demerits. At narrow widths
that flattened costs enough for one-glyph lines to remain competitive.

The accepted direction matches TeX's emergency-pass shape more closely:
emergency stretch is background flexibility for badness calculation, not a
separate shortcut that bypasses the line-quality model. It must be a fixed
dimension for the paragraph context, not a value derived from each line
measure.

## Test lesson

The first 84px/168px CJK oracle was invalid because it assumed two CJK glyphs
always fit when the width is twice one glyph. It ignored glue, kinsoku, and
attached punctuation. The replacement tests use the real mixed showcase text
and reject isolated CJK source lines through the core, public buffer path, and
GUI oracle.

## Consequences

There is no CJK-orphan penalty, no number-unit special case, and no renderer
compensation. Policy compilation still decides which gaps are legal before
DP. Core K-P owns the final line choices, the renderer must agree with the
chosen fixed stretch dimension, and both engines must stay in parity whenever
emergency stretch behavior changes. The 15-field C paragraph ABI remains a
constraint for task036.
