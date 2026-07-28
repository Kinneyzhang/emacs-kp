# Dictionary Syntax Is Part of the Layout Model

## Context

The bundle pulled moving LibreOffice HEAD with GNU-specific commands and no
manifest.  Four dictionaries contain slash syntax.  Three use libhyphen
replacement rules whose golden outputs include `asszony -> asz=szony`,
`paral·lel -> pa=ral=lel`, and `adhem -> e`; Esperanto has slash-prefixed
patterns too.  EKP silently skipped every such line.

One bundled Sanskrit file also lacked an in-file license statement and a
package companion applying terms to that exact hyphenation data.  LibreOffice
had reverted a generic COPYING addition because spelling and hyphenation data
may have different licenses.

## Decision

Slash/replacement dictionaries fail closed with a typed condition.  A
replacement changes line-end text, next-line text, and measured width only if
that breakpoint wins.  EKP boxes have fixed text/width, and both Elisp and C
DP consume those fixed arrays.  Parsing only the ordinary part would emit
wrong language; implementing only output substitution would make DP widths
wrong.  Correct support therefore belongs to a future explicit
break-alternative model across tokenization, DP, C ABI, and lossless render
inversion—not a parser patch.

The bundle is pinned at one snapshot with normalized per-file checksums.
Basque is explicitly retained as a legacy checked-in byte because it is not
in that snapshot and has its own notice.  Sanskrit is removed until license
terms for the exact hyphenation data are authoritative.

## Alternatives

- Silently skipping replacement patterns was rejected because it overstates
  language support.
- Treating the prefix before `/` as a normal pattern was rejected because it
  produces incorrect breaks such as `as-szony`.
- Renderer-only substitutions were rejected because line costs would still
  use the unreplaced glyph widths.
- A new general parser/DP representation was deferred because four languages
  do not justify a cross-engine ABI expansion without product demand.

## Consequences

`eo`, `ca`, `hu_HU`, and `sq_AL` now produce a diagnostic condition instead
of partial hyphenation.  Forty-nine dictionary files remain bundled; the
ordinary-pattern subset is usable for 45.  Offline and upstream gates make
source, checksum, syntax, and license drift visible.

## Rollback

Reverting requires restoring the previous bundle, updater, claims, and silent
skip behavior together.  Do not re-add Sanskrit or advertise replacement
support without their missing evidence/model.
