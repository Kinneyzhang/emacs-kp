# Plan: Text-Property KP Layout Engine 2026-07-29

The authoritative user-visible contract is
`spec_text_property_layout_engine_20260729.md`.

## Goal

Resolve `issue011` at the representation owner.  The real buffer keeps only
the user's logical characters.  EKP computes Knuth-Plass breaks, glue widths,
indentation, and discretionary hyphens as a reusable layout plan, then a
buffer renderer projects that plan with text properties on existing source
characters.

No buffer path may insert layout spaces, soft newlines, or discretionary
hyphens.  No buffer path may use overlays.

## User Contract

- `buffer-string`, `buffer-substring-no-properties`, direct character
  access, search, syntax, markers, point, save, and ordinary editing observe
  the original character sequence.
- Manual commands produce the same KP break and glue decisions as the
  string API.
- `ekp-auto-justify-mode` keeps the current unfinished visual row natural and
  preserves unaffected committed break anchors during continuous editing.
- Ordinary same-row edits do not call whole-hard-line planning. A dirty edit
  transaction owns the local natural island and a committed projection owns
  the saved source, plan, signatures, spans, and anchors.
- Native soft-wrap crossing atomically replans the completed hard-line prefix
  once. Signature diffing limits the property update; core DP remains the
  owner of the committed result.
- Hard newline/paragraph completion, the next real edit outside the dirty
  island, explicit refill, and width/font/layout-context change are commit
  events. Point-only motion is never one.
- Reversible source edits restore the saved projection exactly rather than
  relying on a recomputation to happen to reproduce it.
- Layout-property updates do not change the modified flag, undo history,
  character-modified tick, point, mark, or external modification hooks.
- Turning the mode off removes only EKP-owned projection properties and
  restores every pre-existing property exactly.
- Final-pass emergency layout remains owned by core K-P. Ordinary underfull
  candidates use finite emergency stretch and normal badness/demerits; the
  final active path is preserved with TeX-style zero-increment artificial
  demerits only when an overfull candidate would otherwise extinguish it.
  The buffer renderer and policy compiler must not add CJK-orphan, unit,
  hard-atom-adjacency, or screenshot-specific compensation.

## Architecture

### Core owner: `ekp.el`

Introduce one semantic paragraph plan built from the existing `ekp-para`,
DP breaks, line glues, and source box offsets.  A plan contains ordered
lines; each line records its source/box bounds, leading indentation,
interior glue targets, chosen break, and discretionary-hyphen state.

The plan contains no buffer positions, markers, overlays, or display
property forms.

`ekp-pixel-justify` remains the public string formatter.  Its renderer
consumes the plan and preserves the existing reversible string output and
tests.  The buffer renderer consumes the same plan but has different
representation rights.

### Buffer owner: `ekp-buffer.el`

The buffer renderer maps plan operations to existing source characters:

- ASCII source-space glue:
  `((space-width FACTOR) (min-width ((TARGET))))`.
- Zero-source CJK/mixed glue: `min-width` on the preceding complete
  grapheme, with a target equal to the grapheme's natural width plus glue.
- Leading indentation/alignment: `line-prefix` over the complete planned
  display line.
- Break at source whitespace: the first break-space displays as a newline;
  remaining boundary whitespace displays as empty.
- CJK break: the preceding complete grapheme displays as
  `GRAPHEME + NEWLINE`.
- Latin discretionary break: the preceding complete grapheme displays as
  `GRAPHEME + HYPHEN + NEWLINE`, with cursor anchoring on the reproduced
  grapheme.
- Paragraph-edge whitespace stripped by the KP model remains in the buffer
  and displays as empty.

All projection properties are installed through `with-silent-modifications`
and are nonsticky.  Copy/kill strips only EKP projection metadata so stale
layout cannot be yanked elsewhere.

### Display ownership

EKP never overwrites an unowned replacing `display`, `line-prefix`, or
`wrap-prefix` value.  A paragraph with a property that cannot be composed
losslessly is kept verbatim and reported by diagnostics.  Removing a layout
restores the exact pre-existing property values.

### Multiple windows

Text properties are buffer-wide.  One buffer therefore has one
authoritative plan: the narrowest live window showing that buffer, matching
the current `ekp-buffer--effective-width` behavior.  Wider windows may have
unused right-side space; no window may receive an overflowing plan.
Simultaneous different KP plans for one buffer are explicitly not claimed.

## Live Editing State Machine

1. The active hard line owns one committed projection: baseline source,
   whole-hard-line plan, line signatures, projected spans, and anchors.
2. The first edit in one visual row snapshots that state and opens a dirty
   island. `before-change` removes only the affected projection; it never
   clears an unrelated suffix.
3. Same-row edits update source and dirty bounds only. Existing glue and
   native soft wrapping absorb local edits without whole-hard-line DP.
4. Restoring the baseline source restores the saved owned properties and
   committed state immediately and closes the transaction.
5. Crossing a native soft-wrap boundary commits once: recompute or reuse the
   whole-hard-line plan, derive the completed prefix, and apply the
   signature-diff update as one silent publication. The new current row is
   natural.
6. A real edit outside the dirty island commits the old transaction before
   opening a new one. Hard newline/paragraph completion, explicit refill, and
   width/font/layout-context changes are also commit boundaries.
7. Point-only motion anywhere is display read-only and cannot commit,
   finalize, plan, touch cache identity, or write projection properties.
8. IME composition stays entirely native until commit. Stale generations,
   foreign display ownership, unsupported shrink, oversized hard lines, or
   projection failures fail closed to native display for that hard line.
9. Hard-paragraph completion uses one existing full KP quality pass and
   starts a new natural active hard line.

## Milestones and Gates

### M1 — Core layout plan and string parity

- Add plan structs and `ekp-layout-plan`.
- Make the string renderer consume the plan.
- Gate: every existing core rendered string remains
  `equal-including-properties`; C/Elisp parity and fuzz remain green.

### M2 — Static text-property buffer renderer

- Replace delete/insert justification with property projection.
- Remove physical inversion/save/isearch adapters that no longer own a
  character transformation.
- Gate: source characters, positions, modified state, undo, hooks, and
  foreign properties are invariant; exact GUI glue/break/hyphen/indent
  probes pass.

### M3 — Seamless live-editing foundation

- Implement composition deferral, resize generation cancellation,
  visible-first large-buffer work, the single-paragraph planning guard, and
  exact editor-state preservation.
- Gate: source, point/mark, whitespace, IME, resize, and teardown regressions
  are locked before selecting the final live row-boundary model.

### M4 — Product and repository closure

- Update bilingual user/developer documentation, `issue011`, task/change
  records, changelog, and a design postmortem.
- Run default/permuted/isolated ERT, fuzz, byte compilation with warnings as
  errors, checkdoc, C parity/build gates, GUI matrix, dynamic verification,
  anti-slop cleanup, and independent architecture/code review.

### M5 — Interaction regression closure

- Preserve the mark marker and `mark-active` as independent editor state
  across every reprojection, including showcase width changes.
- Keep active-line edge whitespace natural while retaining edge cleanup on
  committed static lines.
- Gate: focused red/green ERT, a public-command interaction matrix, clean
  GUI width-key/single-space evidence, full repository gates, and user
  confirmation for `issue012` and `issue013`.

### M6 — Native progressive editing

- Delete the partial-KP live-flow model and its lookahead/convergence state.
- Keep the active source tail under native Emacs redisplay ownership.
- Align only completed native screen rows without live break/hyphen
  projection; run complete KP at hard-paragraph completion.
- Own native soft wrapping while auto mode is active, including narrow
  partial-width windows, and restore the previous display-variable
  ownership on teardown.
- Gate: per-keystroke mixed-text GUI evidence, backward-edit invalidation,
  paragraph-completion transition, narrow split-window soft wrapping, full
  repository gates, and user confirmation for `issue014` and `issue015`.

Historical note: M6 is superseded for live planning by M7. Its active-tail
and soft-wrap lifecycle decisions remain prerequisites, but native visual
rows are not the durable planning unit.

### M7 — Semantic hard-line prefix editing

- Status: implementation, automated/performance/GUI gates, and independent
  code/architecture reviews are complete. `issue016` remains open for the
  user's visible editing confirmation.
- Replace native-row commitment with whole-hard-line plan consumption in
  `ekp-buffer`.
- Keep `ekp.el`, core DP semantics, C ABI, DP schema, and
  `ekp-layout-plan` contracts unchanged.
- Project only complete semantic plan lines before point; keep the
  point-containing plan line and all following source natural.
- Add buffer-local history cache entries keyed by hard-line text,
  text-property/layout context, authoritative width, font/face/text-scale
  context, and EKP layout parameters.
- Use semantic line signatures to avoid rewriting unchanged prefixes and
  to prove later edits can revise earlier breaks/glue together.
- Gate: public-path red/green ERT, GUI dynamic recording, latency/cache
  benchmark, full repository gates, independent review, and user
  confirmation for `issue016`.

### M8 — Source-edit-owned live frontier

- Status: superseded by M10 for edit-trigger ownership. The zero-work
  point-motion invariant remains current; `issue017` remains open for the
  user's visible confirmation.
- Preserve the latest real source-edit position as the live frontier.
- Make point-only motion within the active hard line perform no DP, cache,
  or text-property work.
- Preserve the frontier across width/font/layout-context reflow and map it
  into the resulting whole-hard-line plan.
- Gate: focused property-identity RED/GREEN ERT, public earlier-line edit,
  reflow-after-motion regression, full repository gates, dynamic GUI
  point-motion evidence, independent review, and user confirmation.

### M9 — Narrow unique-append latency

- Status: re-profiled as `issue018`/`task030` after M10; ready for work.
  Ordinary same-row planning is gone, so only structural-commit spikes are
  in scope.
- Establish a repeatable width/length matrix before optimizing.
- Reduce new-source-state planning cost without stale plan reuse,
  debounce, skipped publication, global GC changes, or weaker layout
  semantics.
- Keep task029's point-motion zero-work invariant as a permanent gate.

### M10 — Stable live projection transactions

- Status: complete as `issue019`/`task031`; automated, static, benchmark,
  and reviewed temporal GUI gates pass. `issue019` remains open only for
  user-visible confirmation.
- Replace frontier-owned native suffix invalidation with a committed
  projection baseline and one persistent dirty edit transaction.
- Preserve unaffected break anchors during middle-line edits; use native
  soft wrapping for local word migration.
- Replan only at soft-wrap or structural commit boundaries and publish the
  changed prefix atomically.
- Restore reversible edits exactly from the saved baseline.
- Preserve core DP/C ABI/schema/plan contracts and the point-motion
  zero-work invariant.
- Gate: focused RED/GREEN public-path ERT, complete repository gates,
  re-profiled live benchmarks, and temporal GUI evidence.

### M11 — C-backed resize latency

- Status: `task032` developer-complete; `issue020` awaits user confirmation.
- Freeze the current portable module and measure an interleaved
  baseline/candidate width-and-length matrix with raw p50/p95 evidence.
- Attribute complete resize time across plan construction, Emacs/C
  marshalling, C DP, and projection publication before selecting a change.
- Require exact frozen-C and Elisp layout parity; no stale width reuse,
  debounce inflation, skipped reflows, approximate planning, or global GC
  workaround is acceptable.
- Gate: at least 20% paired p50/p95 improvement, candidate p95 at or below
  50 ms, complete automated/static gates, and temporal GUI resize evidence.
- Result: core p50/p95 improved by 33.25%/34.09% to 15.318/27.687 ms;
  complete resize improved by 43.51%/41.03% to 15.900/27.487 ms. Exact
  frozen-C/Elisp parity and all automated, static, and GUI gates pass.

### M12 — Rigid inline atom emergency breaking

- Status: `task033` implemented and developer-verified as the earlier
  rigid-atom repair. `task036` supersedes its ordinary-underfull emergency
  model; `issue021` was closed after user-visible confirmation.
- Reproduce the showcase's narrow CJK prefix plus `ekp-no-break` atom through
  the public string and semantic-plan paths before changing the algorithm.
- Historical task033 design: keep the strict K-P pass and the established
  first-candidate emergency transition unchanged, then add a narrow
  forbidden-run prefix fallback. The current task036 model replaces that
  fallback with fixed final-pass emergency stretch plus content-independent
  active-path preservation.
- Preserve atom integrity, source text, Elisp/C parity, looseness/parshape
  semantics, append correctness, and the buffer projection contract.
- Gate: focused RED/GREEN core and buffer regressions, complete ERT in normal
  and random order, isolated tests, C/Elisp fuzz, warning-as-error builds,
  static/release checks, and clean static plus dynamic 280px GUI evidence.
- Result: the failing boundary moved from 1 to the full prefix boundary 11 in
  Elisp, C, and public buffer paths. Normal/random/isolated ERT pass 201/201,
  fuzz passes 300/300, static and build gates pass, and reviewed
  480→280→340→280 GUI evidence returns PASS.

### M13 — Configurable break policies and orphan-glyph closure

- Status: `task034` implementation, focused automated evidence, and G004 GUI
  evidence are recorded. G005 repository-wide gates and G006 cleanup are
  complete. `task034` is closed for developer work, `issue021` is closed
  after user visual confirmation, and independent final code review returned
  APPROVE with architecture status CLEAR.
- Implement the locked A2/B2/C2/D1/E1/F1/G2/H1/I1/J1/K1/M1 contract:
  inline code defaults to no-hyphen rather than no-break, known inline faces
  are recognized through mode profiles, region policy outranks explicit
  local values which outrank mode profiles and globals, block faces remain
  verbatim, explicit no-break never downgrades, automatic no-break downgrades
  to no-hyphen when overwide, inline and block faces use separate paths,
  manual properties stay session-only, hyphenation defaults to auto,
  URL/path/identifier default to no-hyphen, compact number-unit defaults to
  no-break, kinsoku defaults to common, overlong tokens default to emergency,
  and buffer measure defaults to the narrowest live window.
- Keep the chosen architecture: resolve private structural policy intervals
  before tokenization, compile them into existing hyphen positions and
  forbidden-break vectors before DP, preserve the original source in the
  semantic plan, and keep the C boundary at the current 15 arguments and
  15-field batch payload unless architecture review is reopened.
- Split block and inline ownership in `ekp-buffer`: `ekp-buffer-skip-faces`
  remains paragraph-level verbatim, `ekp-buffer-inline-faces` annotates only
  exact inline intervals, and `ekp-buffer-mode-policy-alist` is consulted
  without auto-copying profile values into buffer locals.
- Add public region controls for `ekp-break-policy`: normal, enable
  hyphenation, disable hyphenation, and clear. Existing no-break/verbatim
  commands remain the only hard-atom and paragraph-bypass controls.
- Fix the remaining visible quality bug from the user's latest screenshot:
  the 280px showcase path must not isolate any pathological single-CJK source
  line around inline code, including `行`, `内`, or `永`, when a legal
  non-emergency alternative exists.
- Gate: follow the RALPLAN test specification R1-R8 plus GUI verification.
  Required evidence includes focused RED/GREEN ERT, full normal/permuted/
  isolated ERT, 300-case property fuzz, C/Elisp parity, warning-as-error
  Elisp compilation, C builds/tests, checkdoc/package/static/release gates,
  and reviewed fullscreen single-window screenshots for inline wrapping,
  no orphan glyph, explicit no-break, block verbatim, measure modes, and
  overlong-token modes.
- Current evidence: G002 records core policy compilation, C parity, 300-case
  fuzz, byte compilation, checkdoc, C build, and focused policy/cache gates.
  G003 records buffer/profile/local/region controls, diagnostics, generation
  reflow, source/editor invariants, focused core/buffer suites, byte
  compilation, and checkdoc. G004 records GUI verifier ERT 7/7 and a clean
  single-window run at `/tmp/ekp-g004-evidence.Tp77dW` with 12/12 checkpoints,
  no failed assertions, 25.75s/206-frame recording, no black segments,
  automatic inline wrapping 3→2→3 lines across 280→340→280, source-space
  internal inline breaks, explicit hard atom, block verbatim, C active, and
  no stale policy projection after the no-hyphen→normal transition.
- Final evidence: G005 recorded repository-wide default/seeded/isolated ERT,
  property fuzz, warning-clean Elisp/C builds, checkdoc/package/release/
  dictionary/static gates, performance checks, and full diff review. G006
  cleanup touched only `ekp.el` and `ekp-buffer.el`; targeted cleanup suites
  passed 6/6 and 3/3, full core ERT passed 125/125, full buffer ERT passed
  120/120, and byte compilation, diff-check, and C build gates passed. G006
  was then review-blocked on nested semantic-plan cache aliasing and malformed
  non-nil C fallback semantics. G007 resolved those first blockers: cached
  semantic plans return consumer-owned copies of plan-owned mutable payloads
  while intentionally sharing `para`; malformed non-nil C single/batch output
  signals `ekp-backend-contract-error`, with nil-result Elisp fallback and
  the 15-argument/15-field ABI unchanged. G007 final review found one
  remaining string-leaf context alias; G008 resolves it by recursively
  copying cons/vector/string context payloads both for snapshot/cache-key
  creation and returned plans. Fresh G008 gates record formal resize parity
  true, ERT 268/268, fuzz 300/300, release pass, core p50/p95
  23.266/36.363ms, resize p50/p95 23.761/36.889ms, and byte-compiled current
  public path p99 below 16ms with zero GC. After review remediation made all
  changed tests warning-clean and synchronized the eighth C setter parameter,
  final independent code review returned APPROVE and architecture review
  returned CLEAR.

### M14 — Final-pass emergency stretch

- Status: `task035` was falsified; `task036` is implemented and its developer
  gates pass. `issue021` is closed after user-visible confirmation.
- Correct the core K-P owner, not the renderer or policy compiler. Strict
  pass behavior remains unchanged.
- In the final pass, ordinary underfull candidates receive finite background
  emergency stretch and still compute adjustment ratio, badness, fitness, and
  demerits. Separately, when an overfull candidate would otherwise eliminate
  the final active path to a breakpoint and no non-overfull candidate survives
  there, install the best provisional path with tight fitness and zero
  incremental demerits, matching TeX's `artificial_demerits` purpose.
- Do not add CJK-orphan, unit, or screenshot-specific penalties. Unit suffix
  configuration remains only a token-classification input, not a layout
  scoring rule.
- Gate: focused core/public-buffer/C parity tests and GUI-oracle checks must
  reject any isolated CJK source line in the showcase paragraph at checked
  widths. Final fullscreen visual review remains required before closing
  `issue021`.
- Result: the Elisp 1D, looseness/parshape, and C paths share the same rule;
  explicit atom interiors remain unbreakable but adjacent legal boundaries
  remain legal; the public 15-field/15-argument C contract is unchanged.
  Focused regressions pass 8/8, the emergency selector passes 10/10, the
  core/buffer/GUI oracle passes 8/8, full ERT passes 288/288, seeded and
  isolated core suites pass 279/279, fuzz passes 300/300, all build/static/
  release gates pass, performance gates pass, and reviewed dynamic GUI
  evidence at `/tmp/ekp-g009-evidence-retry.UOpPNp` returns PASS.

## Stop Gates

- Stop the affected paragraph instead of stealing a foreign replacing
  display owner.
- Stop exact shrink projection for tabs or non-ASCII whitespace when
  `space-width` cannot express it; keep that paragraph verbatim and report
  why.
- Stop if the live-edit implementation appears to require a core DP, C
  ABI, DP schema, or `ekp-layout-plan` semantic change; that means the
  buffer/core boundary has been crossed incorrectly.
- Stop and return to diagnosis after two failed fixes for the same display
  or live-edit invariant.
- Do not claim completion while any source-character, undo, modified-state,
  property-restoration, GUI, or dynamic-edit invariant is unproved.

## Rollback

The string renderer can be restored to the current direct implementation
because M1 preserves its public result.  The buffer renderer can be reverted
to the last released physical representation only as a full rollback; no
compatibility shim or mixed physical/property backend will be retained.
