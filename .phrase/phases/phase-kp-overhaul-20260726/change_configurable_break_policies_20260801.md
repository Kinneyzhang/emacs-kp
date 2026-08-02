# Change: Configurable Break Policies 2026-08-01

Superseded note: the historical hard-atom adjacency rule recorded below was
removed by `task036`. Explicit atoms forbid only interior breaks; current
final-pass reachability semantics are recorded in
`change_final_pass_emergency_stretch_20260802.md`.

## 2026-08-01 — Plan task034 and record the red baseline

- **Add** `task034` for configurable break policies and the remaining
  orphan-glyph quality failure in the 280px showcase paragraph.
- **Modify** the text-property layout spec to make inline code wrapping,
  token policies, kinsoku profiles, overlong-token behavior, buffer measure,
  and region break-policy precedence explicit.
- **Modify** the text-property layout plan with M13, preserving the approved
  architecture: resolve policy intervals before tokenization, compile to the
  existing hyphen/break vectors before DP, and keep the C boundary at 15
  arguments and 15 batch fields.
- **Red baseline:** the latest screenshot shows the previous atom cascade is
  gone, but the crafted inline-code paragraph still permits pathological
  single-CJK source lines around the atom, including `行`, `内`, and `永`.
  The implementation oracle must reject any such single-CJK source line when
  a legal non-emergency alternative exists.
- **Verification contract:** focused RED/GREEN ERT for core, buffer, command,
  cache, and diagnostics; C/Elisp parity; 300-case property fuzz;
  warning-as-error Elisp compilation; C builds/tests; checkdoc/package/static
  release gates; and reviewed fullscreen GUI evidence for inline wrapping,
  no orphan glyph, explicit no-break, block verbatim, measure modes, and
  overlong-token modes.
- **Behavior/Risk:** Planning records only. Runtime behavior is unchanged.
  This entry opened `task034`; later entries record its implementation and
  closure.

## 2026-08-01 — Record task034 implementation and G004 GUI evidence

- **Modify** `task034`, M13, and the text-property layout spec with verified
  implementation facts from G002/G003/G004. At this point `task034` was still
  open pending later gates; G006 closure is recorded below.
- **Implementation evidence recorded:** core policy compilation preserves the
  existing C boundary; buffer policy ownership separates block skip faces from
  inline faces; public local/profile/region controls and diagnostics exist;
  showcase verification distinguishes automatic inline code from explicit
  `ekp-no-break` and verbatim block code.
- **GUI evidence recorded:** G004 passed GUI verifier ERT 7/7 and the clean
  single-window evidence run at `/tmp/ekp-g004-evidence.Tp77dW` reports 12/12
  checkpoints with no failed assertions, 25.75s/206-frame recording, no black
  segments, automatic inline wrapping 3→2→3 lines across 280→340→280, every
  internal inline split as source whitespace, exact source, zero overlays, C
  active, and a settled no-hyphen→normal policy transition with stale
  nil-plan spans at zero.
- **Verification status:** G005 repository-wide gates and G006 cleanup are
  recorded below.
- **Behavior/Risk:** Documentation synchronization only in this entry. Do not
  close `task034` or any user-confirmation issue from this G004-only evidence
  at this point in the sequence.

## 2026-08-01 — Record G005 final performance and repository gate evidence

- **Modify** `task034` and the policy-boundary postmortem with the confirmed
  G005 root causes and final gate evidence. At this point `task034` was still
  open pending G006 cleanup; G006 closure is recorded below.
- **Root causes recorded:** paragraph identity included unconditional policy
  measure, causing paragraph rebuilds; volatile property clearing fragmented
  equal property runs; policy analysis ran before paragraph-cache hits; and
  repeated same-paragraph/same-width requests duplicated semantic plan
  assembly.
- **Fix evidence recorded:** policy measure was removed from the semantic
  signature; adjacent equal filtered intervals are canonicalized; policy full
  analysis now uses a bounded two-tier cache keyed by clean source, canonical
  layout and raw face policy intervals, policy signature, and width context,
  with a measure tier only for actual width-sensitive no-break/overflow
  candidates; and semantic plans use a bounded per-paragraph plan cache. G007
  later strengthened the consumer boundary from shallow copies to
  consumer-owned copies of plan-owned mutable payloads; see the G008 entry
  below.
- **Performance evidence recorded:** formal four-interleaved evaluator reports
  layout parity true. Core baseline p50/p95 was 38.6679/51.6782ms and
  candidate p50/p95 was 21.7102/33.0040ms, for 43.8549%/36.1354% gains.
  Resize baseline p50/p95 was 43.3831/55.6250ms and candidate p50/p95 was
  22.0919/32.8202ms, for 49.0773%/40.9973% gains. Both candidate p95 values
  are under 50ms and both gain sets exceed 20%.
- **Live evaluator recorded:** the source-instrumented locked goal remains the
  known `validation_failed` debt; parity, zero-work, GC, conflict, and
  all-width-nonregression checks are true. Current C p95/p99 is
  26.449/26.740ms, a 76.65%/77.23% improvement. Current Elisp p95/p99 is
  49.940/52.017ms, a 91.64%/91.35% improvement. This is consistent with
  historical open `issue018` and is not a regression.
- **Production public-path evidence recorded:** three byte-compiled runs
  passed with zero GC. C append p99 was 1.361-1.368ms and hard p99 was
  1.876-1.891ms. Elisp append p99 was 1.692-1.775ms and hard p99 was
  2.100-2.230ms.
- **Verification recorded:** default ERT passed 262/262; seeded permuted ERT
  seed 20260728 passed 255/255; isolated per-test process suite exited 0;
  the subsequently added alias guard passed targeted verification; property
  fuzz passed 300/300; warning-as-error byte compilation passed; pinned
  package-lint at `35996f478d81e51dae4fa30d051f741895d07399` exited 0 with
  only an external obsolete warning from the local names dependency; checkdoc
  was empty; release, 49-entry dictionary manifest, pinned dictionary update,
  shell syntax, CI YAML, and diff-check passed; portable, native, debug, and
  sanitize C builds were warning-clean; module 1.6/4-thread smoke passed; and
  focused C verification passed 19/19 including the 15-argument/15-field
  boundary.
- **Verification status:** G005 gates are complete. G006 cleanup is recorded
  below.

## 2026-08-01 — Close task034 with G006 cleanup evidence

- **Modify** `task034`, M13, the global change index, and the policy-boundary
  postmortem with G006 cleanup evidence.
- **Cleanup evidence recorded:** G006 edited only `ekp.el` and
  `ekp-buffer.el`, removing redundant policy/cache code without changing the
  locked behavior.
- **Verification recorded:** targeted cleanup suites passed 6/6 and 3/3; full
  core ERT passed 125/125; full buffer ERT passed 120/120; warning-as-error
  byte compilation passed; diff-check passed; and the C build gate passed.
- **Behavior/Risk:** `task034` is closed for developer implementation,
  repository gates, performance gates, and cleanup gates. `issue021` remains
  open pending user visual confirmation. Independent final code/architecture
  review has not yet been claimed.

## 2026-08-01 — Resolve G006/G007 review blockers with G008 ownership fixes

- **Modify** `task034`, M13, developer documentation, changelog, and
  postmortems with the G007/G008 ownership and backend-contract corrections.
- **Review blockers resolved:** G006 cleanup's independent review found that
  shallow semantic-plan copies left nested line/gap/glue payloads mutable
  through cache hits, and that malformed non-nil C results could still be
  treated too softly. G007 changed the semantic-plan cache boundary to return
  consumer-owned copies of every plan-owned mutable payload: source string,
  context, boxes, offsets, lines, glues, gaps, and signatures. `para` remains
  intentionally shared because paragraph-cache ownership and append identity
  rely on that object. G007 final review then found one remaining alias:
  `copy-tree` did not copy strings inside the returned context, so mutating a
  dynamic policy suffix from the returned plan could poison the cached context.
  G008 resolves that final blocker by using the recursive context copier for
  cons/vector/string payloads both when the context snapshot/cache key is
  created and when a plan is returned.
- **Backend contract recorded:** only a nil whole C result or nil per-item
  breaks may fall back to Elisp. Any malformed non-nil single or batch output
  signals `ekp-backend-contract-error`. The C entry remains 15 arguments and
  the batch payload remains 15 fields.
- **Hard-atom boundary recorded:** the ordinary boundary immediately before a
  following explicit `ekp-no-break` atom is forbidden before DP. This direct
  pre-DP adjacency rule is separate from the final-pass emergency-stretch
  model and is locked by a focused regression.
- **Verification recorded:** formal resize parity is true; core baseline
  p50/p95 is 36.595/48.357ms and candidate p50/p95 is 23.266/36.363ms, for
  36.42%/24.80% gains. Resize baseline p50/p95 is 41.443/53.541ms and
  candidate p50/p95 is 23.761/36.889ms, for 42.67%/31.10% gains. ERT passes
  268/268; property fuzz passes 300/300; release gates pass; byte-compiled
  current public path records zero GC with C append/hard p99
  1.440-1.464/2.022-2.050ms and Elisp append/hard p99
  1.648-1.687/2.136-2.335ms, all below 16ms.
- **Behavior/Risk:** G007 resolved the first recorded G006 review blockers,
  and G008 resolves the final string-leaf context blocker, but this change
  record does not claim final independent APPROVE/CLEAR. `issue021` remains
  open pending user visual confirmation, and historical `issue018` remains
  open.

## 2026-08-02 — Record final independent review clearance

- **Review remediation:** the first final code-review pass found warning-only
  defects in changed test fixtures and a stale C README setter example. The
  fixtures now byte-compile with warnings as errors, and the README documents
  the optional eighth `EMERGENCY-STRETCH` argument.
- **Verification:** the remediation-focused ERT passes 5/5, full ERT passes
  288/288, every changed test file compiles warning-clean, and diff-check
  passes.
- **Independent verdicts:** the code-reviewer re-review returns `APPROVE` with
  zero findings; the subsequent architect review returns `CLEAR` and confirms
  core K-P ownership, content-independent final-pass reachability, Elisp/C
  parity, stable ABI, cache ownership, and complete nonempty GUI oracles.
- **Behavior/Risk:** `task034` and its independent developer gate are
  complete. `issue021` closed after the user's 2026-08-02 visual
  confirmation, and historical `issue018` remains open.
