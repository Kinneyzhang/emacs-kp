# Change: Final-Pass Emergency Stretch 2026-08-02

## 2026-08-02 — Close issue021 after user visual confirmation

- **Modify** the global issue index and issue detail to close `issue021`.
- **User confirmation:** the user accepted the visible result with “可以了，
  提交吧” and requested the commit.
- **Behavior/Risk:** no runtime change; this closes the required user-visible
  acceptance gate after the developer, dynamic GUI, and independent review
  gates had already passed.

## 2026-08-02 — Complete task036 with TeX active-path preservation

- **Modify** `ekp.el` and `ekp_c/ekp_kp.c`: keep strict K-P unchanged; use
  fixed `ekp-emergency-stretch-pixel` for ordinary final-pass underfull
  candidates; and, only when an overfull candidate would otherwise extinguish
  the final active path to a breakpoint, install the best provisional path
  with tight fitness and zero incremental demerits.
- **Delete** the wrong-layer rule that forbade an otherwise legal boundary
  immediately before an explicit hard atom. Hard atoms forbid only their
  interior breaks; an overwide atom remains intact but need not stand alone.
- **Modify** the renderer to distribute the chosen line's actual rest using
  its actual TeX glue-set proportions. Elisp 1D, looseness/parshape, and C
  behavior match; the public 15-field/15-argument C contract is unchanged.
- **Strengthen tests** so isolated-CJK oracles require a nonempty plan that
  exactly covers the source, preventing an empty result from false-greening.
- **Review remediation:** make the changed test fixtures warning-clean under
  `byte-compile-error-on-warn` and document the optional eighth
  `EMERGENCY-STRETCH` argument in the C README API example.
- **Verification:** focused root regressions 8/8, emergency selector 10/10,
  core/buffer/GUI oracle 8/8, full ERT 288/288, seeded and isolated suites
  279/279, fuzz 300/300, warning-clean portable/native/debug/sanitize C
  builds, warning-as-error production and changed-test Elisp compilation,
  checkdoc/package/static/release gates, formal performance gates, and
  reviewed 42.78-second dynamic GUI evidence at
  `/tmp/ekp-g009-evidence-retry.UOpPNp` with `VERDICT=PASS`. Final independent
  code review returns `APPROVE` and architecture review returns `CLEAR`.
- **Behavior/Risk:** no CJK-orphan, unit, atom-adjacency, or screenshot
  heuristic exists. `task036` is complete; `issue021` closed after the
  user's 2026-08-02 visual confirmation.

The entries below are historical steps. Their hard/atomic fixed-cost wording
was superseded by the completed task036 semantics above.

## 2026-08-02 — Open task036 after task035 full-regression failure

- **Modify** `task035` to record the line-width-sized emergency stretch as a
  falsified attempt, not accepted current behavior.
- **Add** `task036` for TeX-style fixed-dimension final-pass emergency
  stretch. The implementation must expose `ekp-emergency-stretch-pixel`
  where nil auto-resolves to roughly three display-font `M` widths and a
  non-negative integer fixes the pixel value.
- **Preserve** the constraints: strict pass unchanged, 15-field C paragraph
  ABI unchanged, renderer semantics synchronized, fixed artificial emergency
  transition only for truly overfull first permitted hard/atomic runs, and no
  CJK-orphan/unit/screenshot-specific penalty.
- **Verification planned:** five new RED cases for fixed dimension behavior,
  nil auto, integer override, C parity without ABI growth, and renderer
  width/glue consistency; the three regressions broken by task035; focused
  G009 GUI/core/public-buffer/C parity; final fullscreen dynamic GUI evidence;
  full ERT/fuzz/build/static/release gates.
- **Behavior/Risk:** Documentation records the next atom of work only.
  Runtime behavior is not claimed fixed by this entry.

## 2026-08-02 — Close task035 as falsified

- **Modify** core K-P documentation to state the attempted final-pass model:
  strict pass unchanged; final pass gives ordinary underfull candidates a
  line-width-sized finite background emergency stretch; those candidates
  still use adjustment ratio, badness, fitness, and demerits.
- **Modify** user-facing hard-atom documentation to avoid claiming a
  hard-coded fullest-prefix fallback. A hard atom may occupy one overflow
  line, while the ordinary prefix remains a normal K-P decision with finite
  emergency stretch.
- **Modify** `issue021`, `task035`, the spec, plan, changelog, developer
  docs, C README, and global change/issue indexes to record the corrected
  owner and current acceptance boundary.
- **Add** `postmortem/20260802-final-pass-emergency-stretch.md` to explain
  why the ordinary-underfull fixed fallback was the wrong layer, why the
  TeX-style emergency-stretch model is used, and why the earlier 84px oracle
  was invalid.
- **Verification:** Focused core single-CJK regressions pass 2/2. The
  implementation lane reports focused G009 core/C/hard-atom/buffer/GUI-oracle
  coverage 18/18, warning-as-error byte compilation pass, and C build pass.
  Full regression then falsified the line-width stretch detail and forced the
  `task036` fixed-dimension follow-up. This documentation pass ran stale-text
  search and `git diff --check`.
- **Behavior/Risk:** `task035` is not current truth. No public configuration,
  C ABI, or renderer ownership change is accepted by this record. `issue021`
  remains open until task036 and the final user-visible visual acceptance gate
  pass.
