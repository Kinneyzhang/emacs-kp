# Issue: Final-Pass CJK Isolation 2026-08-01

## issue021 [x] Final-pass emergency layout can isolate CJK source lines at narrow widths

- **Status:** Resolved. Developer verification and independent review passed;
  the user accepted the visible result and requested the commit.
- **Summary:** At narrow measures, the final K-P pass can choose one-glyph CJK
  source lines in the mixed showcase paragraph. The visible symptom was first
  found around an explicit hard atom, then reproduced after automatic inline
  policy was split from hard atoms.
- **Environment:** Graphical Emacs 30.2 on macOS, narrow showcase widths,
  justify alignment, C module active for 1D layout. The same core problem is
  visible through the public buffer path, so this is not a renderer-only
  issue.
- **Repro:** Run `M-x ekp-showcase`, set a narrow width such as 168px or
  280px, and inspect the mixed Chinese/Latin inline paragraph. The failure
  appears when a CJK glyph becomes a complete source line even though the
  paragraph has legal non-emergency alternatives.
- **Expected vs Actual:** Ordinary underfull candidates in the final pass
  should remain normal K-P candidates: adjustment ratio, badness, fitness,
  and demerits decide the global path. Separately, the final pass must not
  lose all reachability when a legal candidate is overfull. The previous
  model confused that reachability safeguard with a hard/atomic fixed-cost
  scoring shortcut, and an empty-plan oracle could hide the failure.
- **Investigation:** The strict pass is not the bug. The buffer projector
  reproduces the semantic plan exactly and owns no line-choice compensation.
  The root is in core DP final-pass modeling: ordinary underfull candidates
  must receive finite emergency stretch and stay inside normal K-P scoring.
  TeX's `artificial_demerits` instead preserves the last active path in the
  final pass independent of content class; it is not owned by CJK, units,
  hard atoms, or their adjacency.
- **Required Outcome:** Eliminate unintended isolated CJK source lines at the
  checked narrow showcase widths without adding CJK-orphan, number-unit, or
  screenshot-specific penalties. Preserve source text, zero overlays,
  strict-pass behavior, Elisp/C parity, and the public C ABI.
- **Fix Plan:** Keep strict K-P unchanged. In the final pass, add a
  TeX-style fixed emergency-stretch dimension to ordinary underfull
  candidates and score them through normal badness, fitness, and demerits.
  Expose the dimension as `ekp-emergency-stretch-pixel`: nil auto-resolves to
  roughly three display-font `M` widths, while a non-negative integer fixes
  the pixel value. If an overfull candidate would otherwise remove the final
  active path to a breakpoint and no non-overfull candidate survives there,
  install the best provisional path with tight fitness and zero incremental
  demerits. The Elisp 1D path, looseness/parshape path, renderer semantics,
  and C engine share this rule without changing the 15-field C paragraph ABI.
- **Verification:**
  - Focused core single-CJK regressions pass 2/2.
  - The implementation lane's focused G009 suite covering core, C parity,
    hard atoms, public buffer projection, and GUI oracle passes 18/18.
  - Warning-as-error Elisp byte compilation and the C build pass.
  - Full regression falsified task035's line-width-sized emergency stretch:
    the stretch must be a fixed dimension, not derived from each candidate
    measure.
  - Task036's fixed-dimension RED cases pass and the three regressions broken
    by task035 remain green. Direct regression proves artificial lines add
    zero demerits and preserve a complete nonempty source-covering plan.
  - Focused root regressions pass 8/8, emergency selection passes 10/10,
    core/buffer/GUI oracle passes 8/8, full ERT passes 288/288, seeded and
    isolated suites pass 279/279, fuzz passes 300/300, and all build/static/
    release/performance gates pass.
  - Reviewed 42.78-second fullscreen single-window evidence at
    `/tmp/ekp-g009-evidence-retry.UOpPNp` covers 480→168→280→168 and
    no-hyphen→normal policy change, reports no isolated CJK source lines,
    exact source, zero overlays/stale spans, active C, and `VERDICT=PASS`.
- **User Confirmation:** On 2026-08-02, the user replied “可以了，提交吧”,
  accepting the visible result and requesting submission.
- **Resolved At:** 2026-08-02.
- **Resolved By:** `task036`; final-pass active-path preservation in core K-P.
- **Commit:** This commit (`fix: restore TeX final-pass reachability`).
