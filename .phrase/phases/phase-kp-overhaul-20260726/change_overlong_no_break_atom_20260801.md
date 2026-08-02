# Change: Rigid Inline Atom Break Cascade 2026-08-01

Superseded note: this file records the earlier task033 repair. The current
ordinary-underfull final-pass model is recorded in
`change_final_pass_emergency_stretch_20260802.md`; an overwide atom remains
intact but is not guaranteed a standalone line, and atom adjacency has no
special break rule.

## 2026-08-01 — Implement and developer-verify task033

- **Modify** `ekp.el` so the emergency pass preserves its established first
  transition but adds a fullest-underfull prefix fallback only when a
  forbidden break run jumps directly to an overfull permitted candidate.
- **Modify** `ekp_c/ekp_kp.c` with the same state transition and keep the
  looseness/parshape Elisp path in parity.
- **Add** core, public-buffer, and GUI regressions covering diverse CJK
  prefixes, exact source boundaries, atom integrity, Elisp/C parity, and
  repeated 480→280→340→280 reflow.
- **Modify** user/developer documentation and add
  `postmortem/20260801-forbidden-run-emergency-boundary.md` to record why the
  broader fullest-prefix replacement was rejected.
- **Verification:** RED reproduced boundary 1 instead of 11. GREEN passes
  normal, random-order, and isolated 201-test runs; 300 C/Elisp fuzz cases;
  warning-as-error Elisp compilation; package-lint; checkdoc; release and
  dictionary gates; warning-clean debug/sanitize/portable C builds; and
  reviewed dynamic GUI evidence at
  `/private/tmp/ekp-atom-gui-final-Beg8hb` with verdict PASS.
- **Behavior/Risk:** Strict K-P, ordinary emergency layouts, buffer
  projection, source text, and public APIs are unchanged. The additional
  relaxation is limited to emergency scans that cross forbidden break
  positions before an overfull candidate.

## 2026-08-01 — Plan issue021 and task033

- **Add** `issue021` with the 280px showcase reproduction and deterministic
  width-40 batch analogue.
- **Add** `task033` and M12 acceptance gates for the core emergency-break
  correction, Elisp/C parity, and real GUI verification.
- **Modify** the text-property layout spec to require a full ordinary prefix
  line and one intact rigid atom line rather than one-glyph emergency
  fragmentation.
- **Behavior/Risk:** Planning records only. Runtime behavior is unchanged.
  The change will affect only the second pass used after strict K-P has
  already proved the paragraph unreachable.
