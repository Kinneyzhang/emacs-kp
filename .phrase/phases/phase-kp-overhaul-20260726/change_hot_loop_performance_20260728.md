# Change Log: Hot-loop Performance 2026-07-28

## task012

- **Modify** — `ekp-utils.el`
  - Accumulate tokenizer box fragments in reverse and concatenate once when
    a box is emitted.
  - Preserve zero-width attachment and text-property behavior.

- **Modify** — `ekp-hyphen.el`
  - Distinguish cache misses from cached nil with an uninterned sentinel.
  - Build dense inserted output from original word slices in one
    concatenation.

- **Modify** — `tests/ekp-tests.el`, `tests/ekp-bench.el`
  - Add nil call-count, long propertized token, and dense insertion
    regressions.
  - Add reusable 1k–8k adversarial builder measurements.

- **Modify/Add** — developer/audit/issue documentation and
  `postmortem/20260728-linear-text-builders.md`
  - Record the evidence threshold, measurements, ownership decision, and
    rollback boundary.

## Validation

- Nil cache red/green: 0/1 → 1/1; focused behavior tests: 3/3.
- Source-mode 8,000-character tokenizer: 3.133 s → 1.100 s.
- Source-mode 8,000-character dense insertion: 0.945 s → 0.013 s.
- Full ERT: 124/124; C/Elisp fuzz: 300/300.
- Warning-as-error production byte compilation and production checkdoc:
  pass.

## Behavior and Risk

- Public tokenization and hyphenation output is unchanged.
- The cache now retains negative results until its existing invalidation
  boundary.
- The implementation adds no builder abstraction or dependency.
