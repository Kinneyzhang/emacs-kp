# Change: Narrow Live-Append Latency 2026-07-30

## 2026-07-30 — Implement exact incremental live commits

- **Modify** `ekp.el`: retain prepared paragraph and 1D-DP state in layout
  plans, retokenize only the last incomplete word plus appended suffix,
  extend prefix/break/protrusion data from the first dirty box, resume Elisp
  DP from the earliest state that can reach the new tail, and reuse unchanged
  layout-line records.
- **Modify** `ekp-buffer.el`: reconstruct only the live dirty island, attempt
  exact append planning before a full plan, preserve cache identity, and
  release the old hard-line transaction before the next paragraph becomes
  live.
- **Modify** `ekp_c/ekp.c`: validate signed 32-bit values with one
  `extract_integer` call instead of three Lisp predicate/comparison calls.
- **Add** exact append/fallback, multi-engine, unsafe-context,
  stable-boundary, dirty-island, and invalid-C-vector regressions, plus a
  frozen baseline/candidate evaluator covering five widths, four paragraph
  lengths, C/Elisp, and two GC modes.
- **Performance:** the four-round source-instrumented evaluator reduces
  80-pixel C p95/p99 from 114.717/119.201 ms to 25.490/25.785 ms and Elisp
  from 588.017/597.093 ms to 43.860/47.578 ms. Ordinary keys remain a
  zero-work path at 0.595 ms p99 and all widths avoid regression.
- **Production check:** with production files byte-compiled, three repeated
  public-command runs measure append p99 at 1.158–1.326 ms for C and
  1.429–1.438 ms for pure Elisp; hard-boundary p99 is 1.251–1.363 ms and
  1.457–1.470 ms respectively.
- **Decision:** retain the C module. Rust would cross the same Emacs module
  ABI and cannot remove the Elisp-owned tokenization, measurement,
  transaction, or publication work. Rewriting the already sub-millisecond C
  layer would add Cargo and cross-platform release surface without a
  measured end-to-end gain.
- **Open gate:** the locked source-instrumented evaluator still exceeds its
  absolute 16 ms C/Elisp target, so `task030` and `issue018` remain open even
  though the byte-compiled production path is within budget.
