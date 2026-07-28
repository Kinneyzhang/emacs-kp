# Plan: Hot-loop Performance and Nil Cache 2026-07-28

## Goal

Remove only measured allocation pathologies and make cached “no hyphen”
results real cache hits without changing tokenization or hyphenation output.

## Baseline Evidence

Source-mode, three-run adversarial benchmark on Emacs 30.2:

| Length | one Latin token | dense hyphen insertion |
|---:|---:|---:|
| 1,000 | 0.304 s | 0.082 s |
| 2,000 | 0.755 s | 0.061 s |
| 4,000 | 1.202 s | 0.291 s |
| 8,000 | 3.133 s | 0.945 s |

At 8× input, tokenizer time grew about 10.3× and dense insertion about
11.5×.  Both justify a linear builder.  Separately, two lookups of a word
with no breaks call `ekp-hyphen--compute` twice.

## Scope

1. Add a failing call-count test for cached nil and behavior controls for
   long/propertized tokenizer and dense insertion output.
2. Replace per-character accumulator concatenation with fragment lists that
   concatenate once per emitted box.
3. Build inserted hyphen output from original word slices in one pass.
4. Use an explicit miss sentinel for the word-position cache.
5. Add reusable adversarial scaling output to `tests/ekp-bench.el`.
6. Re-run before/after scaling, focused/full ERT, warning-as-error byte
   compilation, checkdoc, fuzz, and diff review.

## Non-goals

- Do not move tokenization or hyphenation into C.
- Do not add a rope/builder abstraction or dependency.
- Do not optimize ordinary paragraph code whose scaling is not measured.

## Rollback

Restore the string accumulators and insertion loop together.  Keep the nil
cache regression test even if the builder change is reverted.
