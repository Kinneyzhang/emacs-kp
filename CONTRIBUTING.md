# Contributing to emacs-kp

Thanks for your interest.  This document covers the essentials for
making a change that will pass review and CI.

## Development setup

```bash
EMACS=emacs   # or /path/to/Emacs

# Byte-compile with warnings as errors (CI does this)
$EMACS -Q --batch -L . \
  --eval '(setq byte-compile-error-on-warn t)' \
  -f batch-byte-compile ekp.el ekp-utils.el ekp-hyphen.el ekp-region.el

# Run the ERT suite (C-module tests auto-skip if not built)
tests/run-tests.sh $EMACS

# Build the C module (required for the parity tests and fuzz)
make -C ekp_c              # add DEBUG=1 for ASan/UBSan

# Property fuzz: 300 random cases, asserts C and Elisp agree byte-for-byte
$EMACS -Q --batch -L . -l tests/ekp-fuzz.el
```

## Ground rules

- **The two engines must produce byte-identical output.** Any change
  to the demerits or line-metric formulas must touch both
  `ekp--dp-run-1d` (Elisp) and `dp_process_position` (`ekp_c/ekp_kp.c`),
  and the fuzz suite must stay at 300/300.
- **The layout is lossless.** The renderer's marker properties
  (`ekp-glue`, `ekp-soft-break`, `ekp-soft-hyphen`, `ekp-hidden`) must
  round-trip exactly through `ekp-unjustify-region`.
- Any C-module API change bumps `EKP_VERSION_MINOR` and the matching
  `ekp-c-module-required-version`, and rebuilds the module.
- New behavior needs an ERT test.  Buffer-level behavior (save,
  isearch, undo, kill/yank, mode interactions) goes in
  `tests/ekp-region-tests.el`.

## Style

- `lexical-binding: t` everywhere; keep byte-compilation warning-free.
- `checkdoc` clean (CI enforces it): imperative docstring first lines,
  arguments mentioned in uppercase, two spaces after a sentence.
- `package-lint` clean: the `ekp-` / `ekp-region-` namespaces, proper
  autoload cookies on interactive entry points.
- Match the surrounding code; keep comments about *why*, not *what*.

## Commits

Conventional Commits (`feat:`, `fix:`, `perf:`, `refactor!:`,
`docs:`, `test:`, `chore:`).  Explain the reasoning in the body, not
just the change.

## License

By contributing you agree that your contributions are licensed under
GPL-3.0-or-later, matching the project (see `COPYING`).
