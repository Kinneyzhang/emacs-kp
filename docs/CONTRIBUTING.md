# Contributing to emacs-kp

Thanks for your interest.  This document covers the essentials for
making a change that will pass review and CI.

## Development setup

```bash
EMACS=emacs   # or /path/to/Emacs

# Byte-compile with warnings as errors (CI does this)
$EMACS -Q --batch -L . \
  --eval '(setq byte-compile-error-on-warn t)' \
  -f batch-byte-compile ekp.el ekp-utils.el ekp-hyphen.el ekp-buffer.el

# Run the ERT suite (C-module tests auto-skip if not built)
tests/run-tests.sh $EMACS

# Build the C module (required for the parity tests and fuzz)
make -C ekp_c PROFILE=portable

# Property fuzz: 300 random cases, asserts C and Elisp agree byte-for-byte
$EMACS -Q --batch -L . -l tests/ekp-fuzz.el

# Check release/CI/version invariants
tests/check-release.sh

# Check dictionary inventory and pinned upstream bytes
tests/check-dictionaries.sh
dictionaries/update.sh check
```

## Ground rules

- **The two engines must produce byte-identical output.** Any change
  to the demerits or line-metric formulas must touch both
  `ekp--dp-run-1d` (Elisp) and `dp_process_position` (`ekp_c/ekp_kp.c`),
  and the fuzz suite must stay at 300/300.
- **The two renderers have different rights.** The string renderer's
  physical marker vocabulary (`ekp-glue`, `ekp-soft-break`,
  `ekp-soft-hyphen`, `ekp-hidden`) stays lossless and compatible. The
  buffer renderer must create no overlay or source character and may use
  only EKP-owned text properties on existing characters.
- Any C-module API change bumps `EKP_VERSION_MINOR` and the matching
  `ekp-c-module-required-version`, and rebuilds the module.
- New behavior needs an ERT test.  Buffer-level behavior (save,
  isearch, undo, kill/yank, mode interactions) goes in
  `tests/ekp-buffer-tests.el`.

## Style

- `lexical-binding: t` everywhere; keep byte-compilation warning-free.
- `checkdoc` clean (CI enforces it): imperative docstring first lines,
  arguments mentioned in uppercase, two spaces after a sentence.
- `package-lint` clean: the `ekp-` / `ekp-buffer-` namespaces, proper
  autoload cookies on interactive entry points.
- Match the surrounding code; keep comments about *why*, not *what*.

## Commits

Conventional Commits (`feat:`, `fix:`, `perf:`, `refactor!:`,
`docs:`, `test:`, `chore:`).  Explain the reasoning in the body, not
just the change.

## Releases

Follow [Docs/RELEASING.md](Docs/RELEASING.md).  In particular, action
dependencies stay pinned to full commit SHAs, released artifacts are
immutable, and every artifact gets a SHA-256 checksum.

## License

By contributing you agree that your contributions are licensed under
GPL-3.0-or-later, matching the project (see `COPYING`).
