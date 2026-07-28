# Change Log: C API and Arithmetic Contract 2026-07-28

## task008

- **Modify** — `ekp_c/ekp.c`, `ekp_c/ekp_module.h`, `ekp_c/ekp_kp.c`
  - Preflighted all single/batch schema fields before extraction.
  - Added `ekp-c-invalid-input` and atomic penalty validation.
  - Widened line arithmetic and stored rest values to 64-bit.
  - Bumped the module to 1.6.

- **Modify** — `ekp.el`, `ekp-utils.el`
  - Required C 1.6.
  - Preserved nil fallback but propagated enabled-backend signals.

- **Add/Modify** — C boundary tests, loaders, and documentation
  - Added six direct module cases and a public dispatch error control.
  - Updated public/developer/C API contracts and audit records.

## Validation

- Direct C boundary red/green: 0/6 → 6/6.
- Public dispatcher signal red/green: 0/1 → 1/1.
- Release C build: C11, `-Wall -Wextra -Wpedantic`, zero warnings.
- Sanitizer C build: ASan/UBSan, zero compile warnings. Loading the ASan
  dylib into signed macOS Emacs was blocked by platform runtime policy, so
  runtime sanitizer evidence remains delegated to Linux CI.
- Full ERT: 116/116.
- C/Elisp fuzz: 300/300.

## Behavior and Risk

- Valid C calls retain the same arguments and result shape.
- Invalid direct calls now use a stable, specific error condition.
- Allocation/no-result remains recoverable via Elisp; module signals expose
  broken internal contracts instead of hiding them.
- Older 1.5 modules are rejected until rebuilt.
