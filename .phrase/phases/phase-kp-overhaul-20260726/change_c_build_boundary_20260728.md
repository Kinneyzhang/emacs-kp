# Change Log: Shell-Free Profiled C Builds 2026-07-28

## Planning

- Added `task009` and locked its process, profile, whitespace-path, and
  build/test acceptance criteria before implementation.

## task009

- **Modify** — `ekp-utils.el`
  - Deleted the one-use generic callback wrapper and shell command.
  - Added direct argv `make-process`, explicit profile validation, correct
    working directory, and success/failure output lifecycle.

- **Modify** — `ekp_c/Makefile`, `.github/workflows/ci.yml`
  - Made portable flags the default.
  - Split native, debug, and sanitizer flags behind `PROFILE`.
  - Updated sanitizer CI to the same vocabulary.

- **Modify** — tests and public/developer/C/audit documentation
  - Added process-shape and unknown-profile controls.
  - Documented profile ownership and closed `issue005`.

## Validation

- Focused process/profile red/green: 0/2 → 2/2.
- portable/native/debug/sanitize: all compiled with zero warnings.
- Real `ekp-c-module-build 'portable`: exit 0 and loaded C 1.6.
- Portable copy under `/tmp/ekp build.*`: built successfully.
- Full ERT and fuzz evidence from `task008` remains valid; final phase QA
  reruns both after all slices.

## Behavior and Risk

- `portable` no longer emits host-specific instructions.
- `native` artifacts are intentionally machine-specific.
- Failed async builds keep and display their output buffer; successful
  builds reload and remove it.
- The old `DEBUG=1` spelling is deleted rather than retained as a shim.
