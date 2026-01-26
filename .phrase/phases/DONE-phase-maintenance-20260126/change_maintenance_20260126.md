# Changes Log - Phase Maintenance 2026-01-26

## 2026-01-26
- **Fix**: Update `ekp_c/Makefile` to support Windows build.
  - Detect `Windows_NT` and use `gcc` instead of `cc`.
  - Set default `EMACS_ROOT` and `EMACS` path for the current environment.
  - Add `EMACS_ROOT` include path to `CFLAGS`.
  - Fix `test` target to use configured `$(EMACS)` executable.
  - Task: `task001`
