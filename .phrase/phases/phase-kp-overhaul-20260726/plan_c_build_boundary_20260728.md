# Plan: Shell-Free Profiled C Builds

## Scope

Finish `issue005` P2-03: invoke make as argv in the module directory and
separate portable, native, debug, and sanitizer build intent.

## Resolution Path

1. Lock the interactive command shape with a whitespace-path process test
   and an invalid-profile control.
2. Remove the one-use process wrapper and shell interpolation; use
   `make-process` with a real argv and bound `default-directory`.
3. Make `portable` the Makefile default; isolate `native`, `debug`, and
   `sanitize` flags behind explicit `PROFILE`.
4. Update CI and all build documentation to the same vocabulary.
5. Build every profile warning-clean, load/test the portable result, and
   compile a copied source tree whose path contains spaces.

## Non-goals

- No dependency on CMake or another build system.
- No automatic compiler/package installation.
- No publishing or replacing a released artifact.

## Rollback

Restore the shell command and monolithic flags, then remove profile tests and
documentation. No persisted data changes.
