# Task 001: Fix Windows build failure

## Issue
User reports `make` fails on Windows because `cc` is not found.
Current Makefile relies on `uname` and assumes `cc` exists.

## Plan
1. Detect Windows via `OS` environment variable (standard on Windows).
2. On Windows, default CC to `gcc` if not set.
3. Remove reliance on `uname` for Windows detection.
4. Verify `pthread` linking.

## Status
- [x] Completed (2026-01-26)

## Validation
- Run `make` in `ekp_c/`.
- Verify `ekp.dll` is created.
- [x] Confirmed `make` builds `ekp.dll`.
- [x] Confirmed `make test` passes (loads module in Emacs).
