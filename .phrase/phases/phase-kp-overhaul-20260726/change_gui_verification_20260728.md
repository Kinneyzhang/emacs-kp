# Change Log: Fail-Closed GUI Verification 2026-07-28

## task007

- **Modify** — `tests/ekp-gui-verify.el`
  - Returned structured per-case results.
  - Centralized table formatting and pass/fail aggregation.
  - Exited with status 1 after reporting any batch failure.

- **Add/Modify** — GUI ERT tests and test loaders
  - Added forced-failure and success controls.
  - Included the batch-safe GUI boundary tests in normal, permuted, and
    isolated runners.

- **Modify** — public, developer, audit, phase, and postmortem records
  - Clarified the tests-only loading boundary and nonzero contract.

## Validation

- Focused red: forced-failure control 0/1.
- Focused green: failure/success controls 2/2.
- Default full ERT: 110/110.
- Non-default permuted ERT: 110/110.
- Live GUI matrix: 7/7 PASS, every row `over=0`.
- Screenshot `/tmp/emacs-kp-gui-20260728-clean.png` was inspected: one
  fullscreen `*ekp-gui-verify*` window, no split, no scratch or client noise.

## Behavior and Risk

- Interactive matrix behavior remains a visible report buffer.
- Batch callers now receive a trustworthy status; this is intentional.
- The matrix remains a developer tool and adds no runtime dependency.
