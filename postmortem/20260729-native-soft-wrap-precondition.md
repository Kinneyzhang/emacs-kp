# Own the Native Soft-Wrap Precondition

## Context

The native progressive editor deliberately lets Emacs decide when the
active source line becomes another visual row. That model worked in a
full-width verification window but failed in the user's narrow
side-by-side window: the line horizontally scrolled and displayed a `$`
truncation indicator.

The source was clean and the KP planner was not involved. Emacs 30.2
defaults `truncate-partial-width-windows` to `50`; a partial-width window
below that many columns truncates even when `truncate-lines` is nil.
Without a native visual row, the live engine has no completed row to
observe or align.

## Decision

`ekp-auto-justify-mode` owns the display precondition its state machine
requires:

1. On activation, snapshot the values and buffer-local ownership of
   `truncate-lines` and `truncate-partial-width-windows`.
2. Make both variables buffer-local and nil before the first reflow.
3. On mode disable or major-mode teardown, restore prior local values or
   remove the temporary local bindings so global ownership resumes.
4. Treat activation as a transaction: if width discovery, initial reflow,
   hook installation, or integration setup signals, remove partial
   lifecycle state, restore the display variables, set the mode back to
   disabled, and let the original error propagate.

This changes only redisplay policy. It creates no source character,
overlay, replacing display break, or independent wrapping algorithm.

## Alternatives Rejected

### Treat truncation as a user configuration error

The failing value is an Emacs default in narrow split windows. A mode whose
algorithm requires native visual rows cannot leave that precondition
implicit and still promise natural editing.

### Compute hidden visual rows while Emacs truncates

That would make EKP simulate a display the user cannot see. It would also
leave the reported horizontal-scrolling experience unchanged.

### Set the variables without restoring them

That would fix typing by permanently changing unrelated buffer behavior.
Minor-mode ownership must end with the mode.

### Change the global defaults

The requirement belongs to one buffer while its auto mode is active.
Changing global behavior would affect unrelated buffers and packages.

## Consequences

- Full-width and narrow side-by-side windows use the same native
  progressive state transitions.
- Existing `visual-line-mode`/`word-wrap` behavior remains available;
  EKP only prevents truncation.
- Disabling auto mode restores the exact prior buffer-local ownership.
  When the original state was global, removing the temporary binding lets
  the current global value resume.
- Multiple windows showing the same buffer share the buffer-local
  soft-wrap policy, matching the existing buffer-wide projection model.

## Verification

A public minor-mode lifecycle ERT reproduces the failure red, then proves
both variables are nil and buffer-local while enabled and that their prior
local/global ownership is restored on disable. Separate regressions force
initial reflow failure and switch major modes, proving those teardown paths
restore the same ownership and never leave a half-enabled mode.

The retained graphical run
`/private/tmp/ekp-soft-wrap-final-pass-PIUigY` types mixed Latin/CJK text
one character at a time in a 44-column left split. The selected window
advances from one to two visual rows with `hscroll=0`, zero live replacing
breaks, zero overlays, exact source text, and no pending transaction.
Temporal review finds only the intended Emacs split, natural continuation,
and a stable final frame.

## Rollback

Revert the wrap-state snapshot, activation, and restoration together.
Leaving only the forced values or only the restoration would violate mode
ownership.
