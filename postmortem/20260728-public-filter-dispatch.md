# Use the Public Substring Filter Dispatcher

## Context

EKP correctly preserved a prior `filter-buffer-substring-function`, but
invoked it through Emacs's private `buffer-substring--filter` helper. That
made the integration depend on an implementation detail outside EKP's
ownership and version baseline.

## Decision

During EKP extraction, dynamically bind `filter-buffer-substring-function`
to the recorded prior value and call public `filter-buffer-substring`.
Inhibit EKP lifecycle cleanup while that temporary binding is active. For a
DELETE operation, unwind the binding first and then reconcile integration
ownership against the buffer's actual filter slot.

## Failed First Repair

The first public-dispatch repair allowed after-change cleanup to run under
the temporary prior-filter binding. The focused kill/DELETE regression
failed because cleanup cleared EKP ownership state without restoring the
real EKP-bound slot. That failure narrowed the problem to lifecycle ordering;
no second speculative behavior patch was stacked on it.

## Alternatives Rejected

### Keep Calling the Private Helper

It works on the current Emacs build but violates the dependency boundary and
makes the documented Emacs baseline depend on unowned internals.

### Reimplement Prior-Filter Dispatch

Duplicating Emacs's filtering and deletion semantics would create a second
protocol implementation and a larger compatibility surface.

### Run Cleanup Before Unwinding

The dynamic prior-filter binding is not the buffer's durable ownership state,
so lifecycle decisions made there are necessarily wrong-layer decisions.

## Consequences

- Prior local and inherited filter behavior still composes through the
  public dispatcher.
- DELETE cleanup observes the real post-operation ownership slot.
- Internal filter calls can no longer drift independently from Emacs's public
  dispatcher contract.
- Errors from a prior filter still propagate; no fallback or swallowing path
  was added.

## Verification

The first repair failed the public kill/DELETE lifecycle regression. The
ordering repair passes all five composition/lifecycle cases. The final
private-boundary scan contains no `buffer-substring--filter` use in runtime
or tests.

## Rollback

Restore direct private-helper dispatch. No persisted data needs migration,
but doing so reintroduces the unowned dependency boundary.
