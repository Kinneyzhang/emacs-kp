# A Report Is Not a Gate Until Failure Propagates

## Context

The GUI matrix measured the correct display facts and printed PASS or FAIL,
but batch callers always received success. The visible table and the process
contract were separate, so release automation could ignore a real overflow.

## Decision

Each case returns its measurement plist plus name. One reporting boundary
formats all rows, aggregates `:pass`, and exits batch Emacs with status 1
after printing the table when any case fails. Interactive use still displays
the report without terminating Emacs.

Keep the matrix under `tests/` because it owns showcase fixtures and
developer-only display scenarios. User/developer documentation must load it
explicitly rather than imply that requiring the runtime package defines it.

## Alternatives Rejected

### Search the rendered table for `FAIL`

Text parsing duplicates the decision and can diverge from measurement data.

### Signal before printing

Automation gets a failure but loses the row evidence needed to diagnose it.

### Move the showcase matrix into the runtime package

That expands the installed surface and couples production code to a
developer fixture without improving the assertion contract.

## Consequences

- Batch success now means every GUI row passed.
- Failure retains a complete diagnostic table and exits exactly 1.
- The report formatter is testable without a graphical display.
- Real pixel correctness still requires a GUI matrix run.

## Verification

The missing-boundary control failed 0/1 before implementation. The repaired
failure/success controls pass 2/2, both full ERT orders pass 110/110, and the
live seven-case matrix passes 7/7. The final screenshot was verified as one
fullscreen target buffer with no split or stale client message.

## Rollback

Restore string results and report-only behavior. No persisted data needs
migration.
