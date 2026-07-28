# Serialize a Logical Copy Instead of Restoring After Save

## Context

EKP previously unformatted the display buffer in `before-save-hook` and
reformatted it in `after-save-hook`. Emacs does not run `after-save-hook`
when writing fails or is interrupted, so the screen stayed unformatted and
the marker state stayed live.

## Decision

Do not mutate the source buffer during save. A buffer-local
`write-region-annotate-functions` entry switches whole-buffer writes to a
hidden logical copy. This is the same documented buffer-switch boundary used
by Emacs format encoders, so normal backup, coding, file-mode, VC, and later
annotation behavior remains owned by Emacs.

## Alternatives Rejected

### Add another restoration hook

There is no general failure hook paired with `after-save-hook`. A
post-command fallback would miss programmatic saves and nested calls.

### Advise `save-buffer` or `write-region` globally

Global advice would make every Emacs save share EKP's lifecycle and create
ordering conflicts with other packages.

### Reimplement saving in `write-contents-functions`

Owning the actual write would duplicate backup, coding, file modes, VC, and
visited-file semantics that Emacs already implements.

## Consequences

- Success and failure paths leave the visible layout byte-identical.
- Disk and auto-save output receive logical text.
- A successful write disposes the copy immediately.
- Because Emacs does not call the post-annotation function after a failed
  write, at most one hidden copy remains per source buffer; retry or teardown
  replaces and clears it.
- No global advice, external dependency, or saved-file format changed.

## Verification

The old model failed real missing-directory and forced-quit tests by leaving
the logical text visible. The new model passes success, filesystem failure,
encoding failure plus retry, and interruption tests. Full ERT passes
102/102, C/Elisp fuzz passes 300/300, and warnings-as-errors byte compilation
plus checkdoc are clean.

## Rollback

Restore the before/after hooks and their marker state. Existing files need
no migration because both designs write the same logical bytes.
