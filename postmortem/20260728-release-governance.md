# Release Governance Has Two Boundaries

## Context

The repository described `.phrase` as its decision source of truth while
ignoring that directory.  CI used mutable action refs and did not exercise
Windows, although the project documents Windows module support.  Package,
C ABI, changelog, local tag, remote tag, CI, and artifacts were checked by
separate informal steps.

## Decision

Repository-local invariants belong to an executable static gate:
version pairs, dated changelog state, immutable CI action refs, platform
coverage, and tracked decision records.  Remote and artifact facts belong to
an explicit release checklist because they require credentials and the exact
reviewed release commit.

The gate does not infer that a local tag was published or that remote CI ran.
Those claims must be verified against the remote during an actual release.

## Alternatives

- A large release framework was rejected: it would add a dependency and more
  policy than this small package needs.
- Treating `.phrase` as disposable local state was rejected because the
  repository workflow already makes task, issue, decision, and change records
  authoritative.
- Floating major action tags were rejected because their executed bytes can
  change without a repository diff.

## Consequences

CI updates now require an intentional SHA change with the readable upstream
tag retained in a comment.  Windows gets an Elisp baseline even when the
optional C toolchain is unavailable.  Actual publishing remains a distinct,
credentialed operation and is never implied by the local gate.

## Rollback

Revert the gate, workflow, release guide, and `.gitignore` decision together.
Restoring only floating refs or ignored decision records would recreate the
same split-brain process.
