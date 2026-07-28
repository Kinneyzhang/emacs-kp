# Plan: Release and CI Governance 2026-07-28

## Goal

Make repository-local release evidence reproducible: CI inputs are immutable,
the supported Windows platform is exercised, `.phrase` decisions are tracked,
and one local gate checks the version and workflow invariants before a release.

## Scope

1. Add a failing static release-gate test for action pins, platform coverage,
   tracked decision records, package/changelog version agreement, and the C
   module version pair.
2. Pin third-party actions to full upstream commit SHAs with readable tag
   comments.
3. Add a Windows batch-test job using the existing public test runner.
4. Stop ignoring `.phrase` and document the release checklist, including
   remote/tag and artifact checks that cannot safely be automated locally.
5. Run the gate, YAML parse/static checks, default tests, and diff checks.

## Non-goals

- Do not push commits or tags, publish artifacts, or rewrite existing tags.
- Do not claim that an unrun remote GitHub Actions workflow is green.
- Do not add a release framework or package dependency.

## Risks and Mitigations

- **Pinned actions age:** keep the human-readable upstream tag beside each SHA
  and make updates an explicit reviewed change.
- **Windows shell differences:** call Emacs directly from PowerShell instead
  of depending on the POSIX test wrapper.
- **Local versus remote state:** the gate checks repository invariants; the
  release checklist separately requires a clean commit, signed/annotated tag,
  remote CI, checksums, and remote tag verification.

## Rollback

Revert the gate, workflow, `.gitignore`, and release documentation together.
Do not restore floating action refs independently.
