# Change Log: Release and CI Governance 2026-07-28

## Planning

- Added `task010` and locked the local/remote boundary, immutable input,
  Windows, tracked-record, and version-pair acceptance criteria before code.

## task010

- **Add** — `tests/check-release.sh`, `Docs/RELEASING.md`
  - Added one executable repository-invariant gate and a distinct
    credentialed release checklist.

- **Modify** — `.github/workflows/ci.yml`
  - Pinned checkout and Emacs setup actions to verified full upstream SHAs.
  - Replaced live MELPA installation with a pinned package-lint checkout.
  - Added an Emacs 30.1 Windows Elisp compile/ERT baseline.

- **Modify** — `.gitignore`, contributor/public/audit documentation
  - Made `.phrase` records versionable.
  - Documented immutable release artifacts, checksums, and remote checks.
  - Closed `issue009` without claiming an unperformed remote publish.

- **Add** — `postmortem/20260728-release-governance.md`
  - Recorded why local invariants and credentialed remote release evidence
    remain separate boundaries.

## Validation

- Release gate red: floating refs, missing Windows/public ERT path, and
  ignored `.phrase` were all reported.
- Release gate green: repository invariants pass.
- Pinned package-lint commit: pass with an empty package directory.
- Workflow YAML parse and POSIX shell syntax: pass.
- Default ERT: 119/119.
- `git diff --check`: pass.

## Behavior and Risk

- Full-SHA action pins intentionally require explicit maintenance updates.
- Windows CI covers the required pure-Elisp product baseline; the optional C
  module remains exercised on Linux and macOS.
- No commit, tag, remote workflow, artifact, push, or publication was made.
