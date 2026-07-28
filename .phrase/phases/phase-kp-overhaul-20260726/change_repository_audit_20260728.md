# Change Log: Repository System Audit 2026-07-28

## 2026-07-28

- **Add** — `Docs/REPOSITORY_AUDIT_20260728.md`
  - Task: `task001`.
  - Recorded the architecture baseline, verified strengths, four P1 defects,
    seven P2 risks, three P3 opportunities, extension routes, non-goals, and
    acceptance gates.
  - Separated evidence, inference, and unknowns so future fixes can start
    from reproducible facts.

- **Modify** — `readme.md`, `readme_zh.md`
  - Task: `task001`.
  - Corrected the bundled dictionary count, C module version, ERT command
    description, GUI verification loading instructions, and the cached
    parameter-change caveat.
  - Linked the dated repository audit.

- **Modify** — `DEVELOPER.md`, `DEVELOPER_ZH.md`
  - Task: `task001`.
  - Linked the audit and removed stale hard-coded test counts.

- **Modify** — `ekp_c/README.md`
  - Task: `task001`.
  - Corrected the documented malformed-argument behavior without changing
    the C API.

- **Add/Modify** — `.phrase` plan, task, issue, and index records
  - Task: `task001`.
  - Registered the audit and its unresolved follow-up work in the current
    phase.

## Behavior and Risk

- Runtime behavior is unchanged.
- The report records implementation directions, not approved designs.
- Windows, remote CI, package-lint, sanitizer runtime loading, and a clean
  GUI screenshot remain explicit validation gaps.
