# Spec: Documentation Improvement

## Summary
Restructure and enhance documentation for `emacs-kp`. Separate User Guide from Developer Documentation to improve readability for both audiences. Provide in-depth API reference for developers.

## Goals
1.  **Separation of Concerns**: `readme.md` for users, `DEVELOPER.md` for contributors.
2.  **Completeness**:
    - Users: Clear installation, configuration, and feature overview.
    - Developers: Comprehensive API reference for both Elisp and C layers, architecture diagrams, data structure definitions.
3.  **Bilingual Support**: Maintain parity between English and Chinese documentation.

## Non-Goals
- Changing the code or functionality of `emacs-kp`.
- Adding new tutorials (beyond basic usage).

## User Flows
- **User**: Lands on repo -> Reads `readme.md` -> Installs & Configures -> Uses package.
- **Contributor**: Lands on repo -> Sees "Developer Guide" link -> Reads `DEVELOPER.md` -> Understands internals -> Submits PR.

## Acceptance Criteria
1.  `DEVELOPER.md` and `DEVELOPER_ZH.md` exist and contain:
    - Architecture overview.
    - Elisp Core API (`ekp-pixel-justify`, parameters, etc.).
    - Data Structures (`ekp-para`, `ekp-box`, etc.).
    - C Module details (API, build, memory model).
2.  `readme.md` and `readme_zh.md` are cleaned up:
    - No C implementation details (moved to Dev guide).
    - Clearer "Quick Start" and "Configuration".
3.  No broken links between documents.
