# Releasing emacs-kp

Releases are immutable snapshots.  Changed bytes require a new version,
new checksums, and a new tag; do not replace an existing release asset.

## Repository gate

Run the local invariant check before creating a release commit:

```sh
tests/check-release.sh
```

It verifies:

- every GitHub Action is pinned to a full commit SHA;
- Windows and the public ERT batch entry point are present in CI;
- `.phrase` decision records are not ignored;
- the package header has a matching dated changelog release;
- the Elisp C-module requirement matches the C header ABI version.

This static gate does not prove a remote workflow or published artifact.

## Release checklist

1. Move all shipped entries out of `Unreleased` into a dated
   `[X.Y.Z] - YYYY-MM-DD` section and set `Version: X.Y.Z` in `ekp.el`.
2. If the C API changed, bump both `ekp-c-module-required-version` in
   `ekp-utils.el` and `EKP_VERSION_MAJOR`/`EKP_VERSION_MINOR` in
   `ekp_c/ekp_module.h`.  Update the C version in user and developer docs.
3. Run:

   ```sh
   tests/check-release.sh
   tests/run-tests.sh emacs
   EKP_TEST_SEED=20260728 tests/run-tests.sh emacs --random-order
   tests/run-tests-isolated.sh emacs
   tests/check-dictionaries.sh
   dictionaries/update.sh check
   make -C ekp_c PROFILE=portable
   emacs -Q --batch -L . -l tests/ekp-fuzz.el
   ```

4. Byte-compile with warnings as errors and run package-lint/checkdoc using
   the same pinned inputs as CI.
5. Verify the worktree is clean and the release commit is the reviewed commit.
6. Create `vX.Y.Z` at that exact commit.  Never move an existing release tag.
7. Let the pinned Linux, macOS, Windows, and sanitizer CI jobs finish
   successfully on the release commit.
8. Build release artifacts from the tagged commit with `PROFILE=portable`.
   Record a SHA-256 checksum for every artifact.
9. Push the commit and tag, publish the immutable assets and checksums, then
   verify the remote tag resolves to the reviewed release commit.

If any item fails, fix it in a new commit and restart from the repository
gate.  Do not publish a partial release.
