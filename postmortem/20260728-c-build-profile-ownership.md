# Build Intent Must Be a Named Profile, Not an Ambient Flag

## Context

The default Makefile mixed release optimization with
`-march=native -flto`, so an ordinary artifact could depend on the build
host. The interactive Emacs command interpolated the checkout path into
`cd PATH && make`, handing quoting and metacharacter semantics to a shell.
Its one-use callback wrapper also inferred success from sentinel text rather
than the exit status.

## Decision

Name build intent explicitly: `portable` is the default, while `native`,
`debug`, and `sanitize` opt into their distinct flags. CI and documentation
use the same vocabulary.

The interactive command validates the profile, binds `default-directory`,
and calls `make-process` with `("/path/to/make" "PROFILE=name")`. Its async
boundary checks process status and exit code; successful builds reload and
clean the output buffer, while failures retain and display diagnostics.

## Alternatives Rejected

### Quote the shell string

Correct quoting differs by shell and Windows command processor, and no
shell feature is needed for one executable plus one argument.

### Keep native optimization as the default

That makes release portability depend on undocumented build-host CPU
features and toolchain LTO compatibility.

### Preserve `DEBUG=1` as an alias

Two vocabularies for one workflow create drift. The repository is not
maintaining a released build-flag API.

## Consequences

- Ordinary artifacts are portable by construction.
- Benchmark builds remain explicitly machine-specific.
- Whitespace/metacharacters in checkout paths are data, not syntax.
- Async failures preserve the evidence users need.

## Verification

The old command failed both process/profile controls. The new command passes
2/2, all four profiles compile warning-free, the real interactive portable
build exits 0 and loads C 1.6, and a copied source path containing spaces
builds successfully.

## Rollback

Restore the shell command and mixed default flags. No saved data needs
migration, but existing native artifacts should not be relabeled portable.
