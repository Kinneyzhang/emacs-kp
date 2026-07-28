#!/bin/sh
# Check repository-local invariants required before an emacs-kp release.

set -eu

ROOT=$(CDPATH= cd -- "$(dirname "$0")/.." && pwd)
CI="$ROOT/.github/workflows/ci.yml"
failed=0

fail()
{
  printf 'release-check: %s\n' "$*" >&2
  failed=1
}

action_refs=$(
  sed -n 's/^[[:space:]]*-[[:space:]]*uses:[[:space:]]*\([^[:space:]#]*\).*$/\1/p' "$CI"
)
test -n "$action_refs" || fail "CI has no external action references"
for ref in $action_refs; do
  case "$ref" in
    *@????????????????????????????????????????) ;;
    *) fail "action is not pinned to a full commit SHA: $ref" ;;
  esac
done

grep -q 'runs-on: windows-latest' "$CI" ||
  fail "CI does not exercise the supported Windows platform"
grep -q 'ert-run-tests-batch-and-exit' "$CI" ||
  fail "CI does not drive the public ERT batch entry point"

if git -C "$ROOT" check-ignore -q .phrase/docs/CHANGE.md; then
  fail ".phrase decision records are ignored"
fi

package_version=$(
  sed -n 's/^;; Version: \([0-9][0-9.]*\)$/\1/p' "$ROOT/ekp.el"
)
test -n "$package_version" || fail "ekp.el has no valid package version"
grep -Eq "^## \\[$package_version\\] - [0-9]{4}-[0-9]{2}-[0-9]{2}$" \
  "$ROOT/CHANGELOG.md" ||
  fail "CHANGELOG has no dated entry for package version $package_version"

required_c_version=$(
  sed -n 's/^(defconst ekp-c-module-required-version \"\([0-9][0-9.]*\)\"$/\1/p' \
    "$ROOT/ekp-utils.el"
)
c_major=$(
  sed -n 's/^#define EKP_VERSION_MAJOR \([0-9][0-9]*\)$/\1/p' \
    "$ROOT/ekp_c/ekp_module.h"
)
c_minor=$(
  sed -n 's/^#define EKP_VERSION_MINOR \([0-9][0-9]*\)$/\1/p' \
    "$ROOT/ekp_c/ekp_module.h"
)
test -n "$required_c_version" || fail "Elisp has no required C version"
test -n "$c_major" && test -n "$c_minor" ||
  fail "C module header has no major/minor version"
if test -n "$required_c_version" && test -n "$c_major" && test -n "$c_minor"; then
  test "$required_c_version" = "$c_major.$c_minor" ||
    fail "Elisp requires C $required_c_version but header declares $c_major.$c_minor"
fi

test "$failed" -eq 0 || exit 1
printf 'release-check: repository invariants pass\n'
