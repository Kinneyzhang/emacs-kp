#!/bin/sh
# Verify or export the pinned emacs-kp dictionary bundle.

set -eu

UPSTREAM_URL=https://github.com/LibreOffice/dictionaries.git
UPSTREAM_COMMIT=8fb8e794237cff49ec212023f96bcdb7d3fbf56c
DIR=$(CDPATH= cd -- "$(dirname "$0")" && pwd)
MANIFEST="$DIR/MANIFEST.tsv"
MODE=${1:-check}
OUTPUT=${2:-}

fail()
{
  printf 'dictionary-update: %s\n' "$*" >&2
  exit 1
}

sha256()
{
  if command -v sha256sum >/dev/null 2>&1; then
    sha256sum "$1" | awk '{print $1}'
  else
    shasum -a 256 "$1" | awk '{print $1}'
  fi
}

case "$MODE" in
  check)
    test -z "$OUTPUT" ||
      fail "check mode does not accept an output directory"
    ;;
  export)
    test -n "$OUTPUT" ||
      fail "usage: update.sh export OUTPUT_DIRECTORY"
    if test -e "$OUTPUT"; then
      test -d "$OUTPUT" || fail "output exists and is not a directory"
      test -z "$(find "$OUTPUT" -mindepth 1 -print -quit)" ||
        fail "output directory must be empty"
    else
      mkdir -p "$OUTPUT"
    fi
    ;;
  *)
    fail "usage: update.sh [check | export OUTPUT_DIRECTORY]"
    ;;
esac

command -v git >/dev/null 2>&1 || fail "git is required"
test -f "$MANIFEST" || fail "MANIFEST.tsv is missing"

work=$(mktemp -d "${TMPDIR:-/tmp}/ekp-dictionary-update.XXXXXX")
cleanup_work()
{
  find "$work" -depth -delete
}
trap cleanup_work EXIT HUP INT TERM

git -C "$work" init -q
git -C "$work" remote add origin "$UPSTREAM_URL"
git -C "$work" fetch -q --depth=1 origin "$UPSTREAM_COMMIT"
git -C "$work" rev-parse --verify FETCH_HEAD >/dev/null
test "$(git -C "$work" rev-parse FETCH_HEAD)" = "$UPSTREAM_COMMIT" ||
  fail "fetched commit differs from the pin"

for evidence in \
  eo/license-en.txt \
  id/LICENSE-dict \
  mr_IN/COPYING \
  nl_NL/license_en_EN.txt \
  ru_RU/README_ru_RU.txt
do
  git -C "$work" cat-file -e "FETCH_HEAD:$evidence" ||
    fail "pinned license evidence is missing: $evidence"
done

if test "$MODE" = export; then
  cp "$MANIFEST" "$DIR/LICENSES.md" "$OUTPUT/"
  for readme in "$DIR"/README_*.txt; do
    cp "$readme" "$OUTPUT/"
  done
fi

tab=$(printf '\t')
count=0
while IFS="$tab" read -r file source expected_hash _license _alternatives; do
  case "$file" in
    ""|\#*) continue ;;
  esac
  count=$((count + 1))
  normalized="$work/$file"
  if test "$source" = legacy; then
    cp "$DIR/$file" "$normalized"
  else
    git -C "$work" show "FETCH_HEAD:$source" > "$work/raw"
    LC_ALL=C tr -d '\r' < "$work/raw" > "$normalized"
  fi
  test "$(sha256 "$normalized")" = "$expected_hash" ||
    fail "$file differs from the pinned manifest"
  if test "$MODE" = check; then
    cmp -s "$DIR/$file" "$normalized" ||
      fail "$file differs from normalized upstream bytes"
  else
    cp "$normalized" "$OUTPUT/$file"
  fi
done < "$MANIFEST"

test "$count" -eq 49 || fail "manifest must contain 49 dictionaries"
printf 'dictionary-update: %s passed for %d pinned entries\n' "$MODE" "$count"
