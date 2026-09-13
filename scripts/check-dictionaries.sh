#!/bin/sh
# Verify the checked-in dictionary inventory against its offline manifest.

set -eu

ROOT=$(CDPATH= cd -- "$(dirname "$0")/.." && pwd)
DIR="$ROOT/dictionaries"
MANIFEST="$DIR/MANIFEST.tsv"
EXPECTED=49

fail()
{
  printf 'dictionary-check: %s\n' "$*" >&2
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

test -f "$MANIFEST" || fail "missing dictionaries/MANIFEST.tsv"

check_tmp=$(mktemp -d "${TMPDIR:-/tmp}/ekp-dictionary-check.XXXXXX")
cleanup_check_tmp()
{
  find "$check_tmp" -depth -delete
}
trap cleanup_check_tmp EXIT HUP INT TERM

actual_list="$check_tmp/actual"
manifest_list="$check_tmp/manifest"
find "$DIR" -maxdepth 1 -type f -name 'hyph_*.dic' -exec basename {} \; |
  LC_ALL=C sort > "$actual_list"
awk -F '	' '!/^#/ && NF {print $1}' "$MANIFEST" |
  LC_ALL=C sort > "$manifest_list"

actual_count=$(wc -l < "$actual_list" | tr -d ' ')
manifest_count=$(wc -l < "$manifest_list" | tr -d ' ')
test "$actual_count" -eq "$EXPECTED" ||
  fail "expected $EXPECTED dictionaries, found $actual_count"
test "$manifest_count" -eq "$EXPECTED" ||
  fail "expected $EXPECTED manifest entries, found $manifest_count"
test "$(uniq -d "$manifest_list" | wc -l | tr -d ' ')" -eq 0 ||
  fail "manifest contains duplicate filenames"
cmp -s "$actual_list" "$manifest_list" ||
  fail "manifest inventory differs from checked-in dictionaries"

tab=$(printf '\t')
while IFS="$tab" read -r file source expected_hash license alternatives; do
  case "$file" in
    ""|\#*) continue ;;
  esac
  test -n "$source" || fail "$file has no source path"
  test -n "$license" || fail "$file has no license evidence"
  test "$(sha256 "$DIR/$file")" = "$expected_hash" ||
    fail "$file checksum differs from manifest"
  actual_alternatives=$(
    LC_ALL=C awk '
      {
        line = $0
        sub(/^[[:space:]]*/, "", line)
        if (line !~ /^[%#]/ && index(line, "/")) count++
      }
      END { print count + 0 }
    ' "$DIR/$file"
  )
  test "$actual_alternatives" -eq "$alternatives" ||
    fail "$file alternative count is $actual_alternatives, expected $alternatives"
  case "$license" in
    embedded)
      LC_ALL=C grep -Eiq \
        'licen[cs]e|copyright|public domain|permission|redistribut' \
        "$DIR/$file" ||
        fail "$file claims embedded license evidence but has no marker"
      ;;
    *)
      test -f "$DIR/$license" ||
        fail "$file license evidence does not exist: $license"
      ;;
  esac
done < "$MANIFEST"

printf 'dictionary-check: %d entries pass\n' "$EXPECTED"
