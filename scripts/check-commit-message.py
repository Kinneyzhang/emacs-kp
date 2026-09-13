#!/usr/bin/env python3
"""Check the project's Conventional Commits message contract."""
import re
import sys
from pathlib import Path

HEADER = re.compile(r'(feat|fix|docs|refactor|perf|test|build|ci|chore|revert)(\([a-z0-9][a-z0-9-]*\))?(!)?: (\S.*)')


def check(message):
    lines = [line.rstrip() for line in message.splitlines() if not line.startswith('#')]
    while lines and not lines[-1]:
        lines.pop()
    if not lines:
        return ['commit message is empty']
    errors = []
    match = HEADER.fullmatch(lines[0])
    if not match:
        errors.append('title must be type(scope optional): concrete summary')
    elif match.group(4).lower().rstrip('.') in {'update', 'update files', 'fix', 'fix bug', 'changes', 'wip'}:
        errors.append('describe the actual behavior or change in the title')
    if len(lines[0]) > 72:
        errors.append('title must not exceed 72 characters')
    if len(lines) < 3 or lines[1] != '':
        errors.append('separate the title and body with a blank line')
    body = '\n'.join(lines[2:])
    if not re.search(r'^Validation: \S.*$', body, re.M):
        errors.append('include Validation: with actual checks/results or an explicit reason checks were not run')
    if not any(line and not line.startswith(('Validation:', 'BREAKING CHANGE:')) for line in lines[2:]):
        errors.append('body must explain the problem, resulting behavior or reason')
    breaking = bool(re.search(r'^BREAKING CHANGE: \S', body, re.M))
    if match and bool(match.group(3)) != breaking:
        errors.append('breaking changes require both ! in the title and a BREAKING CHANGE: explanation')
    return errors


def main():
    if len(sys.argv) != 2:
        print('usage: check-commit-message.py MESSAGE_FILE', file=sys.stderr)
        return 2
    errors = check(Path(sys.argv[1]).read_text())
    if errors:
        print('\n'.join(errors), file=sys.stderr)
        return 1
    return 0


if __name__ == '__main__':
    sys.exit(main())
