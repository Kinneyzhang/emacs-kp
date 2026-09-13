#!/usr/bin/env python3
"""Validate repository organization in a checkout or the exact Git index."""
from __future__ import annotations
import argparse
import json
import os
from pathlib import Path, PurePosixPath
import re
import subprocess
import sys
import tempfile
from urllib.parse import unquote

POLICY_VERSION = 2
DOC_NAMES = {'README.md', 'README.zh-CN.md', 'CHANGELOG.md', 'CHANGELOG.zh-CN.md',
             'AGENTS.md', 'docs/manual.md', 'docs/manual.zh-CN.md',
             'docs/architecture.md', 'docs/architecture.zh-CN.md'}
DIRS = {'tests', 'examples', 'benchmarks', 'scripts', 'docs', 'native',
        'dictionaries', 'design', '.github', '.gitea', '.githooks'}
SKIP = {'.git', '__pycache__', 'target', '.omx', '.worktrees', '.claude'}
SHARED = ('AGENTS.md', '.editorconfig', 'scripts/check-repository.py', 'scripts/run-acceptance.py', '.githooks/pre-commit',
          '.github/workflows/structure.yml')


def inventory(root):
    """Include new nonignored files, and exclude deleted or generated files."""
    try:
        top = subprocess.check_output(['git', '-C', str(root), 'rev-parse', '--show-toplevel'],
                                      stderr=subprocess.DEVNULL, text=True).strip()
        if Path(top).resolve() == root.resolve():
            raw = subprocess.check_output(['git', '-C', str(root), 'ls-files', '-z',
                                           '--cached', '--others', '--exclude-standard'])
            return sorted({s for s in raw.decode().split('\0') if s and (root / s).is_file()})
    except subprocess.CalledProcessError:
        pass
    return sorted(str(p.relative_to(root)) for p in root.rglob('*')
                  if p.is_file() and not (set(p.relative_to(root).parts) & SKIP)
                  and p.suffix not in {'.elc', '.pyc', '.o', '.so', '.dylib', '.dll'})


def check(root, files=None):
    files = inventory(root) if files is None else files
    errors = []
    workspace = (root / 'workspace.json').exists()
    allowed_dirs = DIRS | (set(__import__('json').loads((root / 'workspace.json').read_text())['repositories']) if workspace else set())
    def fail(path, message): errors.append(f'{path}: {message}')
    for required in ['README.md', 'AGENTS.md', 'Makefile', 'scripts/check-repository.py',
                     '.githooks/pre-commit', '.github/workflows/structure.yml']:
        if required not in files: fail(required, 'required maintained file is missing')
    spellings = {}
    for f in files:
        parts = PurePosixPath(f).parts
        for i in range(1, len(parts) + 1):
            spelling = '/'.join(parts[:i]); old = spellings.setdefault(spelling.casefold(), spelling)
            if old != spelling: fail(f, f'case collision with {old}')
        if len(parts) > 1 and parts[0] not in allowed_dirs:
            fail(f, 'unapproved top-level directory')
        if len(parts) == 1 and f not in DOC_NAMES and f not in {
                'Makefile', 'LICENSE', 'COPYING', 'NOTICE', '.gitignore',
                '.gitattributes', '.editorconfig', 'workspace.json',
                'release-dependencies.json'} and not f.endswith('.el'):
            fail(f, 'unapproved root file; use the designated source/tool/data directory')
        # Preserve upstream dictionary/vendor licensing and resource names.
        external = parts[0] == 'dictionaries' or f.startswith('native/vendor/')
        if f.endswith('.md') and f not in DOC_NAMES and not external:
            fail(f, 'use README, CHANGELOG, docs/manual or docs/architecture; merge or delete other documents')
        if f.startswith('docs/') and (len(parts) != 2 or f not in DOC_NAMES):
            fail(f, 'documentation must be flat and use the fixed document names')
        if re.search(r'(^|/)(?:\.DS_Store|[^/]+\.(?:elc|eln|pyc|o|so|dylib|dll))$', f) or 'target' in parts or '__pycache__' in parts:
            fail(f, 'generated artifact must not be tracked')
        if len(parts)==1 and f.endswith('.el') and any(x in f for x in ['benchmark','-tests','-demo','-showcase']):
            fail(f, 'development code belongs in tests, benchmarks or examples')
        if f.startswith('tests/') and not f.startswith('tests/fixtures/') and not f.endswith('.el'):
            fail(f, 'tests/ contains plugin ERT Lisp; tools belong in scripts/ and data in tests/fixtures/')
        if f.startswith('benchmarks/') and not f.endswith('.el'):
            fail(f, 'benchmarks/ contains Lisp workloads; runners belong in scripts/')
        if f.endswith(('.py', '.sh')) and not f.startswith(('scripts/', 'native/vendor/')):
            fail(f, 'Python/Shell tools belong in scripts/')
        if f.startswith('scripts/') and Path(f).name.endswith('-tests.el'):
            fail(f, 'plugin ERT tests belong in tests/')
        if f.startswith('scripts/') and Path(f).name.startswith('test_') and not f.startswith('scripts/tests/'):
            fail(f, 'tool tests belong in scripts/tests/')
        if external or not f.endswith('.md'): continue
        text = (root / f).read_text()
        # Validate inline and reference-style Markdown links outside fenced code.
        plain = re.sub(r'^(`{3,}|~{3,}).*?^\1\s*$', '', text, flags=re.M | re.S)
        links = re.findall(r'\]\(<?([^\s)>]+)>?(?:\s+"[^"]*")?\)', plain)
        links += re.findall(r'^\s*\[[^\]]+\]:\s*<?([^\s>]+)>?', plain, re.M)
        for link in links:
            if re.match(r'[a-zA-Z][\w+.-]*:', link) or link.startswith(('#', '/')): continue
            rel = unquote(link.split('#')[0])
            if not rel: continue
            target = Path(os.path.normpath(str(Path(f).parent / rel)))
            if '..' in target.parts:
                fail(f, f'cross-repository link must use the provider URL: {link}')
            elif target.as_posix() != '.' and target.as_posix() not in files and not any(name.startswith(target.as_posix().rstrip('/') + '/') for name in files):
                fail(f, f'broken local link (including exact case): {link}')
    makefile = root / 'Makefile'
    if makefile.exists():
        text = makefile.read_text()
        for name in ['compile', 'test', 'check', 'clean', 'structure-check', 'setup-hooks']:
            if not re.search(r'^'+name+r'\s*:', text, re.M): fail('Makefile', f'missing {name} target')
        if not re.search(r'^check\s*:[^\n]*\bstructure-check\b', text, re.M):
            fail('Makefile', 'check must depend on structure-check')
        # Literal source paths in the root Makefile must exist, including phase leftovers.
        for token in re.findall(r'(?<![\w/.-])(?:tests|scripts|examples|benchmarks)/[\w./*-]+\.(?:el|py|sh)', text):
            if '*' not in token and not (root/token).is_file():fail('Makefile', f'missing source: {token}')
        for directory in re.findall(r'(?:^|\s)-L ([\w./-]+)', text):
            if directory.startswith('../'):
                # Sibling dependencies can be absent in a standalone clone.
                continue
            if not (root/directory).is_dir():fail('Makefile', f'missing load directory: {directory}')
        for directory in re.findall(r'directory-files(?:-recursively)?\s+"([\w/-]+)"', text):
            if not (root/directory).is_dir():fail('Makefile', f'missing scanned directory: {directory}')
        for pattern in re.findall(r'\$\(wildcard ((?:examples|tests|scripts|benchmarks)/[^)]+)\)', text):
            if not list(root.glob(pattern)):fail('Makefile', f'empty source discovery: {pattern}')
    if not workspace:
        manifest_path = root / 'scripts/acceptance.json'
        if not manifest_path.is_file():
            fail('scripts/acceptance.json', 'missing public acceptance inventory')
        else:
            try:
                manifest = json.loads(manifest_path.read_text())
                cases = manifest['cases']
                names = [c['name'] for c in cases]
                if not cases or len(names) != len(set(names)):
                    fail('scripts/acceptance.json', 'cases must be nonempty and unique')
                for case in cases:
                    file = root / case['file']
                    if not case.get('purpose'):
                        fail('scripts/acceptance.json', 'each scenario needs an observable purpose')
                    if not file.is_file() or not re.search(
                            r'^\(ert-deftest ' + re.escape(case['name']) + r'(?=\s|\()',
                            file.read_text(), re.M):
                        fail('scripts/acceptance.json', f"missing named scenario: {case['name']}")
                if not re.search(r'^check\s*:[^\n]*\bacceptance\b', makefile.read_text(), re.M):
                    fail('Makefile', 'check must include public acceptance scenarios')
            except (ValueError, KeyError, TypeError, OSError) as error:
                fail('scripts/acceptance.json', f'invalid inventory: {error}')
    return sorted(set(errors))


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--root', type=Path, default=Path(__file__).resolve().parents[1])
    parser.add_argument('--staged', action='store_true')
    args = parser.parse_args()
    if args.staged:
        # Materialize the index, so unstaged repairs cannot hide a broken commit.
        with tempfile.TemporaryDirectory(prefix='repository-index-') as directory:
            subprocess.run(['git', '-C', str(args.root), 'checkout-index', '--all',
                            '--prefix', directory + os.sep], check=True)
            snapshot = Path(directory)
            files = sorted(str(p.relative_to(snapshot)) for p in snapshot.rglob('*') if p.is_file())
            errors = check(snapshot, files)
    else:
        errors = check(args.root)
    if errors:
        print('\n'.join(errors), file=sys.stderr)
        return 1
    print(f'{args.root.name}: repository structure OK (policy {POLICY_VERSION})')
    return 0


if __name__ == '__main__':
    sys.exit(main())
