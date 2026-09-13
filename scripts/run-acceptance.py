#!/usr/bin/env python3
"""Run named public scenarios; reject missing or empty acceptance inventories."""
import argparse
import json
from pathlib import Path
import subprocess
import sys


def command(root, emacs):
    manifest = json.loads((root / 'tests/acceptance.json').read_text())
    cases = manifest.get('cases', [])
    if not cases or len({c['name'] for c in cases}) != len(cases):
        raise ValueError('Acceptance cases must be nonempty and uniquely named')
    files = list(dict.fromkeys(manifest.get('support', []) + [c['file'] for c in cases]))
    for file in files:
        if not (root / file).is_file():raise ValueError(f'Missing acceptance source: {file}')
    names = []
    for case in cases:
        name = case['name']
        if not __import__('re').fullmatch(r'[a-zA-Z][a-zA-Z0-9-]+',name) or not case.get('purpose'):
            raise ValueError('Each case needs an ERT name and observable acceptance purpose')
        names.append(name)
    args = [emacs, '-Q', '--batch']
    for path in manifest.get('load_path', ['.']):args += ['-L', str(root / path)]
    args += ['--eval', '(setq load-prefer-newer t native-comp-jit-compilation nil)']
    for file in files:args += ['-l', str(root / file)]
    # ert-get-test fails if any named scenario disappears; no regex can pass with zero cases.
    symbols = ' '.join(names)
    args += ['--eval', f"(progn (mapc #'ert-get-test '({symbols})) (ert-run-tests-batch-and-exit '(member {symbols})))"]
    return args


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--emacs', default='emacs')
    parser.add_argument('--root', type=Path, default=Path(__file__).resolve().parents[1])
    args=parser.parse_args()
    return subprocess.call(command(args.root.resolve(),args.emacs),cwd=args.root)


if __name__ == '__main__':sys.exit(main())
