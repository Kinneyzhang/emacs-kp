#!/usr/bin/env python3
"""Check root Commentary API declarations and cross-package Lisp references."""
import argparse
import json
from pathlib import Path
import re
import sys
import subprocess

API = re.compile(r'^;; (Function|Macro|Variable|Hook|Error|Component): ([^\s()]+)(.*)$', re.M)
DEFINITION = re.compile(r"(?<!['`])\((?:defun|cl-defun|defmacro|cl-defmacro|defsubst|defvar|defconst|defcustom|defvar-local|define-minor-mode|define-derived-mode|etaf-ui--define-component|etaf-define-component)\s+([^\s()]+)")


def tokens(source):
    """Keep Lisp symbols/references, excluding comments, strings and characters."""
    i = 0
    while i < len(source):
        c = source[i]
        if c == ';':
            end = source.find('\n', i)
            i = len(source) if end < 0 else end + 1
        elif c == '"':
            i += 1
            while i < len(source):
                if source[i] == '\\': i += 2
                elif source[i] == '"':
                    i += 1
                    break
                else: i += 1
        elif c == '?' and (i == 0 or source[i-1] in ' \t\n(['):
            i += 2
            if i <= len(source) and source[i-1] == '\\': i += 1
        elif c in " \t\r\n()[]'`,": i += 1
        else:
            start = i
            while i < len(source) and source[i] not in " \t\r\n()[]'`,;\"": i += 1
            if i == start: i += 1
            else: yield source[start:i]


def declarations(entry):
    source = entry.read_text()
    if ';;; Commentary:' not in source or ';;; Code:' not in source:
        return {}, [f'{entry}: missing Commentary/Code sections']
    commentary = source.split(';;; Commentary:', 1)[1].split(';;; Code:', 1)[0]
    records = {}; errors = []
    matches = list(API.finditer(commentary))
    for index, match in enumerate(matches):
        kind, name, signature = match.groups()
        detail = commentary[match.end():matches[index+1].start() if index+1<len(matches) else len(commentary)]
        if name in records: errors.append(f'{entry}: duplicate API declaration: {name}')
        if '--' in name: errors.append(f'{entry}: private symbol declared as API: {name}')
        if kind in ('Function', 'Macro', 'Component') and not signature.strip().startswith('('):
            errors.append(f'{entry}: {name} needs a calling signature')
        if not re.search(r'^;;\s+\S', detail, re.M): errors.append(f'{entry}: {name} needs usage/behavior documentation')
        records[name] = kind.lower()
    if not records: errors.append(f'{entry}: public API declarations are empty')
    return records, errors


def struct_symbols(source):
    """Recognize cl-defstruct accessors, constructors, predicates and copiers.

    Ignore comments/strings before balancing forms. This intentionally recognizes
    literal definitions; dynamically generated symbols remain a review concern.
    """
    masked = re.sub(r'"(?:\\.|[^"\\])*"|;[^\n]*', lambda m: ' ' * len(m[0]), source)
    result = set()
    for start in re.finditer(r'\(cl-defstruct\s+', masked):
        depth = 0
        end = start.start()
        for end in range(start.start(), len(masked)):
            if masked[end] == '(': depth += 1
            elif masked[end] == ')':
                depth -= 1
                if depth == 0: break
        parts = re.findall(r"[()]|[^\s()'`]+", masked[start.start():end+1])
        stack = [[]]
        for token in parts:
            if token == '(':
                child = []; stack[-1].append(child); stack.append(child)
            elif token == ')':
                if len(stack) > 1: stack.pop()
            else: stack[-1].append(token)
        if not stack[0] or len(stack[0][0]) < 2: continue
        form = stack[0][0]
        header = form[1]
        name = header[0] if isinstance(header, list) else header
        if not isinstance(name, str) or name.startswith(','): continue
        options = header[1:] if isinstance(header, list) else []
        prefix = name + '-'
        constructors = []
        predicate = name + '-p'; copier = 'copy-' + name
        for option in options:
            if not isinstance(option, list) or len(option)<2: continue
            key, value = option[:2]
            if key == ':conc-name': prefix = '' if value == 'nil' else value
            elif key == ':constructor': constructors.append(value)
            elif key == ':predicate': predicate = value
            elif key == ':copier': copier = value
        result.update(x for x in [predicate, copier] + (constructors or ['make-'+name]) if x != 'nil')
        for slot in form[2:]:
            slot = slot[0] if isinstance(slot, list) and slot else slot
            if isinstance(slot, str) and slot and not slot.startswith(':'):
                result.add(prefix + slot)
    return result


def package(root):
    entries = list(root.glob('*.el'))
    if len(entries) != 1: raise ValueError(f'{root}: expected one root entry')
    entry = entries[0]
    files = [entry] + sorted((root/'lisp').glob('*.el'))
    diagnostic_files = [p for d in ('scripts', 'benchmarks', 'examples') for p in (root/d).rglob('*.el')]
    defined = set(); features = set()
    for p in files + diagnostic_files:
        text = p.read_text()
        text = re.sub(r'"(?:\\.|[^"\\])*"|;[^\n]*', lambda m: ' ' * len(m[0]), text)
        defined.update(sym for sym in DEFINITION.findall(text) if not sym.startswith(','))
        defined.difference_update(re.findall(r"\(defvar\s+([^\s()]+)\s*\)", text))
        defined.update(re.findall(r"\((?:define-error|defalias|defvaralias)\s+'([^\s()]+)", text))
        defined.update(struct_symbols(text))
        features.update(re.findall(r"\(provide\s+'([^\s()]+)", text))
    api, errors = declarations(entry)
    return {'root':root, 'entry':entry, 'files':files, 'defined':defined, 'features':features, 'api':api, 'errors':errors}


def check(packages, selected=None):
    errors = []
    owners = {}
    feature_owners = {}
    for name, pkg in packages.items():
        # Explicit exports cover generated accessors and macro-created components too.
        for sym in pkg['defined'] | set(pkg['api']):
            if sym in owners and owners[sym] != name:
                errors.append(f'{pkg["entry"]}: symbol {sym} is also owned by {owners[sym]}')
            owners[sym] = name
        for feature in pkg['features']: feature_owners[feature] = name
    for name, pkg in packages.items():
        if selected and name not in selected: continue
        errors.extend(pkg['errors'])
        tests = sorted(p for p in (pkg['root']/'tests').rglob('*.el') if 'fixtures' not in p.relative_to(pkg['root']).parts)
        files = pkg['files'] + sorted((pkg['root']/'examples').rglob('*.el')) + tests
        for file in files:
            source = file.read_text()
            for symbol in set(tokens(source)):
                provider = owners.get(symbol)
                if provider and (provider != name or file in tests) and symbol not in packages[provider]['api']:
                    errors.append(f'{file}: {symbol} is internal to {provider}; use its root entry API')
            for literal in re.findall(r'\(intern(?:-soft)?\s+"([^"\\]+)"', source):
                provider = owners.get(literal)
                if provider and (provider != name or file in tests) and literal not in packages[provider]['api']:
                    errors.append(f'{file}: reflective reference to internal {provider} symbol {literal}')
            for library in re.findall(r'\((?:load|load-file)\s+"([^"\\]+)"', source):
                for provider, contract in packages.items():
                    if provider == name and file not in tests: continue
                    internal = contract['features'] - {contract['entry'].stem}
                    if Path(library).stem in internal:
                        errors.append(f'{file}: direct load of internal {provider} library {library}')
            for feature in re.findall(r"\(require\s+'([^\s()]+)", source):
                provider = feature_owners.get(feature)
                if provider and (provider != name or file in tests) and feature != packages[provider]['entry'].stem:
                    errors.append(f'{file}: require {packages[provider]["entry"].stem}, not internal feature {feature}')
    return sorted(set(errors))


def clone_provider(url, path):
    if url.startswith('git@'):
        host, location = url[4:].split(':', 1)
        url = 'https://' + host + '/' + location
    subprocess.run(['git','clone','--depth=1','--single-branch',url,str(path)],check=True)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--workspace', type=Path)
    parser.add_argument('--root', type=Path, default=Path(__file__).resolve().parents[1])
    parser.add_argument('--providers-dir', type=Path)
    parser.add_argument('--fetch-providers', action='store_true')
    args = parser.parse_args()
    root = args.root.resolve()
    registry = json.loads((Path(__file__).parent/'api-workspace.json').read_text())
    if args.workspace:
        workspace = args.workspace.resolve()
        if args.fetch_providers:
            for name, entry in registry.items():
                if not (workspace/name).exists(): clone_provider(entry['url'], workspace/name)
        packages = {name:package(workspace/name) for name in registry}
        selected = None
    else:
        own = package(root)
        name = next((n for n,e in registry.items() if e['entry']==own['entry'].stem), root.name)
        provider_dir = (args.providers_dir or root.parent).resolve()
        packages = {name: own}
        pending = [name]
        while pending:
            current = pending.pop()
            contract = packages[current]
            references = set()
            for file in contract['files'] + [p for d in ('examples', 'tests') for p in (contract['root']/d).rglob('*.el') if 'fixtures' not in p.relative_to(contract['root']).parts]:
                references.update(tokens(file.read_text()))
            dependencies = set()
            for symbol in references:
                provider = next((n for n,e in sorted(registry.items(), key=lambda pair:-len(pair[1]['entry']))
                                 if symbol==e['entry'] or symbol.startswith(e['entry']+'-')), None)
                if provider and provider not in packages: dependencies.add(provider)
            for dependency in sorted(dependencies):
                path = provider_dir/dependency
                if args.fetch_providers and not path.exists():
                    clone_provider(registry[dependency]['url'], path)
                if not path.is_dir():
                    parser.error(f'missing provider {dependency}; clone it beside this repository or use --fetch-providers --providers-dir PATH')
                packages[dependency] = package(path)
                pending.append(dependency)
        selected = {name}
    errors = check(packages, selected)
    if errors:
        print('\n'.join(errors), file=sys.stderr)
        return 1
    print('Public API declarations and consumer boundaries OK')
    return 0


if __name__ == '__main__': sys.exit(main())
