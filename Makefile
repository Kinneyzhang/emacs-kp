.DEFAULT_GOAL := check
.NOTPARALLEL:
EMACS ?= emacs
SOURCES := lisp/ekp-utils.el lisp/ekp-hyphen.el lisp/ekp.el lisp/ekp-buffer.el
.PHONY: compile test check clean native-build
compile:
	$(EMACS) -Q --batch -L lisp --eval '(setq load-prefer-newer t byte-compile-error-on-warn t)' -f batch-byte-compile $(SOURCES)
test:
	scripts/run-tests.sh "$(EMACS)"
check: structure-check compile acceptance
native-build:
	$(MAKE) -C native PROFILE=portable
clean:
	rm -f lisp/*.elc tests/*.elc examples/*.elc benchmarks/*.elc

.PHONY: structure-check setup-hooks
structure-check:
	python3 scripts/check-repository.py
setup-hooks:
	git config --local core.hooksPath .githooks

.PHONY: acceptance
acceptance:
	python3 scripts/run-acceptance.py --emacs "$(EMACS)"
