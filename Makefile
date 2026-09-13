.DEFAULT_GOAL := check
.NOTPARALLEL:
EMACS ?= emacs
SOURCES := lisp/ekp-utils.el lisp/ekp-hyphen.el ekp.el lisp/ekp-buffer.el lisp/ekp-layout.el
.PHONY: compile test check clean native-build
compile:
	$(EMACS) -Q --batch -L . -L lisp --eval '(setq load-prefer-newer t byte-compile-error-on-warn t)' -f batch-byte-compile $(SOURCES)
test:
	scripts/run-tests.sh "$(EMACS)"
check: structure-check compile acceptance api-check
native-build:
	$(MAKE) -C native PROFILE=portable
clean:
	rm -f *.elc lisp/*.elc tests/*.elc examples/*.elc benchmarks/*.elc

.PHONY: structure-check setup-hooks
structure-check:
	python3 scripts/check-repository.py
setup-hooks:
	git config --local core.hooksPath .githooks

.PHONY: acceptance
acceptance:
	python3 scripts/run-acceptance.py --emacs "$(EMACS)"

.PHONY: api-check
api-check:
	$(EMACS) -Q --batch -L . $(API_PROVIDER_PATH) --eval '(setq load-prefer-newer t)' -l scripts/check-api.el
API_PROVIDER_PATH = -L ../ecss -L ../tp -L ../ebox -L ../etaf -L ../etaf-ui -L ../etaf-db -L ../ebox-playground -L ../etaf-playground
