EMACS ?= emacs
GRAMMAR_DIR ?=
BATCH = $(EMACS) -Q --batch -L . --eval '(when (getenv "GRAMMAR_DIR") (add-to-list (quote treesit-extra-load-path) (getenv "GRAMMAR_DIR")))'
export GRAMMAR_DIR

.PHONY: check compile checkdoc test clean
check: compile checkdoc test
compile:
	$(BATCH) --eval '(setq byte-compile-error-on-warn t)' -f batch-byte-compile djot-mode.el
checkdoc:
	$(BATCH) -l test/checkdoc.el

test:
	$(BATCH) -l test/djot-mode-test.el -f ert-run-tests-batch-and-exit
clean:
	rm -f djot-mode.elc
