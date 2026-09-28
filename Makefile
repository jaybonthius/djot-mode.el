EMACS ?= emacs
GRAMMAR_DIR ?=
BATCH = $(EMACS) -Q --batch -L . --eval '(when (getenv "GRAMMAR_DIR") (add-to-list (quote treesit-extra-load-path) (getenv "GRAMMAR_DIR")))'
export GRAMMAR_DIR

.PHONY: check compile checkdoc test oracle edit-oracle benchmark clean
check: compile checkdoc test
compile:
	$(BATCH) --eval '(setq byte-compile-error-on-warn t)' -f batch-byte-compile djot-mode.el
checkdoc:
	$(BATCH) -l test/checkdoc.el

test:
	$(BATCH) -L test -l test/djot-edit-test.el -f ert-run-tests-batch-and-exit
oracle:
	$(BATCH) -l test/oracle.el
edit-oracle:
	$(BATCH) -l test/edit-oracle.el
benchmark:
	$(BATCH) -l test/benchmark.el
clean:
	rm -f djot-mode.elc
