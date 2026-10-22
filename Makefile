EMACS ?= emacs
EL := use-package-vc.el

.PHONY: compile lint clean

compile:
	$(EMACS) -Q --batch -L . -L dev --eval '(setq byte-compile-error-on-warn t)' -f batch-byte-compile $(EL)

lint: compile
	$(EMACS) -Q --batch -L . -L dev -l checkdoc --eval '(checkdoc-file "$(EL)")'

clean:
	rm -f *.elc dev/*.elc
