.PHONY: test

EMACS ?= emacs

test:
	EMACS=$(EMACS) cask exec ert-runner
