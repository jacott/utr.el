.PHONY: test

all: lisp

PKG = utr

EMACS ?= emacs
EMACS_ARGS ?=
SELECTOR ?= t

DEPS  =

LOAD_PATH  ?= $(addprefix -L ../,$(DEPS))
LOAD_PATH  += -L .

test: $(PKG).elc
	$(EMACS) -Q --batch -l  $(PKG)-tests.el \
	--eval '(ert-run-tests-batch (quote $(SELECTOR)))'

$(PKG).elc: $(PKG).el
	@rm -f $@
	$(EMACS) -Q --batch $(LOAD_PATH) \
	--eval '(byte-compile-file "$(<)")'
