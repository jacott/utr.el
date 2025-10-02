.PHONY: test

all: lisp

PKG = utr

EMACS ?= emacs
EMACS_ARGS ?=
SELECTOR ?= t

DEPS  =

LOAD_PATH  ?= $(addprefix -L ../,$(DEPS))
LOAD_PATH  += -L .

test: .emacs.d/init.el $(PKG).elc
	$(EMACS) -Q --init-directory=./.emacs.d --batch $(LOAD_PATH) \
	-l .emacs.d/init.el \
	-l  $(PKG)-tests.el \
	--eval '(ert-run-tests-batch (quote $(SELECTOR)))'

.emacs.d/init.el: build/init.el
	@mkdir -p .emacs.d
	@cp $< $@

$(PKG).elc: $(PKG).el
	@rm -f $@
	$(EMACS) -Q --init-directory=./.emacs.d --batch $(LOAD_PATH) \
	-l .emacs.d/init.el \
	--eval '(byte-compile-file "$(<)")'
