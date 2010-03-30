#!/usr/bin/make
#
# Makefile for GEBEN

EMACS   = emacs
CP      = cp -p
RM      = rm -f
INSTALL = install
ZIP	= zip

.el.elc:
	$(EMACS) -Q --batch --eval "(add-to-list 'load-path \".\")" --eval '(byte-compile-file "$<")'

DBGP-SRC	= dbgp.el
GEBEN-SRC	= geben.el
SRCS		= $(DBGP-SRC) $(GEBEN-SRC)
OBJS		= $(SRCS:%.el=%.elc)

GEBEN-MODULES	= \
	geben-common.el \
	geben-util.el \
	geben-dbgp-util.el \
	geben-storage.el \
	geben-session.el \
	geben-cmd.el \
	geben-dbgp.el \
	geben-source.el \
	geben-cursor.el \
	geben-breakpoint.el \
	geben-context.el \
	geben-backtrace.el \
	geben-redirect.el \
	geben-dbgp-start.el \
	geben-dbgp-init.el \
	geben-mode.el \
	geben-start.el

DIST-MODULES	= \
	ChangeLog \
	LICENSE \
	Makefile \
	NEWS \
	README \
	geben-dbgp.el \
	geben.el \
	etc \
	tree-widget

MODULES		= $(DBGP-SRC) $(GEBEN-MODULES)
MODULE-OBJS	= $(MODULES:%.el=%.elc)

IMGDIR  = tree-widget/geben
IMGS    = $(wildcard $(IMGDIR)/*.png)

GUESS-SITELISP := $(shell $(EMACS) -Q --batch --eval '	       \
  (let (tbl)						       \
    (mapc (lambda (path)				       \
	    (if (string-match "^\\(.*/site-lisp\\)\\b/?" path) \
		(let* ((spath (match-string 1 path))	       \
		       (pair (assoc spath tbl)))	       \
		  (if pair				       \
		      (setcdr pair (1+ (cdr pair)))	       \
		    (setq tbl (cons (cons spath 1) tbl))))))   \
	  load-path)					       \
    (princ (or (car (car (sort tbl (lambda (a b)	       \
				     (> (cdr a) (cdr b))))))   \
	       "")))')

ifndef SITELISP
SITELISP := $(GUESS-SITELISP)
ifeq ($(SITELISP), nil)
$(error Cannot find appropriate site-lisp directory.)
endif
endif


DEST = $(SITELISP)/geben
DEST-IMG = $(DEST)/tree-widget/geben

.PHONY: all
all: compile-modules

.PHONY: compile-modules
compile-modules: $(MODULE-OBJS)

.PHONY: compile
compile: $(OBJS)

.PHONY: install
install: compile
	$(INSTALL) -m 755 -d $(DEST)
	$(INSTALL) -m 644 $(SRCS) $(OBJS) $(DEST)
	$(INSTALL) -m 755 -d $(DEST-IMG)
	$(INSTALL) -m 644 $(IMGS) $(DEST-IMG)

.PHONY: clean
clean:
	$(RM) $(OBJS) $(MODULE-OBJS) $(GEBEN-SRC)

.PHONY: dist-package
dist-package:
	@$(ZIP) -r geben-`head -n 1 README | sed 's+.*-\\([0-9.]*\\) .*$$+\\1+'`.zip $(DIST-MODULES)

$(GEBEN-SRC): $(GEBEN-MODULES)
	perl -an -e '$$x ? (/^\(provide/ and $$x=0) : (/^\(require/ or $$x=1); print if $$x' $^ > $@
	echo "(provide 'geben)" >> $@
