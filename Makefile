#!/usr/bin/make
#
# Makefile for GEBEN

EMACS  = emacs
CP     = cp -p
RM     = rm -f
INSTALL = install -m 644

.el.elc:
	$(EMACS) -Q --batch --eval '(byte-compile-file "$<")'

SRCS    = geben.el
OBJS    = $(SRCS:%.el=%.elc)
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
    (princ (car (car (sort tbl (lambda (a b)		       \
			     (> (cdr a) (cdr b))))))))')

ifndef SITELISP
SITELISP := $(GUESS-SITELISP)
ifeq ($(SITELISP), nil)
$(error Cannot find appropriate site-lisp directory.)
endif
endif


DEST = $(SITELISP)/geben
DEST-IMG = $(DEST)/tree-widget/geben

.PHONY: all
all: $(OBJS)

.PHONY: install
install: all
	$(INSTALL) -d $(DEST)
	$(INSTALL) -t $(DEST) $(SRCS) $(OBJS)
	$(INSTALL) -d $(DEST-IMG)
	$(INSTALL) -t $(DEST-IMG) $(IMGS)

.PHONY: clean
clean:
	$(RM) $(OBJS)
