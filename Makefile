WGET  ?= wget
EMACS  = emacs

EMACSFLAGS =
EMACSBATCH = $(EMACS) --batch -Q $(EMACSFLAGS)

export EMACS

SRCS = inf-crystal.el
OBJS = $(SRCS:.el=.elc)

.PHONY: all compile clean

all: compile README.md

compile: $(OBJS)

clean:
	$(RM) $(OBJS)

README.md: make-readme-markdown.el $(SRCS)
	$(EMACSBATCH) --script $< <$(SRCS) >$@ 2>/dev/null

make-readme-markdown.el:
	$(WGET) -q -O $@ "https://github.com/mgalgs/make-readme-markdown/raw/master/make-readme-markdown.el"

.INTERMEDIATE: make-readme-markdown.el
