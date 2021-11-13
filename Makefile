#******************************************************************************
#FILE:              Makefile
#LANGUAGE:          make
#SYSTEM:            UNIX
#USER-INTERFACE:    None
#DESCRIPTION
#USAGE
#AUTHORS
#    <PJB> Pascal J. Bourguignon
#MODIFICATIONS
#    2003-01-17 <PJB> Added this header comment.
#BUGS
#LEGAL
#    GPL
#
#    Copyright Pascal J. Bourguignon 2003 - 2003
#
#    This program is free software; you can redistribute it and/or
#    modify it under the terms of the GNU General Public License
#    as published by the Free Software Foundation; either version
#    2 of the License, or (at your option) any later version.
#
#    This program is distributed in the hope that it will be
#    useful, but WITHOUT ANY WARRANTY; without even the implied
#    warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
#    PURPOSE.  See the GNU General Public License for more details.
#
#    You should have received a copy of the GNU General Public
#    License along with this program; if not, write to the Free
#    Software Foundation, Inc., 59 Temple Place, Suite 330,
#    Boston, MA 02111-1307 USA
#******************************************************************************
PREFIX ?= /usr/local
BINDIR=$(PREFIX)/bin

EMACS_FLAGS = -l ~/.emacs

ifeq ($(shell uname),Darwin)
EMACS=/opt/local/bin/emacs
CLISP=/opt/local/bin/clisp
SBCL=/opt/local/bin/sbcl
else
EMACS=/usr/local/bin/emacs
CLISP=/usr/local/bin/clisp
SBCL=/usr/local/bin/sbcl
endif


all: lisp-script emacs-script clisp-script
	-@chmod a+r *

emacs-script: lisp-script
	ln -s lisp-script emacs-script

clisp-script: lisp-script
	ln -s lisp-script clisp-script

lisp-script: lisp-script.c
	$(CC) \
		-DEMACS=\"$(EMACS)\" -DCLISP=\"$(CLISP)\" -DSBCL=\"$(SBCL)\" \
		-g -Wall -o lisp-script lisp-script.c

clean:
	rm -f lisp-script emacs-script clisp-script

install: all
	@echo "Installing programs: emacs-script clisp-script"
	-@umask 022 ; mkdir -p $(BINDIR) 2>/dev/null || true
	install -m 644 emacs-script.el   $(BINDIR)
	install -m 644 clisp-script.lisp $(BINDIR)
	install -m 755 lisp-script       $(BINDIR)
	ln -s -f lisp-script $(BINDIR)/emacs-script
	ln -s -f lisp-script $(BINDIR)/clisp-script

#### THE END ####
