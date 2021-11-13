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
MODULE=emacs
PACKAGE_PATH=com/informatimago/emacs
BINDIR=$(PREFIX)/bin

EMACS_FLAGS= -l .emacs #-l pjb-cl.el

# .el.elc: ; 	@echo '' ; echo Compiling $< ; emacs -batch -q $(EMACSFLAGS) -f batch-byte-compile $< 2>&1 | egrep -v -e 'Loading .*/fns-'
# See also .emacs in this directory with the load-path used to compile.


EMACS_SOURCES=\
		pjb-cl.el \
		pjb-list.el \
		\
		pjb-utilities.el \
		pjb-queue.el \
		pjb-strings.el \
		pjb-s2p-expression.el \
		pjb-i2p-expression.el \
		pjb-roman.el \
		pjb-primes.el \
		pjb-pgp.el \
		pjb-graph.el \
		pjb-dot.el \
		pjb-cvspass.el \
		pjb-cvs.el \
		\
		pjb-object.el \
		pjb-class.el \
		pjb-sources.el \
		pjb-bourse.el \
		pjb-selftrade.el \
		pjb-euro.el \
		pjb-invoices.el \
		pjb-banks.el \
		pjb-work.el \
		\
		pjb-emacs.el \
		pjb-mail.el \
		pjb-vm-kill-file.el \
		pjb-layers.el \
		pjb-c.el \
		pjb-advices.el

OBJECTS=\
		$(EMACS_OBJECTS) \
		lisp-script

INHIBITED=\
		pjb-prolog-mode.elc

all: lisp-script
	-@chmod a+r *

lisp-script:lisp-script.c
	$(CC) -g -Wall -o lisp-script lisp-script.c

install:: all  install-packages
	@echo "Installing programs: emacs-script clisp-script"
	-@umask 022 ; mkdir -p $(BINDIR) 2>/dev/null || true
	install -m 644 emacs-script.el   $(BINDIR)
	install -m 644 clisp-script.lisp $(BINDIR)
	install -m 755 lisp-script       $(BINDIR)
	ln -s -f lisp-script $(BINDIR)/emacs-script
	ln -s -f lisp-script $(BINDIR)/clisp-script


test.elc:test.el

#### THE END ####
