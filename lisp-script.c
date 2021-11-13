/******************************************************************************
FILE:               lisp-script.c
LANGUAGE:           ANSI-C
SYSTEM:             POSIX
USER-INTERFACE:     POSIX
DESCRIPTION
    This is a simple tool that encapsulate clisp or emacs invokation as
    script interpreters.
USAGE
    Insert #!/usr/local/bin/emacs-script
    or     #!/usr/local/bin/clisp-script
    as the first line of a lisp program, and run it.
AUTHORS
    <PJB> Pascal J. Bourguignon
MODIFICATIONS
    2003-10-27 <PJB> Added SBCL.
    2002-04-06 <PJB> Creation.
BUGS
LEGAL
    GPL
    Copyright Pascal J. Bourguignon 2002 - 2003

    This file is part of lisp script tools.

    This  program is  free software;  you can  redistribute  it and/or
    modify it  under the  terms of the  GNU General Public  License as
    published by the Free Software Foundation; either version 2 of the
    License, or (at your option) any later version.

    This program  is distributed in the  hope that it  will be useful,
    but  WITHOUT ANY WARRANTY;  without even  the implied  warranty of
    MERCHANTABILITY or FITNESS FOR  A PARTICULAR PURPOSE.  See the GNU
    General Public License for more details.

    You should have received a  copy of the GNU General Public License
    along with  this program; see the  file COPYING; if  not, write to
    the Free  Software Foundation, Inc.,  59 Temple Place,  Suite 330,
    Boston, MA 02111-1307 USA
******************************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <sysexits.h>

#ifndef EMACS
#define EMACS "/usr/local/bin/emacs"
#endif

#ifndef CLISP
#define CLISP "/usr/local/bin/clisp"
#endif

#ifndef SBCL
#define SBCL  "/usr/local/bin/sbcl"
#endif


#ifdef MACOSX

    /* Not exactly correct. We should grab GNU dirname/basename.*/

    static const char* basename(const char* path)
    {
        const char* slash=strrchr(path,'/');
        if(slash==0){
            return(path);
        }else{
            return(slash+1);
        }
    }/*basename*/

#else
#include <libgen.h>
#endif


    const char* pname="lisp-script";

    void usage(const char* pname)
    {
        fprintf(stderr,
                "%s usage:\n"\
                "    %s -h|--help | script-file [script-arguments...]\n",
                pname,pname);
    }/*usage*/


int main(int argc,char** argv)
{
    int i;
    int length;
    int position;
    char* script_file=0;
    char* script_option;
    char* init_file;
    enum { emacs, clisp, sbcl } kind;

    pname=basename(argv[0]);
    if(strcmp(pname,"emacs-script")==0){
        kind=emacs;
    }else if(strcmp(pname,"clisp-script")==0){
        kind=clisp;
    }else if(strcmp(pname,"sbcl-script")==0){
        kind=sbcl;
    }else{
      lisp_script:
        fprintf(stderr,"%s: should be invoked either as emacs-script, "
                "clisp-script or sbcl-script.\n",pname);
        return(EX_USAGE);
    }

    if((strcmp(argv[1],"-h")==0)||(strcmp(argv[1],"--help")==0)){
        usage(pname);
        return(EX_OK);
    }

    if(1<argc){
        script_file=argv[1];
    }else{
        fprintf(stderr,"%s: missing script-file argument.\n",pname);
        usage(pname);
        return(EX_USAGE);
    }

    if(0!=access(script_file,R_OK)){
        fprintf(stderr,"Can't read script file '%s'.\n",script_file);
        return(EX_NOINPUT);
    }

    init_file=(char*)malloc(strlen(argv[0])+8);
    switch(kind){
    case emacs:
        sprintf(init_file,"%s.el",argv[0]);
        break;
    case clisp:
    case sbcl:
        sprintf(init_file,"%s.lisp",argv[0]);
        break;
    default:
        goto lisp_script;
    }

    if(0!=access(init_file,R_OK)){
        fprintf(stderr,"Can't read %s init file '%s'.\n",argv[0],init_file);
        return(EX_UNAVAILABLE);
    }

    length=0;
    for(i=1;i<argc;i++){
        length+=3+strlen(argv[i]);
    }
    script_option=(char*)malloc(length+16);
    position=0;
    position+=sprintf(script_option+position,"(script ");
    for(i=1;i<argc;i++){
        position+=sprintf(script_option+position,"\"%s\" ",argv[i]);
    }
    position+=sprintf(script_option+position,")");
    /*
    fprintf(stderr,"init_file=%s\n",init_file);
    fprintf(stderr,"script_option=%s\n",script_option);
    */

    switch(kind){
    case emacs:
        execl(EMACS,"emacs","--batch","--no-site-file","--no-init-file",
              "--load",init_file,
              "--eval",script_option,
              0);
        fprintf(stderr,"Can't execl '%s'.\n",EMACS);
        break;
    case clisp:
        execl(CLISP,"clisp","-q",
              "-i",init_file,
              "-x",script_option,
              0);
        fprintf(stderr,"Can't execl '%s'.\n",CLISP);
        break;
    case sbcl:
        execl(SBCL,"sbcl","--noinform",
              "--userinit",init_file,
              "--eval",script_option,
              0);
        fprintf(stderr,"Can't execl '%s'.\n",SBCL);
        break;
    default:
        goto lisp_script;
    }
    return(EX_UNAVAILABLE);
}/*main*/

/*** lisp-script.c                    -- 2003-10-27 22:57:16 -- pascal   ***/
