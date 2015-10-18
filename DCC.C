/*
 * Copyright (C) 1991-4, Cristina Cifuentes
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 */

/*****************************************************************************
 *              dcc decompiler
 * Reads the command line switches and then executes each major section in turn
 * (C) Cristina Cifuentes
 ****************************************************************************/


#include "dcc.h"
#include <string.h>
#ifndef __MSDOS__
#include <unistd.h>
#endif

#                   include <malloc.h>  /* Debuging only */

/* Global variables - extern to other modules */
char    *progname;          /* argv[0] - for error msgs 			  */
char    *asm1_name, *asm2_name;     /* Assembler output filenames     */
SYMTAB  symtab;             /* Global symbol table      			  */
STATS   stats;              /* cfg statistics       				  */
PROG    prog;               /* programs fields      				  */
OPTION  option;             /* Command line options     			  */
PPROC   pProcList;			/* List of procedures, topologically sort */
PPROC	pLastProc;			/* Pointer to last node in procedure list */
CALL_GRAPH	*callGraph;		/* Call graph of the program			  */

static char *initargs(int argc, char *argv[]);


/****************************************************************************
 * main
 ***************************************************************************/
Int main(int argc, char *argv[])
{  
    char   *filename;

    /* Extract switches and filename */
    filename = initargs(argc, argv);


    /* Front end reads in EXE or COM file, parses it into I-code while 
     * building the call graph and attaching appropriate bits of code for 
     * each procedure.
    */
    FrontEnd (filename, &callGraph);

    /* In the middle is a so called Universal Decompiling Machine.
     * It processes the procedure list and I-code and attaches where it can 
     * to each procedure an optimised cfg and ud lists
    */
    udm();

    /* Back end converts each procedure into C using I-code, interval 
     * analysis, data flow etc. and outputs it to output file ready for 
     * re-compilation.
    */  
	BackEnd(filename, callGraph);

	writeCallGraph (callGraph);
/* 
    freeDataStructures(pProcList);
*/
    return(0);
}
/****************************************************************************
 * initargs - Extract command line arguments
 ***************************************************************************/
static char *initargs(int argc, char *argv[])
{
    char *pc;
    progname = *argv;   /* Save invocation name for error messages */

    while (--argc > 0 && (*++argv)[0] == '-') {
        for (pc = argv[0]+1; *pc; pc++)
            switch (*pc) {
            case 'a':       /* Print assembler listing */
                if (*(pc+1) == '2')
                    option.asm2 = TRUE;
                else
                    option.asm1 = TRUE;
                if (*(pc+1) == '1' || *(pc+1) == '2')
                    pc++;
                break;
            case 'i':
                option.Interact = TRUE;
                break;
            case 'm':       /* Print memory map */
                option.Map = TRUE;
                break;
            case 's':       /* Print Stats */
                option.Stats = TRUE;
                break;
            case 'V':       /* Very verbose => verbose */
                option.VeryVerbose = TRUE;
            case 'v':       /* Make everything verbose */
                option.verbose = TRUE;
                break;
            case 'o':       /* assembler output file */
                if (*(pc+1)) {
                    asm1_name = asm2_name = pc+1;
                    goto NextArg;
                }
                else if (--argc > 0) {
                    asm1_name = asm2_name = *++argv;
                    goto NextArg;
                }
            default:
                fatalError(INVALID_ARG, *pc);
            }
    NextArg:;
    }

    if (argc == 1) {
        if (option.asm1 || option.asm2) {
            if (! asm1_name) {
                asm1_name = strcpy(allocMem(strlen(*argv)+4), *argv);
                pc = strrchr(asm1_name, '.'); 
                if (pc > strrchr(asm1_name, '/'))
                    *pc = '\0';
                asm2_name = allocMem(strlen(asm1_name)+4) ;
                strcat(strcpy(asm2_name, asm1_name), ".a2"); 
                unlink(asm2_name);
                strcat(asm1_name, ".a1");
            }
            unlink(asm1_name);  /* Remove asm output files */
        }
        return *argv;       /* filename of the program to decompile */
    }

    fatalError(USAGE);
}
