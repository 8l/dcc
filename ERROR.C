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

/****************************************************************************
 *          dcc project error messages
 * (C) Cristina Cifuentes
 ***************************************************************************/

#include "dcc.h"
#include <stdio.h> 
#include <stdlib.h>
#ifdef __MSDOS__
#include <stdarg.h>
#else
#include <varargs.h>
#endif

static char *errorMessage[] = {
  "Invalid option -%c\n",                                   /* INVALID_ARG    */
  "Invalid instruction %02X at location %06lX\n",           /* INVALID_OPCODE */
  "Don't understand 80386 instruction %02X at location %06lX\n",
															/* INVALID_386OP  */
  "Segment override with no memory operand at location %06lX\n",
															/* FUNNY_SEGOVR   */
  "REP prefix without a string instruction at location %06lX\n",/* FUNNY_REP */
  "Cannot open %s\n",                                       /* CANNOT_OPEN    */
  "Error while reading %s\n",                               /* CANNOT_READ    */
  "malloc of %ld bytes failed\n",                           /* MALLOC_FAILED  */
  "Don't understand new EXE format\n",                      /* NEWEXE_FORMAT  */
  "Failed to find a BB for jump to %ld in proc %s\n",    	/* NO_BB		  */
  "Basic Block is a synthetic jump\n",                /* INVALID_SYNTHETIC_BB */
  "Failed to find a BB for interval\n",                     /* INVALID_INT_BB */
  "Instruction at location %06lX goes beyond loaded image\n",   
															/* IP_OUT_OF_RANGE*/
  "Definition not found for condition code usage at opcode %d\n",
															/* DEF_NOT_FOUND */
  "JX use, definition not supported at opcode #%d\n",		/* JX_NOT_DEF */
  "Def - use not supported.  Def op = %d, use op = %d.\n",  /* NOT_DEF_USE */
  "Failed to construct repeat..until() condition.\n",		/* REPEAT_FAIL */
  "Failed to construct while() condition.\n",				/* WHILE_FAIL */
};


/****************************************************************************
 fatalError: displays error message and exits the program.
 ****************************************************************************/
#ifdef __MSDOS__
void fatalError(Int errId, ...)
#else
void fatalError(va_alist) va_dcl
#endif
{  va_list args;
#ifndef __MSDOS__   /* ultrix */
   Int errId;

    va_start(args);
    errId = va_arg(args, Int);
#else
    va_start(args, errId);
#endif

    if (errId == USAGE)
       fprintf(stderr,"Usage: %s [-a1a2mpsvV][-o asmfile] DOS_executable\n",
            progname);
    else {
        fprintf(stderr, "%s: ", progname);
        vfprintf(stderr, errorMessage[errId - 1], args);
    }
    va_end(args);
    exit((int)errId);
}


/****************************************************************************
 reportError: reports the warning/error and continues with the program.
 ****************************************************************************/
#ifdef __MSDOS__
void reportError(Int errId, ...)
#else
void reportError(va_alist) va_dcl
#endif
{  va_list args;
#ifndef __MSDOS__   /* ultrix */
   Int errId;

    va_start(args);
    errId = va_arg(args, Int);
#else           /* msdos */
    va_start(args, errId);
#endif
    fprintf(stderr, "%s: ", progname);
    vfprintf(stderr, errorMessage[errId - 1], args);
    va_end(args);
}
