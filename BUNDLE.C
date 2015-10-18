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
 * File: bundle.c
 * Module that handles the bundle type (array of pointers to strings).
 * (C) Cristina Cifuentes
 ****************************************************************************/

#include "dcc.h"
#include <stdarg.h>
#include <malloc.h>
#include <memory.h>
#include <stdlib.h>
#include <string.h>

#define deltaProcLines  20


void newBundle (bundle *procCode)
/* Allocates memory for a new bundle and initializes it to zero.    */
{
    memset (&(procCode->decl), 0, sizeof(strTable));
    memset (&(procCode->code), 0, sizeof(strTable));
}


static void incTableSize (strTable *strTab)
/* Increments the size of the table strTab by deltaProcLines and copies all 
 * the strings to the new table.        */
{
    strTab->allocLines += deltaProcLines;
    strTab->str = allocVar (strTab->str, strTab->allocLines*sizeof(char *));
}


void appendStrTab (strTable *strTab, char *format, ...) 
/* Appends the new line (in printf style) to the string table strTab.   */
{  va_list args;

    va_start (args, format);
    if (strTab->numLines == strTab->allocLines)
    {
        incTableSize (strTab);
    }
    strTab->str[strTab->numLines] = (char *)malloc(lineSize * sizeof(char));
    if (strTab->str == NULL)
    {
        fatalError(MALLOC_FAILED, lineSize * sizeof(char));
    }
    vsprintf (strTab->str[strTab->numLines], format, args); 
    strTab->numLines++;
    va_end (args);
}


Int nextBundleIdx (strTable *strTab)
/* Returns the next available index into the table */
{
	return (strTab->numLines);
}


void addLabelBundle (strTable *strTab, Int idx, Int label)
/* Adds the given label to the start of the line strTab[idx].  The first
 * tab is removed and replaced by this label */
{ char s[lineSize];

	sprintf (s, "l%ld: %s", label, &strTab->str[idx][4]);
	strcpy (strTab->str[idx], s);
}


static void writeStrTab (FILE *fp, strTable strTab)
/* Writes the contents of the string table on the file fp.  */
{ Int i;

    for (i = 0; i < strTab.numLines; i++)
        fprintf (fp, "%s", strTab.str[i]); 
}


void writeBundle (FILE *fp, bundle procCode)
/* Writes the contents of the bundle (procedure code and declaration) to
 * a file.          */
{
    writeStrTab (fp, procCode.decl);
    if (procCode.decl.str[procCode.decl.numLines - 1][0] != ' ')
       fprintf (fp, "\n");
    writeStrTab (fp, procCode.code);
}


static void freeStrTab (strTable *strTab)
/* Frees the storage allocated by the string table. */
{ Int i;

    if (strTab->allocLines > 0)  {
       for (i = 0; i < strTab->numLines; i++)
         free (strTab->str[i]); 
       free (strTab->str);
       memset (strTab, 0, sizeof(strTable));
    }
}


void freeBundle (bundle *procCode)
/* Deallocates the space taken by the bundle procCode */
{ 
    freeStrTab (&(procCode->decl));
    freeStrTab (&(procCode->code));
}


