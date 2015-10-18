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
 * Project: dcc
 * File:    bundle.h
 * Purpose: Module to handle the bundle type (array of pointers to strings).
 * (C) Cristina Cifuentes
 ****************************************************************************/

#include <stdio.h>

typedef struct {
    Int     numLines;   /* Number of lines in the table   */
    Int     allocLines; /* Number of lines allocated in the table */
    char    **str;      /* Table of strings */
} strTable;


typedef struct {
    strTable    decl;   /* Declarations */
    strTable    code;   /* C code       */
} bundle;


#define lineSize	360		/* 3 lines in the mean time */

void    newBundle (bundle *procCode);
void    appendStrTab (strTable *strTab, char *format, ...);
Int		nextBundleIdx (strTable *strTab);
void	addLabelBundle (strTable *strTab, Int idx, Int label);
void    writeBundle (FILE *fp, bundle procCode);
void    freeBundle (bundle *procCode);

