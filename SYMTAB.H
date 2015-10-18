/*
 * Copyright (C) 1993, Queensland University of Technology
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

/*
 * Symbol table prototypes
 * (C) Mike van Emmerik
*/

/* * * * * * * * * * * * * * * * * */
/* Symbol table structs and protos */
/* * * * * * * * * * * * * * * * * */

typedef struct
{
    char    *pSymName;              /* Ptr to symbolic name or comment */
    dword   symOff;                 /* Symbol image offset */
    PPROC   symProc;                /* Procedure pointer */
    word    preHash;                /* Hash value before the modulo */
    word    postHash;               /* Hash value after the modulo */
    word    nextOvf;                /* Next entry this hash bucket, or -1 */
    word    prevOvf;                /* Back link in Ovf chain */
} SYMTABLE;

enum _tableType                     /* The table types */
{
    Label=0,                        /* The label table */
    Comment,                        /* The comment table */
    NUM_TABLE_TYPES                 /* Number of entries: must be last */
};              

typedef enum _tableType tableType;  /* For convenience */

void    createSymTables(void);
void    destroySymTables(void);
void    enterSym(char *symName, dword   symOff, PPROC   symProc, boolT bSymToo);
boolT   readSym (char *symName, dword *pSymOff, PPROC *pSymProc);
boolT   readVal (char *symName, dword   symOff, PPROC   symProc);
void    deleteSym(char *symName);
void    deleteVal(dword symOff, PPROC symProc, boolT bSymToo);
boolT   findVal(dword symOff, PPROC symProc, word *pIndex);
word    symHash(char *name, word *pre);
word    valHash(dword off, PPROC proc, word *pre);
void    selectTable(tableType);     /* Select a particular table */

char   *addStrTbl(char *pStr);      /* Add string to string table */

