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

/* Perfect hashing function library. Contains functions to generate perfect
    hashing functions
 * (C) Mike van Emmerik
 */


#define TRUE 1
#define FALSE 0
#define bool unsigned char
#define byte unsigned char
#define word unsigned short

/* Prototypes */
void hashParams(int NumEntry, int EntryLen, int SetSize, char SetMin,
                    int NumVert);   /* Set the parameters for the hash table */
void hashCleanup(void);         /* Frees memory allocated by hashParams() */
void map(void);                 /* Part 1 of creating the tables */
void assign(void);              /* Part 2 of creating the tables */
int  hash(byte *s);             /* Hash the string to an int 0 .. NUMENTRY-1 */

word *readT1(void);             /* Returns a pointer to the T1 table */
word *readT2(void);             /* Returns a pointer to the T2 table */
word *readG(void);              /* Returns a pointer to the g  table */


/* The application must provide these functions: */
void getKey(int i, byte **pKeys);/* Set *keys to point to the i+1th key */
void dispKey(int i);            /* Display the key */


/* Macro reads a LH word from the image regardless of host convention */
#ifndef LH
#define LH(p)  ((int)((byte *)(p))[0] + ((int)((byte *)(p))[1] << 8))
#endif
