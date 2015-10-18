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
 * Error codes
 * (C) Cristina Cifuentes
 ****************************************************************************/

/* These definitions refer to errorMessage in error.c */

#define USAGE                   0
#define INVALID_ARG             1
#define INVALID_OPCODE          2
#define INVALID_386OP           3
#define FUNNY_SEGOVR            4
#define FUNNY_REP               5
#define CANNOT_OPEN             6
#define CANNOT_READ             7
#define MALLOC_FAILED           8
#define NEWEXE_FORMAT           9

#define NO_BB              		10
#define INVALID_SYNTHETIC_BB    11
#define INVALID_INT_BB          12
#define IP_OUT_OF_RANGE         13
#define DEF_NOT_FOUND           14
#define JX_NOT_DEF				15
#define NOT_DEF_USE				16
#define REPEAT_FAIL				17
#define WHILE_FAIL				18


#ifdef __MSDOS__
void fatalError(Int errId, ...);
void reportError(Int errId, ...);
#else
void fatalError();
void reportError();
#endif

