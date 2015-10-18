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


/*
 * File:    ast.h
 * Purpose: definition of the abstract syntax tree ADT.
 * Date:    September 1993
 * (C) Cristina Cifuentes
 */


#define operandSize		20

/* The following definitions and types define the Conditional Expression
 * attributed syntax tree, as defined by the following EBNF:
    CondExp     ::= CondTerm AND CondTerm | CondTerm
    CondTerm    ::= (CondFactor op CondFactor)
    CondFactor  ::= Identifier | ! CondFactor
    Identifier  ::= globalVar | register | localVar | parameter | constant
    op          ::= <= | < | = | != | > | >=
 */

/* Conditional Expression enumeration nodes and operators               */
typedef enum {
    BOOLEAN,            /* condOps  			*/
    NEGATION,           /* not (2's complement)	*/ 
	ADDRESSOF,			/* addressOf (&)		*/
	DEREFERENCE,		/* contents of (*) 		*/
    IDENTIFIER,         /* {register | local | param | constant | global} */
	/* The following are only available to C programs */
	POST_INC,			/* ++ (post increment)	*/
	POST_DEC,			/* -- (post decrement)	*/
	PRE_INC,			/* ++ (pre increment)	*/
	PRE_DEC,			/* -- (pre decrement)	*/
} condNodeType;

typedef enum {
    GLOB_VAR,       /* global variable  */
    REGISTER,       /* register         */
    LOCAL_VAR,      /* negative disp    */
    PARAM,          /* positive disp    */
	GLOB_VAR_IDX,	/* indexed global variable *//*** should merge w/glob-var*/
    CONST,          /* constant         */
	STRING,			/* string			*/
	LONG_VAR,		/* long variable	*/
	FUNCTION,		/* function			*/
    OTHER           /* other **** tmp solution */
} condId;

typedef enum {
        /* For conditional expressions */
    LESS_EQUAL = 0, /* <=   */
    LESS,           /* <    */ 
    EQUAL,          /* ==   */
    NOT_EQUAL,      /* !=   */
    GREATER,        /* >    */
    GREATER_EQUAL,  /* >=   */
        /* For general expressions */
    AND,            /* &    */
	OR,				/* |	*/
	XOR,			/* ^	*/
	NOT,			/* ~	*/  /* 1's complement */
	ADD,			/* +	*/
	SUB,			/* -	*/
	MUL,			/* *	*/
	DIV,			/* /	*/
	SHR,			/* >>	*/
	SHL,			/* <<	*/
	MOD,			/* %	*/
	DBL_AND,		/* &&	*/
	DBL_OR,			/* ||	*/
	DUMMY,			/*      */
} condOp;

/* High-level boolean conditions for iJB..iJNS icodes */
static condOp condOpJCond[12] = {LESS, LESS_EQUAL, GREATER_EQUAL, GREATER, 
								EQUAL, NOT_EQUAL, LESS, GREATER_EQUAL, 
								LESS_EQUAL, GREATER, GREATER_EQUAL, LESS};

static condOp invCondOpJCond[12] = {GREATER_EQUAL, GREATER, LESS, LESS_EQUAL,
									NOT_EQUAL, EQUAL, GREATER_EQUAL, LESS, 
									GREATER, LESS_EQUAL, LESS, GREATER_EQUAL};


/* Register types */
typedef enum {
    BYTE_REG, WORD_REG
} regType;


/* Expression data type */
typedef struct _condExpr {
    condNodeType            type;       /* Conditional Expression Node Type */
    union _exprNode {                   /* Different cond expr nodes        */
        struct {                        /* for BOOLEAN 						*/
            condOp           op;
            struct _condExpr *lhs;
            struct _condExpr *rhs;
        }                    boolExpr;
        struct _condExpr    *unaryExp;  /* for NEGATION,ADDRESSOF,DEREFERENCE*/
        struct _ident {                 /* for IDENTIFIER                   */
            condId           idType;
            regType          regiType;  /* for REGISTER only                */
            union _idNode {
                Int          regiIdx;   /* index into localId, REGISTER		*/
                Int          globIdx;   /* index into symtab for GLOB_VAR   */
                Int          localIdx;  /* idx into localId,  LOCAL_VAR		*/
                Int          paramIdx;  /* idx into args symtab, PARAMS     */
				Int			 idxGlbIdx;	/* idx into localId, GLOB_VAR_IDX   */
				struct _kte	{			/* for CONST only					*/
                	dword    kte;   	/*   value of the constant			*/
					byte	 size;		/*   #bytes size constant	 		*/
				}			 kte;
				dword		 strIdx;	/* idx into image, for STRING	 	*/
				Int			 longIdx;	/* idx into LOCAL_ID table, LONG_VAR*/
				struct _call {			/* for FUNCTION only				*/
					struct _proc     *proc;
					struct _STKFRAME *args;
				}			 call;
                struct {                /* for OTHER; tmp struct            */
                    byte     seg;       /*   segment                        */
                    byte     regi;      /*   index mode                     */
                    int16    off;       /*   offset                         */
                }            other;
            }                idNode;
        }                    ident;
    }                        expr;  
} COND_EXPR;

/* Sequence of conditional expression data type */
/*** NOTE: not used at present ****/
typedef struct _condExpSeq {
    COND_EXPR           *expr;
    struct _condExpSeq  *next;
} SEQ_COND_EXPR;


