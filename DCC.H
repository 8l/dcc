/*
 * Copyright (C) 1991-4, Cristina Cifuentes
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

/****************************************************************************
 *          dcc project general header
 * (C) Cristina Cifuentes, Mike van Emmerik
 ****************************************************************************/


/**** Common definitions and macros ****/
#ifdef __MSDOS__            /* Intel: 16 bit integer        */
typedef long    Int;        /* Int: 0x80000000..0x7FFFFFFF  */
typedef unsigned long flags32;  /* 32 bits  */
typedef unsigned long dword;    /* 32 bits  */
#define MAX 0x7FFFFFFF
#else                       /* Unix: 32 bit integer         */
typedef int Int;            /* Int: 0x80000000..0x7FFFFFFF  */
typedef unsigned int flags32;  /* 32 bits  */
typedef unsigned int dword;    /* 32 bits  */
#define MAX 0x7FFFFFFF
#endif

/* Type definitions used in the program */
typedef unsigned char byte; /* 8 bits   */ 
typedef unsigned short word;/* 16 bits  */
typedef short int16;        /* 16 bits  */
typedef unsigned char boolT; /* 8 bits   */


#ifndef TRUE                /* X curses defines these already */
#define TRUE    1
#define FALSE   0
#endif

#define SYNTHESIZED_MIN 0x100000    /* Synthesized labs use bits 21..32 */

/* These are for C library signature detection */
#define SYMLEN  16                  /* Length of proc symbols, incl null */
#define PATLEN  23                  /* Length of proc patterns  */
#define WILD    0xF4                /* The wild byte            */

/****** MACROS *******/

/* Macro to allocate a node of size sizeof(structType). */
#define allocStruc(structType)  (structType *)allocMem(sizeof(structType))

/* Macro reads a LH word from the image regardless of host convention */
/* Returns a 16 bit quantity, e.g. C000 is read into an Int as C000 */
#define LH(p)  ((int16)((byte *)(p))[0] + ((int16)((byte *)(p))[1] << 8))

/* Macro reads a LH word from the image regardless of host convention */
/* Returns a signed quantity, e.g. C000 is read into an Int as FFFFC000 */
#define LHS(p) (((byte *)(p))[0] + (((char *)(p))[1] << 8))

/* Macro tests bit b for type t in prog.map */
#define BITMAP(b, t)  (prog.map[(b) >> 2] & ((t) << (((b) & 3) << 1)))

/* Macro to convert a segment, offset definition into a 20 bit address */
#define opAdr(seg,off)  ((seg << 4) + off)


#include "ast.h"
#include "icode.h"
#include "locident.h"
#include "error.h"
#include "graph.h"
#include "bundle.h"


/* STATE TABLE */
typedef struct {
    dword       IP;             /* Offset into Image                    */
    int16       r[INDEXBASE];   /* Value of segs and AX                 */
    byte        f[INDEXBASE];   /* True if r[.] has a value             */
    struct {                    /* For case stmt indexed reg            */
        byte    regi;           /*   Last conditional jump              */
        int16   immed;          /*   Contents of the previous register  */
    }           JCond;
} STATE;
typedef STATE *PSTATE;


/* SYMBOL TABLE */
typedef struct {        
    char        name[10];   /* New name for this variable   */
    dword       label;      /* physical address (20 bit)    */
    Int         size;       /* maximum size                 */
	flags32     flg;        /* SEG_IMMED, IMPURE, WORD_OFF  */
    hlType      type;       /* probable type                */
    word        duVal;      /* DEF, USE, VAL                */
} SYM;
typedef SYM *PSYM;

typedef struct {
    Int         csym;       /* No. of symbols in table  */
    Int         alloc;      /* Allocation               */
    PSYM        sym;        /* Symbols                  */
} SYMTAB;
typedef SYMTAB *PSYMTAB;


/* STACK FRAME */
typedef struct {        
	COND_EXPR	*actual;	/* Expression tree of actual parameter 		*/
	COND_EXPR 	*regs;		/* For register arguments only				*/
    int16       off;        /* Immediate off from BP (+:args, -:params) */
    byte        regOff;     /* Offset is a register (e.g. SI, DI)       */
    Int         size;       /* Size             						*/
    hlType      type;       /* Probable type    						*/
    word        duVal;      /* DEF, USE, VAL    						*/
	boolT		hasMacro;	/* This type needs a macro					*/
	char		macro[10];	/* Macro name								*/
    char        name[10];   /* Name for this symbol/argument            */
	boolT		invalid;	/* Boolean: invalid entry in formal arg list*/
} STKSYM;
typedef STKSYM *PSTKSYM;

typedef struct _STKFRAME {
    Int         csym;       /* No. of symbols in table      */
    Int         alloc;      /* Allocation                   */
    PSTKSYM     sym;        /* Symbols                      */
	int16		minOff;		/* Initial offset in stack frame*/
    int16       maxOff;     /* Maximum offset in stack frame*/
	Int			cb;			/* Number of bytes in arguments	*/
	Int			numArgs;	/* No. of arguments in the table*/
} STKFRAME;
typedef STKFRAME *PSTKFRAME;


/* PROCEDURE NODE */
typedef struct _proc {
    dword        procEntry; /* label number                         	 */
    char         name[SYMLEN]; /* Meaningful name for this proc     	 */
    STATE        state;     /* Entry state                          	 */
    Int          depth;     /* Depth at which we found it - for printing */
	flags32      flg;       /* Combination of Icode & Proc flags    	 */
    int16        cbParam;   /* Probable no. of bytes of parameters  	 */
    STKFRAME     args;      /* Array of arguments                   	 */
	LOCAL_ID	 localId;	/* Local identifiers						 */
	ID			 retVal;	/* Return value - identifier				 */

	/* Icodes and control flow graph */
	ICODE_REC	 Icode;		/* Record of ICODE records					 */
    PBB          cfg;       /* Ptr. to BB list/CFG                  	 */
    PBB         *dfsLast;   /* Array of pointers to BBs in dfsLast
                             * (reverse postorder) order            	 */
    Int          numBBs;    /* Number of BBs in the graph cfg       	 */
    boolT        hasCase;   /* Procedure has a case node            	 */

	/* For interprocedural live analysis */
	dword		 liveIn;	/* Registers used before defined			 */
	dword		 liveOut;	/* Registers that may be used in successors	 */
	boolT		 liveAnal;	/* Procedure has been analysed already		 */

	/* Double-linked list */
    struct      _proc *next;
	struct		_proc *prev;
} PROC;
typedef PROC *PPROC;


/* CALL GRAPH NODE */
typedef struct _callGraph {
	PPROC			  proc;			/* Pointer to procedure in pProcList	*/
	Int				  numOutEdges;	/* # of out edges (ie. # procs invoked)	*/
	Int				  numAlloc;		/* # of out edges allocated				*/
	struct _callGraph **outEdges;	/* array of out edges					*/
} CALL_GRAPH;
typedef CALL_GRAPH *PCALL_GRAPH;
#define NUM_PROCS_DELTA		5		/* delta # procs a proc invokes		 	*/

extern PPROC pProcList;         /* Pointer to the head of the procedure list */
extern PPROC pLastProc;			/* Pointer to last node of the proc list     */
extern PCALL_GRAPH callGraph;	/* Pointer to the head of the call graph     */
extern bundle cCode;			/* Output C procedure's declaration and code */

/* Procedure FLAGS */
#define PROC_BADINST   0x000100 /* Proc contains invalid or 386 instruction */
#define PROC_IJMP      0x000200 /* Proc incomplete due to indirect jmp	 	*/
#define PROC_ICALL     0x000400 /* Proc incomplete due to indirect call		*/
#define PROC_HLL       0x001000 /* Proc is likely to be from a HLL			*/
#define CALL_PASCAL    0x002000 /* Proc uses Pascal calling convention		*/
#define CALL_C         0x004000 /* Proc uses C calling convention			*/
#define CALL_UNKNOWN   0x008000 /* Proc uses unknown calling convention		*/
#define PROC_NEAR      0x010000 /* Proc exits with near return				*/
#define PROC_FAR       0x020000 /* Proc exits with far return				*/
#define GRAPH_IRRED    0x100000 /* Proc generates an irreducible graph		*/
#define SI_REGVAR	   0x200000 /* SI is used as a stack variable 			*/
#define DI_REGVAR	   0x400000 /* DI is used as a stack variable 			*/
#define PROC_IS_FUNC   0x800000	/* Proc is a function 						*/
#define REG_ARGS	  0x1000000 /* Proc has registers as arguments			*/
#define	PROC_VARARG	  0x2000000	/* Proc has variable arguments				*/
#define PROC_OUTPUT   0x4000000 /* C for this proc has been output 			*/
#define PROC_RUNTIME  0x8000000 /* Proc is part of the runtime support		*/
#define PROC_ISLIB	 0x10000000 /* Proc is a library function				*/
#define PROC_ASM	 0x20000000 /* Proc is an intrinsic assembler routine   */
#define PROC_IS_HLL  0x40000000 /* Proc has HLL prolog code					*/
#define CALL_MASK    0xFFFF9FFF /* Masks off CALL_C and CALL_PASCAL		 	*/

/* duVal FLAGS */
#define DEF 0x0010      /* Variable was first defined than used 		*/
#define USE 0x0100      /* Variable was first used than defined 		*/
#define VAL 0x1000      /* Variable has an initial value.  2 cases:
                         * 1. When variable is used first (ie. global)
                         * 2. When a value is moved into the variable
                         *    for the first time.       				*/
#define USEVAL  0x1100  /* Use and Val              					*/


/**** Global variables ****/

extern char *progname;              /* Saved argv[0] for error messages */
extern char *asm1_name, *asm2_name; /* Assembler output filenames 		*/

typedef struct {            /* Command line option flags */
    unsigned verbose        : 1;
    unsigned VeryVerbose    : 1;
    unsigned asm1           : 1;    /* Early disassembly listing */
    unsigned asm2           : 1;    /* Disassembly listing after restruct */
    unsigned Map            : 1;
    unsigned Stats          : 1;
    unsigned Interact       : 1;    /* Interactive mode */
} OPTION;

extern OPTION option;       /* Command line options             */
extern SYMTAB symtab;       /* Global symbol table              */

typedef struct {            /* Loaded program image parameters  */
    int16       initCS;
    int16       initIP;     /* These are initial load values    */
    int16       initSS;     /* Probably not of great interest   */
    int16       initSP;
    boolT       fCOM;       /* Flag set if COM program (else EXE)*/
    Int         cReloc;     /* No. of relocation table entries  */
    dword      *relocTable; /* Ptr. to relocation table         */
    byte       *map;        /* Memory bitmap ptr                */
    Int         cProcs;     /* Number of procedures so far      */
    Int         offMain;    /* The offset  of the main() proc   */
    word        segMain;    /* The segment of the main() proc   */
    Int         cbImage;    /* Length of image in bytes         */
    byte       *Image;      /* Allocated by loader to hold entire 
                             * program image                    */
} PROG;

extern PROG prog;   		/* Loaded program image parameters  */
extern char condExp[200];	/* Conditional expression buffer 	*/
extern char callBuf[100];	/* Function call buffer				*/
extern dword duReg[30];		/* def/use bits for registers		*/
extern dword maskDuReg[30];	/* masks off du bits for regs		*/

/* Registers used by icode instructions */
static char *allRegs[21] = {"ax", "cx", "dx", "bx", "sp", "bp", 
                			"si", "di", "es", "cs", "ss", "ds", 
							"al", "cl", "dl", "bl", "ah", "ch", "dh", "bh",
							"tmp"};

/* Memory map states */
#define BM_UNKNOWN  0   /* Unscanned memory     */
#define BM_DATA     1   /* Data                 */
#define BM_CODE     2   /* Code                 */
#define BM_IMPURE   3   /* Used as Data and Code*/

/* Graph statistics */
typedef struct {
    Int numBBbef;       /* # BBs before deleting redundant ones   */
    Int numBBaft;       /* # BBs after deleting redundant ones    */
    Int numEdgesBef;    /* # out edges before removing redundancy */
    Int numEdgesAft;    /* # out edges after removing redundancy  */
    Int nOrder;         /* nth order graph, value for n           */
} STATS;

extern STATS stats; /* cfg statistics   */


/**** Global function prototypes ****/

void    FrontEnd(char *filename, PCALL_GRAPH *);            /* frontend.c   */
void   *allocMem(Int cb);                                   /* frontend.c   */
void   *allocVar(void *p, Int newsize);                     /* frontend.c   */
void    udm(void);                                          /* udm.c        */
PBB     createCFG(PPROC pProc);                             /* graph.c      */
void    compressCFG(PPROC pProc);                           /* graph.c      */
void    freeCFG(PBB cfg);                                   /* graph.c      */
PBB     newBB(PBB, Int, Int, byte, Int, PPROC);				/* graph.c		*/
void    BackEnd(char *filename, PCALL_GRAPH);               /* backend.c    */
char   *cChar(byte c);                                      /* backend.c    */
Int     scan(dword ip, PICODE p);                           /* scanner.c    */
void    parse (PCALL_GRAPH *);                              /* parser.c     */
boolT   labelSrch(PICODE pIc, Int n, dword tg, Int *pIdx);  /* parser.c     */
void    setState(PSTATE state, word reg, int16 value);      /* parser.c     */
Int		strSize (byte *, char);								/* parser.c		*/
void    disassem(Int pass, PPROC pProc);                    /* disassem.c   */
void    interactDis(PPROC initProc, Int initIC);            /* disassem.c   */
void	bindIcodeOff (PPROC);								/* idioms.c		*/
void    lowLevelAnalysis (PPROC pProc);                     /* idioms.c     */
void    propLong (PPROC pproc);								/* proplong.c   */
boolT   JmpInst(llIcode opcode);                            /* idioms.c     */
void    checkReducibility(PPROC pProc, derSeq **derG);      /* reducible.c  */
queue   *appendQueue(queue **Q, BB *node);                  /* reducible.c  */
void    freeDerivedSeq(derSeq *derivedG);                   /* reducible.c  */
void    displayDerivedSeq(derSeq *derG);                    /* reducible.c  */
void    structure(PPROC pProc, derSeq *derG);               /* control.c    */
void	compoundCond (PPROC);								/* control.c    */
void    dataFlow(PPROC pProc, dword liveOut);               /* dataflow.c   */
void    writeIntComment (PICODE icode, char *s);      		/* comwrite.c   */
void    writeProcComments (PPROC pProc, strTable *sTab);    /* comwrite.c   */
void    checkStartup(PSTATE pState);                        /* chklib.c     */
void    SetupLibCheck(void);                                /* chklib.c     */
void    CleanupLibCheck(void);                              /* chklib.c     */
boolT   LibCheck(PPROC p);                                  /* chklib.c     */

/* Exported functions from procs.c */
boolT	insertCallGraph (PCALL_GRAPH, PPROC, PPROC);
void	writeCallGraph (PCALL_GRAPH);
void 	newRegArg (PPROC, PICODE, PICODE);
boolT 	newStkArg (PICODE, COND_EXPR *, llIcode, PPROC);
void	allocStkArgs (PICODE, Int);
void	placeStkArg (PICODE, COND_EXPR *, Int);
void	adjustActArgType (COND_EXPR *, hlType, PPROC);		
void	adjustForArgType (PSTKFRAME, Int, hlType);

/* Exported functions from ast.c */
COND_EXPR *boolCondExp (COND_EXPR *lhs, COND_EXPR *rhs, condOp op);
COND_EXPR *unaryCondExp (condNodeType, COND_EXPR *exp);
COND_EXPR *idCondExpGlob (int16 segValue, int16 off);
COND_EXPR *idCondExpReg (byte regi, flags32 flg, LOCAL_ID *);
COND_EXPR *idCondExpRegIdx (Int idx, regType);
COND_EXPR *idCondExpLoc (Int off, LOCAL_ID *);
COND_EXPR *idCondExpParam (Int off, PSTKFRAME argSymtab);
COND_EXPR *idCondExpKte (dword kte, byte);
COND_EXPR *idCondExpLong (LOCAL_ID *, opLoc, PICODE, hlFirst, Int idx, operDu,
						  Int);
COND_EXPR *idCondExpLongIdx (Int);
COND_EXPR *idCondExpFunc (PPROC, PSTKFRAME);
COND_EXPR *idCondExpOther (byte seg, byte regi, int16 off);
COND_EXPR *idCondExpID (ID *, LOCAL_ID *, Int);
COND_EXPR *idCondExp (PICODE, opLoc, PPROC, Int i, PICODE duIcode, operDu);
COND_EXPR *copyCondExp (COND_EXPR *);
void	  removeRegFromLong (byte, LOCAL_ID *, COND_EXPR *);
char      *walkCondExpr (COND_EXPR *exp, PPROC pProc, Int *);
condId 	  idType (PICODE pIcode, opLoc sd);
Int		  hlTypeSize (COND_EXPR *, PPROC);
hlType	  expType (COND_EXPR *, PPROC);
void	  setRegDU (PICODE, byte regi, operDu);
void	  copyDU (PICODE, PICODE, operDu, operDu);
void	  changeBoolCondExpOp (COND_EXPR *, condOp);
boolT	  insertSubTreeReg (COND_EXPR *, COND_EXPR **, byte, LOCAL_ID *);
boolT	  insertSubTreeLongReg (COND_EXPR *, COND_EXPR **, Int);
void      freeCondExpr (COND_EXPR *exp);
COND_EXPR *concatExps (SEQ_COND_EXPR *, COND_EXPR *, condNodeType);
void	  initExpStk();
void	  pushExpStk (COND_EXPR *);
COND_EXPR *popExpStk();
Int		  numElemExpStk();
boolT	  emptyExpStk();

/* Exported functions from hlicode.c */
PICODE	newIcode (ICODE_REC *, PICODE);
void    newAsgnHlIcode (PICODE, COND_EXPR *, COND_EXPR *);
void	newCallHlIcode (PICODE);
void    newUnaryHlIcode (PICODE, hlIcode, COND_EXPR *);
void    newJCondHlIcode (PICODE, COND_EXPR *);
void	invalidateIcode (PICODE);
boolT	removeDefRegi (byte, PICODE, Int, LOCAL_ID *);
void	highLevelGen (PPROC);
char	*writeCall (PPROC, PSTKFRAME, PPROC, Int *);
char 	*write1HlIcode (struct _hl, PPROC, Int *);
char 	*writeJcond (struct _hl, PPROC, Int *);
char 	*writeJcondInv (struct _hl, PPROC, Int *);
Int		power2 (Int);
void	writeDU (PICODE, Int);
void	inverseCondOp (COND_EXPR **);

/* Exported funcions from locident.c */ 
Int newByteWordRegId (LOCAL_ID *, hlType t, byte regi);
Int newByteWordStkId (LOCAL_ID *, hlType t, Int off, byte regOff);
Int	newIntIdxId (LOCAL_ID *, int16 seg, int16 off, byte regi, Int, hlType);
Int newLongRegId (LOCAL_ID *, hlType t, byte regH, byte regL, Int idx);
Int newLongStkId (LOCAL_ID *, hlType t, Int offH, Int offL);
Int newLongId (LOCAL_ID *, opLoc sd, PICODE, hlFirst, Int idx, operDu, Int);
boolT checkLongEq (struct _longStkId, PICODE, Int, Int, PPROC, COND_EXPR **,
				   COND_EXPR **, Int);
boolT checkLongRegEq (struct _longId, PICODE, Int, Int, PPROC, COND_EXPR **,
					  COND_EXPR **, Int);
byte otherLongRegi (byte, Int, LOCAL_ID *);
void insertIdx (IDX_ARRAY *, Int);
void propLongId (LOCAL_ID *, byte, byte, char *);
