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
 *          dcc project disassembler
 * (C) Cristina Cifuentes, Mike van Emmerik, Jeff Ledermann
 ****************************************************************************/

#ifdef __MSDOS__ 
#include <curses.h>
#else
#ifdef __USLC__
#include <curses.h>
#else
#include <cursesX.h>
#endif 
#endif

#include "dcc.h"
#include "symtab.h"
#include <stdio.h>
#include <string.h>
#ifdef __MSDOS__
#include <alloc.h>
#else
#include <malloc.h>
#endif

#ifndef max
#define max(a,b)  (((a) > (b)) ? (a) : (b))
#endif

#define POS_LAB     15              /* Position of label */
#define POS_OPC     20              /* Position of opcode */
#define POS_OPR     25              /* Position of operand */
#define	WID_PTR		10				/* Width of the "xword ptr" lingo */
#define POS_OPR2    POS_OPR+WID_PTR /* Position of operand after "xword ptr" */
#define POS_CMT     54              /* Position of comment */


#define DELTA_ICODE 16              /* Number of icodes to realloc by each time */

static char *szOps[] =
{
"CBW",  "AAA",      "AAD",      "AAM",      "AAS",      "ADC",  "ADD",  "AND",
"BOUND","CALL",     "CALL",     "CLC",      "CLD",      "CLI",  "CMC",  "CMP",
"CMPS", "REPNE CMPS","REPE CMPS","DAA",     "DAS",      "DEC",  "DIV",  "ENTER",
"ESC",  "HLT",      "IDIV",     "IMUL",     "IN",       "INC",  "INS",  "REP INS",
"INT",  "IRET",     "JB",       "JBE",      "JAE",      "JA",   "JE",   "JNE",
"JL",   "JGE",      "JLE",      "JG",       "JS",       "JNS",  "JO",   "JNO",
"JP",   "JNP",      "JCXZ",     "JMP",      "JMP",      "LAHF", "LDS",  "LEA",
"LEAVE","LES",      "LOCK",     "LODS",     "REP LODS", "LOOP", "LOOPE","LOOPNE",
"MOV",  "MOVS",     "REP MOVS", "MUL",      "NEG",      "NOT",  "OR",   "OUT",
"OUTS", "REP OUTS", "POP",      "POPA",     "POPF",     "PUSH", "PUSHA","PUSHF",
"RCL",  "RCR",      "ROL",      "ROR",      "RET",      "RETF", "SAHF", "SAR",
"SHL",  "SHR",      "SBB",      "SCAS",     "REPNE SCAS","REPE SCAS",   "CWD",  "STC",
"STD",  "STI",      "STOS",     "REP STOS", "SUB",      "TEST", "WAIT", "XCHG",
"XLAT", "XOR",      "INTO",     "NOP",		"REPNE",	"REPE",	"MOD"
};

/* The following opcodes are for mod != 3 */
static char *szFlops1[] =
{
/* 0        1        2       3        4        5        6        7  */
"FADD",  "FMUL",  "FCOM", "FCOMP", "FSUB",  "FSUBR", "FDIV",  "FDIVR",  /* 00 */
"FLD",   "???",   "FST",  "???",   "FLDENV","FLDCW", "FSTENV","FSTSW",  /* 08 */
"FIADD", "FIMUL", "FICOM","FICOMP","FISUB", "FISUBR","FIDIV", "FIDIVR", /* 10 */
"FILD",  "???",   "FIST", "FISTP", "???",   "???",   "???",   "FSTP",   /* 18 */
"FADD",  "FMUL",  "FCOM", "FCOMP", "FSUB",  "FSUBR", "FDIV",  "FDIVR",  /* 20 */
"FLD",   "FLD",   "FST",  "FSTP",  "FRESTOR","???",  "FSAVE", "FSTSW",  /* 28 */
"FIADD", "FIMUL", "FICOM","FICOMP","FISUB", "FISUBR","FIDIV", "FIDIVR", /* 30 */
"FILD",  "???",   "FIST", "FISTP", "FBLD",  "???",   "FBSTP", "FISTP"   /* 38 */
};

/* The following opcodes are for mod == 3 */
static char *szFlops2[] =
{
/* 0        1        2       3        4        5        6        7  */
"FADD",  "FMUL",  "FCOM", "FCOMP", "FSUB",  "FSUBR", "FDIV",  "FDIVR",  /* 00 */
"FLD",   "FXCH",  "FNOP", "???",   "",      "",      "",      "",       /* 08 */
"FIADD", "FIMUL", "FICOM","FICOMP","FISUB", "",      "FIDIV", "FIDIVR", /* 10 */
"FILD",  "???",   "FIST", "FISTP", "???",   "???",   "???",   "FSTP",   /* 18 */
"FADD",  "FMUL",  "FCOM", "FCOMP", "FSUB",  "FSUBR", "FDIV",  "FDIVR",  /* 20 */
"FFREE", "FSTP",  "FST",  "???",   "FUCOM", "FUCOMP","???",   "???",    /* 28 */
"FADDP", "FMULP", "FICOM","",      "FSUBRP","FISUBR","FDIVRP","FDIVP",  /* 30 */
"FILD",  "???",   "FIST", "FISTP", "",      "???",   "FBSTP", "FISTP"   /* 38 */
};

static char *szFlops0C[] =
{
"FCHS",  "FABS",  "???",   "???",   "FTST", "FXAM",  "???",   "???"
};

static char *szFlops0D[] =
{
"FLD1",  "FLDL2T","FLDL2E","FLDP1", "FLDLG2","FLDLN2","FLDZ", "???"
};

static char *szFlops0E[] =
{
"F2XM1", "FYL2X", "FPTAN", "FPATAN","FXTRACT","FPREM1","FDECSTP","FINCSTP"
};

static char *szFlops0F[] =
{
"FPREM", "FYLXP1","FSQRT", "FSINCOS","FRNDINT","FSCALE","FSIN","FCOS"
};

static char *szFlops15[] =
{
"???",  "FUCOMPP",  "???", "???", "???", "???",  "???",   "???"
};

static char *szFlops1C[] =
{
"???",  "???",  "FCLEX", "FINIT", "FTST", "FXAM",  "???",   "???"
};

static char *szFlops33[] =
{
"???",  "FCOMPP",  "???", "???", "???", "???",  "???",   "???"
};

static char *szFlops3C[] =
{
"FSTSWAX","???",  "???", "???", "???", "???",  "???",   "???"
};


static char *szIndex[8] = {"bx+si", "bx+di", "bp+si", "bp+di", "si", "di","bp","bx" };
static char *szBreg[8]  = { "al", "cl", "dl", "bl", "ah", "ch", "dh", "bh" };
static char *szWreg[12] = { "ax", "cx", "dx", "bx", "sp", "bp", "si", "di",
                            "es", "cs", "ss", "ds" };
static char *szPtr[2]   = { " word ptr ", " byte ptr " };


static void  dis1Line  (Int i, boolT fWin, chtype attr, Int pass);
       void  dis1LineOp(Int i, boolT fWin, chtype attr, word *len, PPROC pProc);
static void  formatRM(char *p, flags32 flg, PMEM pm);
static char *strDst(flags32 flg, PMEM pm);
static char *strSrc(PICODE pc);
static char *strHex(dword d);
static Int   checkScanned(dword pcCur);
static void  setProc(PPROC proc);
static void  dispData(word dataSeg);
static void  flops(PICODE pi);
       boolT callArg(word off, char *temp);  /* Check for procedure name */

static  FILE   *fp;
static  PICODE  pc;
static  char    buf[200], *p;
static  Int     cb, j, numIcode, allocIcode, eop, *pl;
static  dword   nextInst;
static  boolT    fImpure;
static  Int     lab, prevPass;
static  PPROC   pProc;          /* Points to current proc struct */

typedef struct _POSSTACK
{
    Int     ic;                 /* An icode offset */
    PPROC   pProc;              /* A pointer to a PROC structure */
} POSSTACK;

static  POSSTACK *posStack;     /* Pointer to the position stack */
byte              iPS;          /* Index into the stack */

static  char    cbuf[256];      /* Has to be 256 for wgetstr() to work */


/*****************************************************************************
 * disassem - Prints a disassembled listing of a procedure.
 *			  pass == 1 generates output on file .a1
 *			  pass == 2 generates output on file .a2
 *			  pass == 3 generates output on file .b
 ****************************************************************************/
void disassem(Int pass, PPROC ppProc)
{
    Int         i;

    pProc = ppProc;             /* Save the passes pProc */
    if (pass != prevPass)
    {
        prevPass = pass;
        lab = 0; 	/* Restart label numbers */
    }
    createSymTables();
    allocIcode = numIcode = pProc->Icode.numIcode;
    if ((cb = allocIcode * sizeof(ICODE)) == 0)
    {
        return;  /* No Icode */
    }

    /* Open the output file (.a1 or .a2 only) */
	if (pass != 3)
	{
    	p = (pass == 1)? asm1_name: asm2_name;
    	if (! (fp = fopen(p, "a+")))
    	{
        	fatalError(CANNOT_OPEN, p);
    	}
	}

    /* Create temporary code array */
    pc = memcpy(allocMem(cb), pProc->Icode.icode, (size_t)cb);

    if (pass == 1)
    {
        /* Bind jump offsets to labels */
        for (i = 0; i < numIcode; i++)
        {
            if ((pc[i].ic.ll.flg & I) && !(pc[i].ic.ll.flg & JMP_ICODE) &&
                JmpInst(pc[i].ic.ll.opcode))
            {
                /* Replace the immediate operand with an icode index */
                if (labelSrch(pc, numIcode, pc[i].ic.ll.immed.op,
                    (Int *)&pc[i].ic.ll.immed.op))
                {
                    /* This icode is the target of a jump */
                    pc[pc[i].ic.ll.immed.op].ic.ll.flg |= TARGET;
                    pc[i].ic.ll.flg |= JMP_ICODE;   /* So its not done twice */
                }
                else
                {
                    /* This jump cannot be linked to a label */
                    pc[i].ic.ll.flg |= NO_LABEL;
                }
            }
        }
    }

    /* Create label array to keep track of location => label name */
    pl = memset(allocMem(numIcode * sizeof(Int)), 0,
        (size_t)(numIcode * sizeof(Int)));

    /* Write procedure header */
	if (pass != 3)
    	fprintf(fp, "\t\t%s  PROC  %s\n", pProc->name, 
									(pProc->flg & PROC_FAR)? "FAR": "NEAR");
    
    /* Loop over array printing each record */
    for (i = nextInst = 0; i < numIcode; i++)
    {
        dis1Line(i, FALSE, 0, pass);
    }

    /* Write procedure epilogue */
	if (pass != 3)
	{
    	fprintf(fp, "\n\t\t%s  ENDP\n\n", pProc->name);
    	fclose(fp);
	}

    free(pc);
    free(pl);
    destroySymTables();
}

/****************************************************************************
 * dis1Line() - disassemble one line to stream fp                           *                                   *
 * i is index into Icode for this proc                                      *
 * It is assumed that icode i is already scanned                            *
 ****************************************************************************/
static void
dis1Line(Int i, boolT fWindow, chtype attr, Int pass)
{
    PICODE pIcode = &pc[i];

	/* Disassembly stage 1 --
	 * Do not try to display NO_CODE entries or synthetic instructions,
	 * other than JMPs, that have been introduced for def/use analysis. */
    if ((option.asm1) && 
		((pIcode->ic.ll.flg & NO_CODE) ||
		 ((pIcode->ic.ll.flg & SYNTHETIC) && (pIcode->ic.ll.opcode != iJMP))))
	{
		return;
	}
	else if (pIcode->ic.ll.flg & NO_CODE)
    {
        return;
    }

    /* p points to the current position in buf[] */
    p = memset(buf, ' ', sizeof(buf));

	if (pIcode->ic.ll.flg & (TARGET | CASE))
    {
        if (fWindow)                        /* Printing to a window? */ 
            printw("\n");                   /* Yes, use the curses command */
        else if (pass == 3)					
			appendStrTab (&cCode.code, "\n"); /* No, print to c code buffer */
		else
            fprintf(fp, "\n");              /* No, print to the stream */ 
    } 

	/* Find next instruction label and print hex bytes */
	if (pIcode->ic.ll.flg & SYNTHETIC)
		nextInst = pIcode->ic.ll.label;
	else
	{
		cb = (dword) pIcode->ic.ll.numBytes;
		nextInst = pIcode->ic.ll.label + cb;

		/* Output hexa code in program image */
		if (pass != 3)
		{
        	for (j = 0; j < cb; j++, p += 2)
            	sprintf(p, "%02X", prog.Image[pIcode->ic.ll.label + j]);
        	*p = ' ';
		}
    }

    /* Check if there is a symbol here */
    selectTable(Label);
    if (readVal(&buf[POS_LAB], pIcode->ic.ll.label, 0))
    {
        buf[strlen(buf)] = ':';             /* Also removes the null */
    }

    else if (pIcode->ic.ll.flg & TARGET)    /* Symbols override Lnn labels */
    {   /* Print label */
        if (! pl[i])
        {
            pl[i] = ++lab;
        }
		if (pass == 3)
        	sprintf(buf, "L%ld", pl[i]);
		else	
        	sprintf(&buf[15], "L%ld", pl[i]);
        buf[strlen(buf)] = ':';             /* Also removes the null */
    }

    if (pIcode->ic.ll.opcode == iSIGNEX && (pIcode->ic.ll.flg & B))
    {
        pIcode->ic.ll.opcode = iCBW;
    }

	if (pass == 3)
	{
		strcpy (&buf[8], szOps[pIcode->ic.ll.opcode]);
		buf[eop = strlen(buf)] = ' ';
		p = buf + 8 + (POS_OPR - POS_OPC);
	}
	else
	{
    	strcpy(&buf[POS_OPC], szOps[pIcode->ic.ll.opcode]);
    	buf[eop = strlen(buf)] = ' ';
    	p = buf + POS_OPR;
	}

    switch (pIcode->ic.ll.opcode)
    {
    case iADD:  case iADC:  case iSUB:  case iSBB:  case iAND:  case iOR:
    case iXOR:  case iTEST: case iCMP:  case iMOV:  case iLEA:  case iXCHG:
        strcpy(p, strDst(pIcode->ic.ll.flg, &pIcode->ic.ll.dst));
        strcat(p, strSrc(pIcode));
        break;

    case iESC:
        flops(pIcode);
        break;

    case iSAR:  case iSHL:  case iSHR:  case iRCL:  case iRCR:  case iROL:
    case iROR:
        strcpy(p, strDst(pIcode->ic.ll.flg | I, &pIcode->ic.ll.dst));
        strcat(p, (pIcode->ic.ll.flg & I)? strSrc(pIcode): ", cl");
        break;

    case iINC:  case iDEC:  case iNEG:  case iNOT:  case iPOP:
        strcpy(p, strDst(pIcode->ic.ll.flg | I, &pIcode->ic.ll.dst));
        break;

    case iPUSH:
        if (pIcode->ic.ll.flg & I)
        {
            strcpy(p + WID_PTR, strHex(pIcode->ic.ll.immed.op));
        }
        else
        {
            strcpy(p, strDst(pIcode->ic.ll.flg | I, &pIcode->ic.ll.dst));
        }
        break;

    case iDIV:  case iIDIV:  case iMUL: case iIMUL: case iMOD:
        if (pIcode->ic.ll.flg & I)
        {
            strcat(strcpy(p, strDst(pIcode->ic.ll.flg, &pIcode->ic.ll.dst)),
					", ");
            formatRM(p + strlen(p), pIcode->ic.ll.flg, &pIcode->ic.ll.src);
            strcat(p, strSrc(pIcode));
        }
        else    
			strcpy(p, strDst(pIcode->ic.ll.flg | I, &pIcode->ic.ll.src));
        break;

    case iLDS:  case iLES:  case iBOUND:
        strcpy(p, strDst(pIcode->ic.ll.flg, &pIcode->ic.ll.dst));
        strcat(strcat(p, ", dword ptr"), strSrc(pIcode)+1);
        break;

    case iJB:  case iJBE:  case iJAE:  case iJA:
    case iJL:  case iJLE:  case iJGE:  case iJG: 
    case iJE:  case iJNE:  case iJS:   case iJNS:
    case iJO:  case iJNO:  case iJP:   case iJNP:
    case iJCXZ:case iLOOP: case iLOOPE:case iLOOPNE:
    case iJMP: case iJMPF:

        /* Check if there is a symbol here */
        selectTable(Label);
        if ((pIcode->ic.ll.immed.op < (dword)numIcode) &&  /* Ensure in range */
            readVal(p+WID_PTR, pc[pIcode->ic.ll.immed.op].ic.ll.label, 0))
        {
            break;                          /* Symbolic label. Done */
        }

        if (pIcode->ic.ll.flg & NO_LABEL)
        {
            strcpy(p + WID_PTR, strHex(pIcode->ic.ll.immed.op));
        }
        else if (pIcode->ic.ll.flg & I)
        {
            j = pIcode->ic.ll.immed.op;
            if (! pl[j])       /* Forward jump */
            {
                pl[j] = ++lab;
            }
            if (pIcode->ic.ll.opcode == iJMPF)
            {
                sprintf(p, " far ptr L%ld", pl[j]);
            }
            else
            {
                sprintf(p + WID_PTR, "L%ld", pl[j]);
            }
        }           
        else if (pIcode->ic.ll.opcode == iJMPF)
        {
            strcat(strcpy(p-1, "dword ptr"), strSrc(pIcode)+1);
        }
        else
        {
            strcpy(p, strDst(I, &pIcode->ic.ll.src));
        }
        break;

    case iCALL: case iCALLF:
        if (pIcode->ic.ll.flg & I)
        {
            sprintf(p, "%s ptr %s", 
                (pIcode->ic.ll.opcode == iCALL) ?" near":"  far",
                (pIcode->ic.ll.immed.proc.proc)->name);
        }
        else if (pIcode->ic.ll.opcode == iCALLF)
        {
            strcat(strcpy(p, "dword ptr"),strSrc(pIcode)+1);
        }
        else
        {
            strcpy(p, strDst(I, &pIcode->ic.ll.src));
        }
        break;

    case iENTER:
        strcat(strcpy(p + WID_PTR, strHex(pIcode->ic.ll.dst.off)), ", ");
        strcat(p, strHex(pIcode->ic.ll.immed.op));
        break;

    case iRET:  case iRETF:  case iINT:
        if (pIcode->ic.ll.flg & I)
        {
            strcpy(p + WID_PTR, strHex(pIcode->ic.ll.immed.op));
        }
        else
        {
            buf[eop] = '\0';
        }
        break;

    case iCMPS:  case iREPNE_CMPS:  case iREPE_CMPS:
    case iSCAS:  case iREPNE_SCAS:  case iREPE_SCAS:
    case iSTOS:  case iREP_STOS:
    case iLODS:  case iREP_LODS:
    case iMOVS:  case iREP_MOVS:
    case iINS:   case iREP_INS:
    case iOUTS:  case iREP_OUTS:
        if (pIcode->ic.ll.src.segOver)
        {
            (pIcode->ic.ll.opcode == iOUTS || pIcode->ic.ll.opcode == iREP_OUTS)
                ? strcat(strcpy(p+WID_PTR,"dx, "), szPtr[pIcode->ic.ll.flg & B])
                : strcpy(&buf[eop+1], szPtr[pIcode->ic.ll.flg & B]);
            if (pIcode->ic.ll.opcode == iLODS || 
				pIcode->ic.ll.opcode == iREP_LODS || 
				pIcode->ic.ll.opcode == iOUTS || 
				pIcode->ic.ll.opcode == iREP_OUTS)
            {
                strcat(p, szWreg[pIcode->ic.ll.src.segOver-rAX]);
            }
            else
            {
                strcat(strcat(p, "es:[di], "),
                    szWreg[pIcode->ic.ll.src.segOver - rAX]);
            }
            strcat(p, ":[si]");
        }
        else    strcpy(&buf[eop], (pIcode->ic.ll.flg & B)? "B": "W");
        break;

    case iXLAT:
        if (pIcode->ic.ll.src.segOver)
        {
            strcpy(&buf[eop+1], szPtr[1]);
            strcat(strcat(p, szWreg[pIcode->ic.ll.src.segOver-rAX]), ":[bx]");
        }
        else    buf[eop] = '\0';
        break;

    case iIN:
        strcpy(p+WID_PTR, (pIcode->ic.ll.flg & B)?"al, ": "ax, ");
        strcat(p+WID_PTR, (pIcode->ic.ll.flg & I)? strHex(pIcode->ic.ll.immed.op): "dx");
        break;

    case iOUT:
        strcpy(p+WID_PTR, (pIcode->ic.ll.flg & I)? strHex(pIcode->ic.ll.immed.op): "dx");
        strcat(p+WID_PTR, (pIcode->ic.ll.flg & B)?", al": ", ax");
        break;

    default:
        buf[eop] = '\0';
        break;
    }

    /* Comments */
    if (pIcode->ic.ll.flg & SYNTHETIC)
    {
        fImpure = FALSE;
    }
    else
    {
        for (j = pIcode->ic.ll.label, fImpure = 0; j > 0 && j < (Int)nextInst; 
             j++)
        {
            fImpure |= BITMAP(j, BM_DATA);
        }
    }


    /* Check for user supplied comment */
    selectTable(Comment);
    if (readVal(cbuf, pIcode->ic.ll.label, 0))
    {
        buf[strlen(buf)] = ' ';             /* Removes the null */
        buf[POS_CMT] = ';';
        strcpy(buf+POS_CMT+1, cbuf);
    }

    else if (fImpure || (pIcode->ic.ll.flg & (SWITCH | CASE | SEG_IMMED | 
                IMPURE | SYNTHETIC | TERMINATES)))
    {
        buf[strlen(buf)] = ' ';
        buf[POS_CMT] = '\0';
        if (pIcode->ic.ll.flg & CASE)
        {
            sprintf(buf+POS_CMT, ";Case l%ld", pIcode->ic.ll.caseTbl.numEntries);
        }
        if (pIcode->ic.ll.flg & SWITCH)
        {
            strcat(buf, ";Switch ");
        }
        if (fImpure)
        {
            strcat(buf, ";Accessed as data ");
        }
        if (pIcode->ic.ll.flg & IMPURE)
        {
            strcat(buf, ";Impure operand ");
        }
        if (pIcode->ic.ll.flg & SEG_IMMED)
        {
            strcat(buf, ";Segment constant");
        }
        if (pIcode->ic.ll.flg & TERMINATES)
        {
            strcat(buf, ";Exit to DOS");
        }
    }

	/* Comment on iINT icodes */
	if (pIcode->ic.ll.opcode == iINT)
		writeIntComment (pIcode, buf);	

	/* Display output line */
    if (! (pIcode->ic.ll.flg & SYNTHETIC))
    {
        if (fWindow)
        {
            word off;

            off = (word)(pIcode->ic.ll.label - ((dword)pProc->state.r[rCS] << 4));
            attrset(attr);
            printw(      "%04X %s\n", off, buf);
            attrset(A_NORMAL);
        }
        else if (pass == 3)		/* output to .b code buffer */ 
			appendStrTab (&cCode.code, "%s\n", buf);
		else					/* output to .a1 or .a2 file */
			fprintf (fp, "%03ld %06lX %s\n", i, pIcode->ic.ll.label, buf);
    }
	else		/* SYNTHETIC instruction */
	{
		strcat (buf, ";Synthetic inst");
        if (fWindow)
            printw(      "        %s\n", buf);
        else if (pass == 3)		/* output to .b code buffer */ 
			appendStrTab (&cCode.code, "%s\n", buf);
		else					/* output to .a1 or .a2 file */
			fprintf (fp, "%03ld        %s\n", i, buf);
	}
}



/****************************************************************************
 * formatRM
 ***************************************************************************/
static void formatRM(char *p, flags32 flg, PMEM pm)
{
    char    seg[4];

    if (pm->segOver)
    {
        strcat(strcpy(seg, szWreg[pm->segOver - rAX]), ":");
    }
    else    *seg = '\0';

    if (pm->regi == 0)
    {
        sprintf(p,"%s[%s]", seg, strHex((dword)pm->off));
    }

	else if (pm->regi == (INDEXBASE - 1))
	{
		strcpy (p, "tmp");
	}

    else if (pm->regi < INDEXBASE)
    {
        strcpy(p, (flg & B)? szBreg[pm->regi - rAL]: szWreg[pm->regi - rAX]);
    }
    
    else if (pm->off)
    {
        if (pm->off < 0)
        {
            sprintf(p,"%s[%s-%s]", seg, szIndex[pm->regi - INDEXBASE],
                     strHex((dword)(- pm->off)));
        }
        else
        {
            sprintf(p,"%s[%s+%s]", seg, szIndex[pm->regi - INDEXBASE],
                     strHex((dword)pm->off));
        }
    }
    else    sprintf(p,"%s[%s]", seg, szIndex[pm->regi - INDEXBASE]);
}


/*****************************************************************************
 * strDst
 ****************************************************************************/
static char *strDst(flags32 flg, PMEM pm)
{
    static char buf[30];

    /* Immediates to memory require size descriptor */
    if ((flg & I) && (pm->regi == 0 || pm->regi >= INDEXBASE))
    {
        memcpy(buf, szPtr[flg & B], WID_PTR);
    }
    else
    {
        memset(buf, ' ', WID_PTR);
    }

    formatRM(buf + WID_PTR, flg, pm);
    return buf;
}


/****************************************************************************
 * strSrc                                                                   *
 ****************************************************************************/
static char *strSrc(PICODE pc)
{
    static char buf[30] = {", "};

    if (pc->ic.ll.flg & I)
        strcpy(buf + 2, strHex(pc->ic.ll.immed.op));
	else if (pc->ic.ll.flg & IM_SRC)		/* level 2 */
		strcpy (buf + 2, "dx:ax");
    else
        formatRM(buf + 2, pc->ic.ll.flg, &pc->ic.ll.src);

    return buf;
}


/****************************************************************************
 * strHex                                                                   *
 ****************************************************************************/
static char *strHex(dword d)
{
    static char buf[10];

    d &= 0xFFFF;
    sprintf(buf, "0%lX%s", d, (d > 9)? "h": "");
    return (buf + (buf[1] <= '9'));
}




 /* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\

<           Interactive Disassembler and Associated Routines                >

 \* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */



dword   pcTop;                  /* Image offset of top line */
Int     icTop;                  /* Icode index  of top line */
dword   pcCur;                  /* Image offset of cursor */
Int     icCur;                  /* Icode index  of cursor */
dword   pcBot;                  /* Image offset of bottom line */
Int     icBot;                  /* Icode index  of bottom line */
dword   pcLast;                 /* Image offset of last instr in proc */
int     NSCROLL;                /* Number of limes to scroll. Pseudo constant */

/* Clear the screen, paint the title */
void clrTitle(void)
{
    erase();                    /* No need to use clear(), as much of the
                                    screen may not need repainting */
    attrset(A_BOLD);
    mvprintw(0, 0, "Proc %s at %06lX (%04X:%04X): %d bytes of parameters ",
        pProc->name, pProc->Icode.icode[0].ic.ll.label,
        pProc->state.r[rCS],
        (word)(pProc->Icode.icode[0].ic.ll.label - ((dword)(pProc->state.r[rCS]) << 4)),
        pProc->cbParam);
    if (pProc->flg & PROC_ISLIB) printw(" LIBRARY");
    attrset(A_NORMAL);
}


/****************************************************************************
*           updateScr - update the screen                                    *
 ****************************************************************************/
/* new is true if must recalculate the top line */
void
updateScr(boolT new)
{
    int y, x;
    Int i, ic;

    clrTitle();
    if (new || (pcCur > pcBot) || (pcCur < pcTop))
    {
        /* We need to redo the screen completely */
        icTop = icCur;
        for (x=0; x < NSCROLL; x++)
        {
            if (icTop && pc[icTop-1].ic.ll.label + 
                (dword)pc[icTop-1].ic.ll.numBytes == pc[icTop].ic.ll.label)
            {
                /* Then this instruction is contiguous with the current */
                icTop--;
            }
            else break;
        }
        pcTop = pc[icTop].ic.ll.label;

    }

    move(1, 0);
    nextInst = pcTop;
    for (y=1, ic=icTop; y < LINES-1; ic++)
    {
        if ((ic >= numIcode) || (nextInst != pc[ic].ic.ll.label))
        {
            if (labelSrch(pc, numIcode, nextInst, &i))
            {
                ic = i;
            }
            else 
            {
                pcLast = pc[ic-1].ic.ll.label;    /* Remember end of proc */
                break;                      /* Must be past last */
            }
        }

        /* Save pc of current line. Last assignment will be pc of bott line */
        pcBot = nextInst;
        icBot = ic;

        dis1Line(ic, TRUE, (pcCur == nextInst) ? A_REVERSE : A_NORMAL, 0);
        getyx(stdscr, y, x);


        if (ic == numIcode-1)
        {
            switch (pc[ic].ic.ll.opcode)
            {
                case iJMP:  case iJMPF:
                case iRET:  case iRETF:
                case iIRET:
                break;

                default:
                /* We have other than a break of control flow instruction
                    at the end of the proc. Parse more instructions to
                    complete the basic block
                */
                if ((ic = checkScanned(nextInst)) == -1)
                {
                    /* Some error. */
                    pcLast = pcCur;    /* Remember end of proc */
                    break;             /* Must be past last */
                }

            }
        }
    }
    refresh();
}

#if 0
/* An opcode based version of updateScr() */
/****************************************************************************
*           updateScrOp - update the screen                                  *
 ****************************************************************************/
/* new is true if must recalculate the top line */
void
updateScrOp(boolT new)
{
    int y, x;
    dword pc;
    word len;

    clrTitle();
    if (new || (pcCur > pcBot) || (pcCur < pcTop))
    {
        /* We need to redo the screen completely */
        pcTop = pcCur;
    }

    move(1, 0);
    for (y=1, pc = pcTop; y < LINES-1;)
    {
        /* Save pc of current line. Last assignment will be pc of bott line */
        pcBot = pc;

        dis1LineOp(pc, TRUE, (pcCur == pc) ? A_REVERSE : A_NORMAL, &len,
            pProc);
        pc += len;
        getyx(stdscr, y, x);
    }

    refresh();
}
 
#endif
 
void pushPosStack(void)
{
    /* Push the current position on the position stack */
    posStack[iPS].ic = icCur;
    posStack[iPS++].pProc = pProc;
}

void popPosStack(void)
{
    /* Push the current position on the position stack */
    /* Note: relies on the byte wraparound. Beware! */
    if (posStack[--iPS].pProc != (PPROC)-1)
    {
        if (posStack[iPS].pProc != pProc)
        {
            setProc(posStack[iPS].pProc);
        }
        icCur = posStack[iPS].ic;
        pcCur = pc[icCur].ic.ll.label;
    }
    else iPS++;                             /* Stack empty.. don't pop */
}


/* Check to see if there is an icode for given image offset.
    Scan it if necessary, adjusting the allocation of pc[] and pl[]
    if necessary. Returns -1 if an error, otherwise the icode offset
*/
static Int
checkScanned(dword pcCur)
{
    Int i;

    /* First we check if the current icode is in range */
    /* A sanity check first */
    if (pcCur >= (dword)prog.cbImage)
    {
        /* Couldn't be! */
        return -1;
    }

    if (!labelSrch(pc, numIcode, pcCur, &i))
    {
        /* This icode does not exist yet. Tack it on the end of the existing */
        if (numIcode >= allocIcode)
        {
            allocIcode = numIcode + DELTA_ICODE;      /* Make space for this one, and a few more */
            pc = allocVar(pc, allocIcode * sizeof(ICODE));
            /* It is important to clear the new icodes, to ensure that the type
                is set to NOT_SCANNED */
            memset(&pc[numIcode], 0, (size_t)(allocIcode-numIcode)*sizeof(ICODE));
            pl = allocVar(pl, allocIcode * sizeof(Int));
            memset(&pl[numIcode], 0, (size_t)(allocIcode-numIcode)*sizeof(Int));
        }
        i = numIcode++;
    }

    if (pc[i].type == NOT_SCANNED)
    {
        /* This is a new icode not even scanned yet. Scan it now */
        /* Ignore most errors... at this stage */
        if (scan(pcCur, &pc[i]) == IP_OUT_OF_RANGE)
        {
            /* Something went wrong... just forget it */
            return -1;
        }
    }

    return i;
}





/* Set up to use the procedure proc */
/* This includes some important initialisations, allocations, etc that are
    normally done in disassem() */
static void
setProc(PPROC proc)
{
    Int i;

    pProc = proc;                           /* Keep in a static */

    /* Free old arrays, if any */
    if (pc) free(pc);
    if (pl) free(pl);


    /* Create temporary code array */
    numIcode = pProc->Icode.numIcode;
    cb = numIcode * sizeof(ICODE);
    pc = memcpy(allocMem(cb), pProc->Icode.icode, (size_t)cb);

    /* Create label array to keep track of location => label name */
    pl = memset(allocMem(numIcode * sizeof(Int)), 0,
        (size_t)(numIcode * sizeof(Int)));
 
    /* Bind jump offsets to labels */
    for (i = 0; i < numIcode; i++)
    {
        if ((pc[i].ic.ll.flg & I) && !(pc[i].ic.ll.flg & JMP_ICODE) &&
            JmpInst(pc[i].ic.ll.opcode))
        {
            /* Immediate jump instructions. Make dest an icode index */
            if (labelSrch(pc, numIcode, pc[i].ic.ll.immed.op, 
                          (Int *)&pc[i].ic.ll.immed.op))
            {
                /* This icode is the target of a jump */
                pc[pc[i].ic.ll.immed.op].ic.ll.flg |= TARGET;
                pc[i].ic.ll.flg |= JMP_ICODE;   /* So its not done twice */
            }
            else
            {
                /* This jump cannot be linked to a label */
                pc[i].ic.ll.flg |= NO_LABEL;
            }
        }
    }

    /* Window initially scrolled with entry point on top */
    pcCur = pcTop = pProc->procEntry;
    labelSrch(pc, numIcode, pcCur, &icCur);
    /* pcLast is set properly in updateScr(), at least for now */
    pcLast = (dword)-1;

}

/****************************************************************************
 *          interactDis - interactive disassembler                          *
 ****************************************************************************/
void interactDis(PPROC initProc, Int initIC)
{

    boolT    fInteract;
    Int     i;
    int     ch;

    pProc = initProc;                       /* Keep copy of init proc */
    initscr();                              /* Initialise the curses system */
    keypad (stdscr, TRUE);                  /* Enable keypad */
    cbreak();                               /* Don't wait till newline */
    nodelay(stdscr, FALSE);                 /* getch() will wait for char */
    noecho();                               /* Don't echo keys */
    NSCROLL = max(3, LINES >> 3);           /* Number of lines to scroll */

    /* Allocate the position stack */
    posStack = allocVar(posStack, 256 * sizeof(POSSTACK));
    iPS = 0;
    memset(posStack, -1, 256 * sizeof(POSSTACK));

    /* Initially, work on the given proc */
    setProc(initProc);
    if (initIC)
    {
        icCur = initIC;
        pcCur = pc[icCur].ic.ll.label;
    }

    /* Initialise the symbol table */
    createSymTables();

    strcpy(cbuf, "label");                  /* Provide a default label string */

    updateScr(TRUE);

    fInteract = TRUE;
    while (fInteract)
    {
        switch (ch = getch())
        {
            case KEY_DOWN:
                if (pcCur >= pcLast) continue;  /* Ignore it */
                pcCur += pc[icCur].ic.ll.numBytes;
                labelSrch(pc, numIcode, pcCur, &icCur);
                if (pcCur >= pcBot)
                {
                    int j;

                    /* We have gone past the bottom line. Scroll a few lines */
                    for (j=0; j < NSCROLL; j++)
                    {
                        if (pcTop >= pcLast)
                        {
                            break;
                        }
                        pcTop += pc[icTop].ic.ll.numBytes;
                        if (labelSrch(pc, numIcode, pcTop, &i))
                            icTop = i;
                        else break;         /* Some problem... no more scroll */
                    }
                }
                updateScr(FALSE);
                break;

            case KEY_UP:
                /* First simply try the prev icode */
                if ((icCur == 0) ||
                    pc[--icCur].ic.ll.label + (dword)pc[icCur].ic.ll.numBytes != pcCur)
                {
                    for (i = 0; i < numIcode; i++)
                    {
                        if (pc[i].ic.ll.label + (dword)pc[i].ic.ll.numBytes == pcCur)
                        {
                            break;          /* This is the one! */
                        }
                    }
                    if (pc[i].ic.ll.label + pc[i].ic.ll.numBytes != pcCur)
                        break;              /* Not found. Sorry! */
                    icCur = i;
                }
                pcCur = pc[icCur].ic.ll.label;
                updateScr(FALSE);
                break;


#ifdef KEY_SRIGHT
            case KEY_SRIGHT:                /* Shift right */
#endif
            case '2':                       /* In case shift right not avail */
                /* As for right arrow, but considers source operand first */
                if  (pc[icCur].ic.ll.src.off != 0)
                {
                    pushPosStack();
                    pcCur = pc[icCur].ic.ll.src.off;
                    if (!labelSrch(pc, numIcode, pcCur, &icCur))
                        break;
                    updateScr(FALSE);
                }
                /* Fall through to KEY_RIGHT processing */

            case KEY_RIGHT:
                if (pc[icCur].ic.ll.flg & I)
                {
                    if ((pc[icCur].ic.ll.opcode >= iJB) &&
                        (pc[icCur].ic.ll.opcode <= iJMPF))
                    {
                        /* An immediate jump op. Jump to it */
                        pushPosStack();
                        if (pc[icCur].ic.ll.flg & JMP_ICODE)
                        {
                            /* immed.op is an icode offset */
                            icCur = pc[icCur].ic.ll.immed.op;
                            pcCur = pc[icCur].ic.ll.label;
                        }
                        else
                        {
                            /* immed.op is still an image offset.
                                Quite likely we need to scan */
                            pcCur = pc[icCur].ic.ll.immed.op;
                            if ((icCur = checkScanned(pcCur)) == -1)
                                break;
                        }
                    }
                    else if ((pc[icCur].ic.ll.opcode == iCALL) ||
                             (pc[icCur].ic.ll.opcode == iCALLF))
                    {
                        /* The dest is a pointer to a proc struct */

                        pushPosStack();
                        setProc((PPROC)pc[icCur].ic.ll.immed.op);
                    }
                    else
                    {
                        /* Other immediate */
                        pushPosStack();
                        pcCur = pc[icCur].ic.ll.immed.op;
                        dispData(pProc->state.r[rDS]);
                        break;
                    }
                }
                else if (pc[icCur].ic.ll.dst.off != 0)
                {
                    pushPosStack();
                    pcCur = pc[icCur].ic.ll.dst.off;
                    if (!labelSrch(pc, numIcode, pcCur, &icCur))
                    {
                        dispData(pProc->state.r[rDS]);
                        break;
                    }
                }
                else if (pc[icCur].ic.ll.src.off != 0)
                {
                    pushPosStack();
                    pcCur = pc[icCur].ic.ll.src.off;
                    if (!labelSrch(pc, numIcode, pcCur, &icCur))
                    {
                        dispData(pProc->state.r[rDS]);
                        break;
                    }
                }
                updateScr(TRUE);
                break;

            case KEY_LEFT:
                popPosStack();
                pcCur = pc[icCur].ic.ll.label;
                updateScr(TRUE);
                break;
                

            case KEY_NPAGE:
                pcCur = pcTop = pcBot;      /* Put bottom line at top now */
                icCur = icTop = icBot;
                updateScr(FALSE);
                break;

            case KEY_PPAGE:
                pcTop -= (LINES-2) * 2; /* Average of 2 bytes per inst */
                for (i = 0; i < numIcode; i++)
                {
                    if  ((pc[i].ic.ll.label <= pcTop) &&
                         (pc[i].ic.ll.label + (dword)pc[i].ic.ll.numBytes >= pcTop))
                    {
                        break;          /* This is the spot! */
                    }
                }
                if (i >= numIcode)
                {
                    /* Something went wrong. Goto to first icode */
                    i = 0;
                }
                icCur = icTop = i;
                pcCur = pcTop = pc[i].ic.ll.label;
                updateScr(FALSE);
                break;

            case 'l':                       /* Add a symbolic label here */
            {
                WINDOW *win;
                char    *pStr;

                /* Create a new window in the middle of the screen */
                win = newwin(5, 40, (LINES-5)>>1, (COLS-40)>>1);
                box(win, 0, 0);
                mvwprintw(win, 2, 3, "Enter symbol: ");
                /* Poorly documented: pdCurses re-uses old string! Huh! */
                cbuf[0] = '\0';             /* Remove prev string */
                /* Also poorly documented: 256 bytes will be wiped! */
                echo();
                wgetstr(win, cbuf);         /* Get a string to buf */
                noecho();
                delwin(win);                /* Delete the window */
                if (strlen(cbuf) >= SYMLEN)
                {
                    /* Name too ling. Truncate */
                    cbuf[SYMLEN-1] = '\0';
                }
                pStr = addStrTbl(cbuf);     /* Add to the string table */

                selectTable(Label);         /* Select the label table */
                /* Add the symbol to both value- and symbol- hashed tables */
                enterSym(pStr, pcCur, pProc, TRUE);

                if (icCur == 0)
                {
                    /* We are at the first icode of a function.
                        Assume it is the entry point, and rename the function */
                    strcpy(pProc->name, cbuf);
                }

                updateScr(FALSE);
                break;
            }

            case ';':
            {
                WINDOW *win;
                char    *pStr;
                word w;

                if (findVal(pcCur, 0, &w))
                {
                    readVal(cbuf, pcCur, 0);/* Make it the default string */
                    deleteVal(pcCur, 0, FALSE);
                }
                else
                {
                    cbuf[0] = '\0';             /* Remove prev string */
                }

                /* Enter a comment here, from a window */
                win = newwin(5, COLS, (LINES-5) >> 1, 0);
                box(win, 0, 0);
                mvwprintw(win, 2, 3, "Enter comment: ");
                /* Also poorly documented: 256 bytes will be wiped! */
                echo();
                wgetstr(win, cbuf);         /* Get a string to buf */
                noecho();
                delwin(win);                /* Delete the window */

                pStr = addStrTbl(cbuf);     /* Add to the string table */

                selectTable(Comment);
                enterSym(pStr, pcCur, pProc, FALSE);/* Add the symbol */

                updateScr(FALSE);
                break;
            }
                

            case 'A'-'@':
            case 'a':
            case 'A':
            case 'b':
            case 'B':
            case 'L':
            {
                WINDOW *win;

                /* Create a new window in the middle of the screen */
                win = newwin(5, 40, (LINES-5)>>1, (COLS-40)>>1);
                box(win, 0, 0);
                wattrset(win, A_BOLD);      /* Use bold characters */
                mvwprintw(win, 2, 3, "You just pressed %c!", ch);
                wgetstr(win, cbuf);
                delwin(win);
                updateScr(FALSE);
                break;
            }

            case 'X' & 0x1F:                /* Control X; can't use Alt with Unix */
                fInteract = FALSE;          /* Exit interactive mode */
                break;
        }
    }

    clear();
    refresh();                              /* Remove interactive stuff */
    endwin();                               /* Restore TTY status */

    free(posStack);
    destroySymTables();
}


/****************************************************************************
 *          Display the current image position as data                      *
 ****************************************************************************/
static void
dispData(word dataSeg)
{
    int y, c, i, x;
    Int pc, pcStart;
    Int off = (Int)dataSeg << 4;

    if (pcCur >= (dword)prog.cbImage)
    {
        /* We're at an invalid address. Use 0x100 instead */
        pcCur = 0;
    }
    clrTitle();

    pcStart = pc = pcCur;           /* pc at start of line */
    for (y=1; y < LINES-1;)
    {
        mvprintw(y, 1, "%04lX ", pc);
        for (i=0; i < 16; i++)
        {
            printw("%02X ", prog.Image[pc++ + off]);
            if ((pc + off) > prog.cbImage) break;
        }
        pc = pcStart;
        for (i=0; i < 16; i++)
        {
            c = prog.Image[pc++ + off];
            if ((c < 0x20) || (c > 0x7E))
            {
                c = '.';
            }
            printw("%c", c);
            if ((pc + off) > prog.cbImage) break;
        }
        printw("\n");
        pcStart = pc;

        if ((pc + off) > prog.cbImage) break;

        getyx(stdscr, y, x);
    }

    refresh();

}


boolT callArg(word off, char *sym)
{
    dword   imageOff;
    PPROC   p, pPrev;

    imageOff = off + ((dword)pProc->state.r[rCS] << 4);
    /* Search procedure list for one with appropriate entry point */
    for (p = pProcList; p && p->procEntry != imageOff; p = p->next)
    {
        pPrev = p;
    }

    if (p == 0)
    {
        /* No existing proc entry */
        LibCheck(p);
        if (p->flg & PROC_ISLIB)
        {
            /* No entry for this proc, but it is a library function.
                Create an entry for it */
            p = memset(allocStruc(PROC), 0, sizeof(PROC));
            pPrev->next = p;
            p->procEntry = imageOff;
        }
    }

    if (p)
    {
        /* We have a proc entry for this procedure. Copy the name */
        strcpy(sym, p->name);
        return TRUE;
    }

    return FALSE;

}

/* Handle the floating point opcodes (icode iESC) */
static void flops(PICODE pIcode)
{
    char bf[30];
    byte op = (byte)pIcode->ic.ll.immed.op;

    /* Note that op is set to the escape number, e.g.
        esc 0x38 is FILD */

    if ((pIcode->ic.ll.dst.regi == 0) || (pIcode->ic.ll.dst.regi >= INDEXBASE))
    {
        /* The mod/rm mod bits are not set to 11 (i.e. register).
           This is the normal floating point opcode */
        strcpy(&buf[POS_OPC], szFlops1[op]);
        buf[strlen(buf)] = ' ';

        if ((op == 0x29) || (op == 0x1F))
        {
            strcpy(bf, "tbyte ptr ");
        }
        else switch (op & 0x30)
        {
            case 0x00:
            case 0x10:
                strcpy(bf, "dword ptr ");
                break;
            case 0x20:
                strcpy(bf, "qword ptr ");
                break;
            case 0x30:
                switch (op)
                {
                    case 0x3C:       /* FBLD */
                    case 0x3E:       /* FBSTP */
                        strcpy(bf, "tbyte ptr ");
                        break;
                    case 0x3D:       /* FILD 64 bit */
                    case 0x3F:       /* FISTP 64 bit */
                        strcpy(bf, "qword ptr ");
                        break;

                    default:
                        strcpy(bf, "word  ptr ");
                        break;
                }
        }

        formatRM(bf + 10, pIcode->ic.ll.flg, &pIcode->ic.ll.dst);
        strcpy(p, bf);
    }
    else
    {
        /* The mod/rm mod bits are set to 11 (i.e. register).
           Could be specials (0x0C-0x0F, etc), or the st(i) versions of
		   normal opcodes. Because the opcodes are slightly different for
		   this case (e.g. op=04 means FSUB if reg != 3, but FSUBR for
		   reg == 3), a separate table is used (szFlops2). */
        switch (op)
        {
            case 0x0C:
                strcpy(&buf[POS_OPC], szFlops0C[pIcode->ic.ll.dst.regi - rAX]);
                break;
            case 0x0D:
                strcpy(&buf[POS_OPC], szFlops0D[pIcode->ic.ll.dst.regi - rAX]);
                break;
            case 0x0E:
                strcpy(&buf[POS_OPC], szFlops0E[pIcode->ic.ll.dst.regi - rAX]);
                break;
            case 0x0F:
                strcpy(&buf[POS_OPC], szFlops0F[pIcode->ic.ll.dst.regi - rAX]);
                break;
            case 0x15:
                strcpy(&buf[POS_OPC], szFlops15[pIcode->ic.ll.dst.regi - rAX]);
                break;
            case 0x1C:
                strcpy(&buf[POS_OPC], szFlops1C[pIcode->ic.ll.dst.regi - rAX]);
                break;
            case 0x33:
                strcpy(&buf[POS_OPC], szFlops33[pIcode->ic.ll.dst.regi - rAX]);
                break;
            case 0x3C:
                strcpy(&buf[POS_OPC], szFlops3C[pIcode->ic.ll.dst.regi - rAX]);
                break;
			default:
        		strcpy(&buf[POS_OPC], szFlops2[op]);
        		buf[strlen(buf)] = ' ';
				if ((op >= 0x20) && (op <= 0x27))
				{
					/* This is the ST(i), ST form. */
					sprintf(&buf[POS_OPR2], "ST(%d),ST", pIcode->ic.ll.dst.regi - rAX);
				}
				else 
				{
					/* ST, ST(i) */
					sprintf(&buf[POS_OPR2], "ST,ST(%d)", pIcode->ic.ll.dst.regi - rAX);
				}

				break;
        }
    }
}


