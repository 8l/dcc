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
 * File:    hlIcode.c
 * Purpose: High-level icode routines
 * Date:    September-October 1993
 * (C) Cristina Cifuentes
 */

#include <string.h>
#include <malloc.h>
#include "dcc.h"

#define ICODE_DELTA 25;

/* Masks off bits set by duReg[] */
dword maskDuReg[] = { 0x00,
            0xFEEFFE, 0xFDDFFD, 0xFBB00B, 0xF77007, /* word regs */
            0xFFFFEF, 0xFFFFDF, 0xFFFFBF, 0xFFFF7F,
            0xFFFEFF, 0xFFFDFF, 0xFFFBFF, 0xFFF7FF, /* seg regs  */
            0xFFEFFF, 0xFFDFFF, 0xFFBFFF, 0xFF7FFF, /* byte regs */
            0xFEFFFF, 0xFDFFFF, 0xFBFFFF, 0xF7FFFF,
            0xEFFFFF,                               /* tmp reg   */
            0xFFFFB7, 0xFFFF77, 0xFFFF9F, 0xFFFF5F, /* index regs */
            0xFFFFBF, 0xFFFF7F, 0xFFFFDF, 0xFFFFF7 };

static char buf[lineSize];     /* Line buffer for hl icode output */


PICODE newIcode (ICODE_REC *icode, PICODE pIcode)
/* Copies the icode that is pointed to by pIcode to the icode array.
 * If there is need to allocate extra memory, it is done so, and
 * the icode.alloc variable is adjusted.        */
{ PICODE resIcode;

    if (icode->numIcode == icode->alloc)
    {
        icode->alloc += ICODE_DELTA;
        icode->icode = allocVar(icode->icode, icode->alloc * sizeof(ICODE));
    }
    resIcode = (PICODE)memcpy (&icode->icode[icode->numIcode], pIcode, 
                               sizeof(ICODE));
    icode->numIcode++;
    return (resIcode);
}


void newAsgnHlIcode (PICODE pIcode, COND_EXPR *lhs, COND_EXPR *rhs)
/* Places the new ASSIGN high-level operand in the high-level icode array */
{
    pIcode->type = HIGH_LEVEL;
    pIcode->ic.hl.opcode = ASSIGN;
    pIcode->ic.hl.oper.asgn.lhs = lhs;
    pIcode->ic.hl.oper.asgn.rhs = rhs;
}


void newCallHlIcode (PICODE pIcode)
/* Places the new CALL high-level operand in the high-level icode array */
{
    pIcode->type = HIGH_LEVEL;
    pIcode->ic.hl.opcode = CALL;
    pIcode->ic.hl.oper.call.proc = pIcode->ic.ll.immed.proc.proc;
	pIcode->ic.hl.oper.call.args = allocMem (sizeof(STKFRAME));
	memset (pIcode->ic.hl.oper.call.args, 0, sizeof(STKFRAME));
	if (pIcode->ic.ll.immed.proc.cb != 0)
		pIcode->ic.hl.oper.call.args->cb = pIcode->ic.ll.immed.proc.cb;
	else
		pIcode->ic.hl.oper.call.args->cb =pIcode->ic.hl.oper.call.proc->cbParam;
}


void newUnaryHlIcode (PICODE pIcode, hlIcode op, COND_EXPR *exp)
/* Places the new POP/PUSH/RET high-level operand in the high-level icode 
 * array */
{
    pIcode->type = HIGH_LEVEL;
    pIcode->ic.hl.opcode = op;
    pIcode->ic.hl.oper.exp = exp;
}


void newJCondHlIcode (PICODE pIcode, COND_EXPR *cexp)
/* Places the new JCOND high-level operand in the high-level icode array */
{
    pIcode->type = HIGH_LEVEL;
    pIcode->ic.hl.opcode = JCOND;
    pIcode->ic.hl.oper.exp = cexp;
}


void invalidateIcode (PICODE pIcode)
/* Sets the invalid field to TRUE as this low-level icode is no longer valid,
 * it has been replaced by a high-level icode. */
{
    pIcode->invalid = TRUE;
}


boolT removeDefRegi (byte regi, PICODE picode, Int thisDefIdx, LOCAL_ID *locId)
/* Removes the defined register regi from the lhs subtree.  If all registers
 * of this instruction are unused, the instruction is invalidated (ie.
 * removed) */
{ Int numDefs;

    numDefs = picode->du1.numRegsDef;
    if (numDefs == thisDefIdx)
        for ( ; numDefs > 0; numDefs--)
        {
            if ((picode->du1.idx[numDefs-1][0] != 0)||(picode->du.lastDefRegi)) 
                break;
        }

    if (numDefs == 0)
    {
        invalidateIcode (picode);
        return (TRUE);
    }
    else
    {
        switch (picode->ic.hl.opcode) {
        case ASSIGN:    removeRegFromLong (regi, locId, 
                                                picode->ic.hl.oper.asgn.lhs);
                        picode->du1.numRegsDef--;
                        picode->du.def &= maskDuReg[regi];
                        break;
        case POP:
        case PUSH:      removeRegFromLong (regi, locId, picode->ic.hl.oper.exp);
                        picode->du1.numRegsDef--;
                        picode->du.def &= maskDuReg[regi];
                        break;
        }
        return (FALSE);
    }
}


void highLevelGen (PPROC pProc)
/* Translates LOW_LEVEL icodes to HIGH_LEVEL icodes - 1st stage.
 * Note: this process should be done before data flow analysis, which
 *       refines the HIGH_LEVEL icodes. */
{ Int i,                /* idx into icode array */
      idx,
      numIcode;         /* number of icode instructions */ 
  PICODE pIcode;        /* ptr to current icode node */
  COND_EXPR *lhs, *rhs; /* left- and right-hand side of expression */
  flags32 flg;          /* icode flags */

    numIcode = pProc->Icode.numIcode;
    for (i = 0; i < numIcode; i++)
    {
        pIcode = &pProc->Icode.icode[i];
        if ((pIcode->ic.ll.flg & NOT_HLL) == NOT_HLL)
            invalidateIcode (pIcode);
        if ((pIcode->type == LOW_LEVEL) && (pIcode->invalid == FALSE))
        {
            flg = pIcode->ic.ll.flg;
            if ((flg & IM_OPS) != IM_OPS)   /* not processing IM_OPS yet */
                if ((flg & NO_OPS) != NO_OPS)       /* if there are opers */
                {
                    if ((flg & NO_SRC) != NO_SRC)   /* if there is src op */
                        rhs = idCondExp (pIcode, SRC, pProc, i, pIcode, NONE);
                    lhs = idCondExp (pIcode, DST, pProc, i, pIcode, NONE);
                }

            switch (pIcode->ic.ll.opcode) {
              case iADD:    rhs = boolCondExp (lhs, rhs, ADD);
                            newAsgnHlIcode (pIcode, lhs, rhs);
                            break;

              case iAND:    rhs = boolCondExp (lhs, rhs, AND);
                            newAsgnHlIcode (pIcode, lhs, rhs);
                            break;

              case iCALL:
              case iCALLF:  newCallHlIcode (pIcode);
                            break;

              case iDEC:    rhs = idCondExpKte (1, 2);
							rhs = boolCondExp (lhs, rhs, SUB);
							newAsgnHlIcode (pIcode, lhs, rhs); 
                            break;

              case iDIV:    
              case iIDIV:/* should be signed div */
                            rhs = boolCondExp (lhs, rhs, DIV);  
                            if (pIcode->ic.ll.flg & B)
                            {
                                lhs = idCondExpReg (rAL, 0, &pProc->localId);
                                setRegDU (pIcode, rAL, DEF);
                            }
                            else
                            {
                                lhs = idCondExpReg (rAX, 0, &pProc->localId);
                                setRegDU (pIcode, rAX, DEF);
                            }
                            newAsgnHlIcode (pIcode, lhs, rhs);
                            break;

              case iIMUL:   rhs = boolCondExp (lhs, rhs, MUL);
                            lhs = idCondExp (pIcode, LHS_OP, pProc, i, pIcode,
                                             NONE);
                            newAsgnHlIcode (pIcode, lhs, rhs);
                            break;

              case iINC:    rhs = idCondExpKte (1, 2);
							rhs = boolCondExp (lhs, rhs, ADD);
							newAsgnHlIcode (pIcode, lhs, rhs);
                            break;

              case iLEA:    rhs = unaryCondExp (ADDRESSOF, rhs);
                            newAsgnHlIcode (pIcode, lhs, rhs);
                            break;

              case iMOD:    rhs = boolCondExp (lhs, rhs, MOD);
                            if (pIcode->ic.ll.flg & B)
                            {
                                lhs = idCondExpReg (rAH, 0, &pProc->localId);
                                setRegDU (pIcode, rAH, DEF);
                            }
                            else
                            {
                                lhs = idCondExpReg (rDX, 0, &pProc->localId);
                                setRegDU (pIcode, rDX, DEF);
                            }
                            newAsgnHlIcode (pIcode, lhs, rhs);
                            break;

              case iMOV:    newAsgnHlIcode (pIcode, lhs, rhs);
                            break;

              case iMUL:    rhs = boolCondExp (lhs, rhs, MUL);
                            lhs = idCondExp (pIcode, LHS_OP, pProc, i, pIcode,
                                             NONE);
                            newAsgnHlIcode (pIcode, lhs, rhs);
                            break;

              case iNEG:    rhs = unaryCondExp (NEGATION, lhs);
                            newAsgnHlIcode (pIcode, lhs, rhs);
                            break;

			  case iNOT:	rhs = boolCondExp (NULL, rhs, NOT);
							newAsgnHlIcode (pIcode, lhs, rhs);
							break;

              case iOR:     rhs = boolCondExp (lhs, rhs, OR);
                            newAsgnHlIcode (pIcode, lhs, rhs);
                            break;

              case iPOP:    newUnaryHlIcode (pIcode, POP, lhs);
                            break;

              case iPUSH:   newUnaryHlIcode (pIcode, PUSH, lhs);
                            break;

              case iRET:    
              case iRETF:   newUnaryHlIcode (pIcode, RET, NULL);
                            break;

              case iSHL:    rhs = boolCondExp (lhs, rhs, SHL);
                            newAsgnHlIcode (pIcode, lhs, rhs);
                            break;

              case iSAR:    /* signed */
              case iSHR:    rhs = boolCondExp (lhs, rhs, SHR); /* unsigned*/
                            newAsgnHlIcode (pIcode, lhs, rhs);
                            break;

              case iSIGNEX: newAsgnHlIcode (pIcode, lhs, rhs);  
                            break;

              case iSUB:    rhs = boolCondExp (lhs, rhs, SUB);
                            newAsgnHlIcode (pIcode, lhs, rhs);
                            break;

              case iXCHG:   
                            break;

              case iXOR:    rhs = boolCondExp (lhs, rhs, XOR);
                            newAsgnHlIcode (pIcode, lhs, rhs);
                            break;
            }
        }

    }

}


void inverseCondOp (COND_EXPR **exp)
/* Modifies the given conditional operator to its inverse.  This is used
 * in if..then[..else] statements, to reflect the condition that takes the
 * then part. 	*/
{ 
  static condOp invCondOp[] = {GREATER, GREATER_EQUAL, NOT_EQUAL, EQUAL,
							   LESS_EQUAL, LESS, DUMMY,DUMMY,DUMMY,DUMMY,
							   DUMMY, DUMMY, DUMMY, DUMMY, DUMMY, DUMMY, 
							   DUMMY, DBL_OR, DBL_AND};
	if (*exp == NULL) return;

	if ((*exp)->type == BOOLEAN)
	{
		switch ((*exp)->expr.boolExpr.op) {
		  case LESS_EQUAL: case LESS: case EQUAL:
		  case NOT_EQUAL: case GREATER: case GREATER_EQUAL:
			(*exp)->expr.boolExpr.op = invCondOp[(*exp)->expr.boolExpr.op];
			break;

		  case AND: case OR: case XOR: case NOT: case ADD:
		  case SUB: case MUL: case DIV: case SHR: case SHL: case MOD:
			*exp = unaryCondExp (NEGATION, *exp);
			break;

		  case DBL_AND: case DBL_OR:
			(*exp)->expr.boolExpr.op = invCondOp[(*exp)->expr.boolExpr.op];
			inverseCondOp (&(*exp)->expr.boolExpr.lhs);
			inverseCondOp (&(*exp)->expr.boolExpr.rhs);
			break;
		} /* eos */

	}
	else if ((*exp)->type == NEGATION)
		*exp = (*exp)->expr.unaryExp;

	/* other types are left unmodified */
}


char *writeCall (PPROC tproc, PSTKFRAME args, PPROC pproc, Int *numLoc)
/* Returns the string that represents the procedure call of tproc (ie. with
 * actual parameters) */
{ Int i;                        /* counter of # arguments       */
  char *s, *condExp;

	s = allocMem (100 * sizeof(char));
	s[0] = '\0';
	sprintf (s, "%s (", tproc->name);
    for (i = 0; i < args->csym; i++)
    {
        condExp = walkCondExpr (args->sym[i].actual, pproc, numLoc);
        strcat (s, condExp);
        if (i < (args->csym - 1))
            strcat (s, ", ");
    }
    strcat (s, ")"); 
	return (s);
}


char *writeJcond (struct _hl h, PPROC pProc, Int *numLoc)
/* Displays the output of a JCOND icode. */
{ char *e;

    memset (buf, ' ', sizeof(buf));
	buf[0] = '\0';
    strcat (buf, "if ");
	inverseCondOp (&h.oper.exp);
    e = walkCondExpr (h.oper.exp, pProc, numLoc);
    strcat (buf, e);
    strcat (buf, " {\n");
	return (buf);
}


char *writeJcondInv (struct _hl h, PPROC pProc, Int *numLoc)
/* Displays the inverse output of a JCOND icode.  This is used in the case
 * when the THEN clause of an if..then..else is empty.  The clause is
 * negated and the ELSE clause is used instead.	*/
{ char *e;

    memset (buf, ' ', sizeof(buf));
	buf[0] = '\0';
    strcat (buf, "if ");
    e = walkCondExpr (h.oper.exp, pProc, numLoc);
    strcat (buf, e);
    strcat (buf, " {\n");
	return (buf);
}


char *write1HlIcode (struct _hl h, PPROC pProc, Int *numLoc)
/* Returns a string with the contents of the current high-level icode.
 * Note: this routine does not output the contens of JCOND icodes.  This is
 * 		 done in a separate routine to be able to support the removal of
 *		 empty THEN clauses on an if..then..else.	*/
{ char *e;

    memset (buf, ' ', sizeof(buf));
	buf[0] = '\0';
    switch (h.opcode) {
      case ASSIGN:  e = walkCondExpr (h.oper.asgn.lhs, pProc, numLoc);
                    strcat (buf, e);
                    strcat (buf, " = ");
                    e = walkCondExpr (h.oper.asgn.rhs, pProc, numLoc);
                    strcat (buf, e);
                    strcat (buf, ";\n");
                    break;
      case CALL:    e = writeCall (h.oper.call.proc, h.oper.call.args, pProc,
								   numLoc);
                    strcat (buf, e);
                    strcat (buf, ";\n");
                    break;
      case RET:     e = walkCondExpr (h.oper.exp, pProc, numLoc); 
					if (e[0] != '\0')
					{
	  					strcat (buf, "return (");
                    	strcat (buf, e);
                    	strcat (buf, ");\n");
					}
                    break;
      case POP:     strcat (buf, "POP ");
                    e = walkCondExpr (h.oper.exp, pProc, numLoc);
                    strcat (buf, e);
                    strcat (buf, "\n");
                    break;
      case PUSH:    strcat (buf, "PUSH ");
                    e = walkCondExpr (h.oper.exp, pProc, numLoc);
                    strcat (buf, e);
                    strcat (buf, "\n");
                    break;
    }
    return (buf);
}


Int power2 (Int i)
/* Returns the value of 2 to the power of i */
{
    if (i == 0)
        return (1);
    return (2 << (i-1));
}


void writeDU (PICODE pIcode, Int idx)
/* Writes the registers/stack variables that are used and defined by this
 * instruction. */
{ static char buf[100];
  Int i, j;

    memset (buf, ' ', sizeof(buf));
    buf[0] = '\0';
    for (i = 0; i < (INDEXBASE-1); i++)
    {
        if ((pIcode->du.def & power2(i)) != 0)
        {
            strcat (buf, allRegs[i]);
            strcat (buf, " ");
        }
    }
    if (buf[0] != '\0')
        printf ("Def (reg) = %s\n", buf);

    memset (buf, ' ', sizeof(buf));
    buf[0] = '\0';
    for (i = 0; i < INDEXBASE; i++)
    {
        if ((pIcode->du.use & power2(i)) != 0)
        {
            strcat (buf, allRegs[i]);
            strcat (buf, " ");
        }
    }
    if (buf[0] != '\0')
        printf ("Use (reg) = %s\n", buf);

    /* Print du1 chain */
    printf ("# regs defined = %d\n", pIcode->du1.numRegsDef);
    for (i = 0; i < MAX_REGS_DEF; i++)
        if (pIcode->du1.idx[i][0] != 0)
        {
            printf ("%d: du1[%d][] = ", idx, i);
            for (j = 0; j < MAX_USES; j++)
            {
                if (pIcode->du1.idx[i][j] == 0)
                    break;
                printf ("%d ", pIcode->du1.idx[i][j]);
            }
            printf ("\n");
        }
    
    /* For CALL, print # parameter bytes */
    if (pIcode->ic.hl.opcode == CALL)
        printf ("# param bytes = %d\n", pIcode->ic.hl.oper.call.args->cb);
    printf ("\n");
}


void freeHlIcode (PICODE icode, Int numIcodes)
/* Frees the storage allocated to h->hlIcode */
{ Int i;
  struct _hl h;

    for (i = 0; i < numIcodes; i++)
    {
        h = icode[i].ic.hl;
        switch (h.opcode) {
          case ASSIGN:  freeCondExpr (h.oper.asgn.lhs);
                        freeCondExpr (h.oper.asgn.rhs);
                        break;
          case POP: case PUSH:  
          case JCOND:   freeCondExpr (h.oper.exp);
                        break;
        }
    }
}

