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
 * File:    procs.c
 * Purpose: Functions to support Call graphs and procedures
 * Date:    November 1993
 * (C) Cristina Cifuentes
 */

#include "dcc.h"
#include <string.h>


/* Static indentation buffer */
#define indSize     61          /* size of indentation buffer; max 20 */
static char indentBuf[indSize] = 
    "                                                            ";

static char *indent (Int indLevel)
/* Indentation according to the depth of the statement */
{ 
    return (&indentBuf[indSize-(indLevel*3)-1]); 
}


static void insertArc (PCALL_GRAPH pcallGraph, PPROC newProc)
/* Inserts an outEdge at the current callGraph pointer if the newProc does 
 * not exist.  */
{ CALL_GRAPH *pcg;
  Int i;

    /* Check if procedure already exists */
    for (i = 0;  i < pcallGraph->numOutEdges; i++)
        if (pcallGraph->outEdges[i]->proc == newProc)
            return;

    /* Check if need to allocate more space */
    if (pcallGraph->numOutEdges == pcallGraph->numAlloc)
    {
        pcallGraph->numAlloc += NUM_PROCS_DELTA;
        pcallGraph->outEdges = allocVar(pcallGraph->outEdges,
                                    pcallGraph->numAlloc * sizeof(PCALL_GRAPH));
    }

    /* Include new arc */
    pcg = allocStruc(CALL_GRAPH); 
    memset (pcg, 0, sizeof(CALL_GRAPH));
    pcg->proc = newProc;
    pcallGraph->outEdges[pcallGraph->numOutEdges] = pcg;
    pcallGraph->numOutEdges++;
}


boolT insertCallGraph (PCALL_GRAPH pcallGraph, PPROC caller, PPROC callee)
/* Inserts a (caller, callee) arc in the call graph tree. */
{ Int i;

    if (pcallGraph->proc == caller)
    {
        insertArc (pcallGraph, callee);
        return (TRUE);
    }
    else
    {
        for (i = 0; i < pcallGraph->numOutEdges; i++)
            if (insertCallGraph (pcallGraph->outEdges[i], caller, callee))
                return (TRUE);
        return (FALSE);
    }
}


static void writeNodeCallGraph (PCALL_GRAPH pcallGraph, Int indIdx) 
/* Displays the current node of the call graph, and invokes recursively on
 * the nodes the procedure invokes. */
{ Int i;

    printf ("%s%s\n", indent(indIdx), pcallGraph->proc->name);
    for (i = 0; i < pcallGraph->numOutEdges; i++)
        writeNodeCallGraph (pcallGraph->outEdges[i], indIdx + 1);
}


void writeCallGraph (PCALL_GRAPH pcallGraph)
/* Writes the header and invokes recursive procedure */
{ 
    printf ("\n\nCall Graph:\n");
    writeNodeCallGraph (pcallGraph, 0); 
}


/**************************************************************************
 *  Routines to support arguments 
 *************************************************************************/

void newRegArg (PPROC pproc, PICODE picode, PICODE ticode)
/* Updates the argument table by including the register(s) (ie. lhs of
 * picode) and the actual expression (ie. rhs of picode).
 * Note: register(s) are only included once in the table.   */
{ COND_EXPR *lhs;
  PSTKFRAME ps, ts;
  ID *id;
  Int i, tidx;
  boolT regExist;
  condId type;
  PPROC tproc;
  byte regL, regH;		/* Registers involved in arguments */

	/* Flag ticode as having register arguments */
	tproc = ticode->ic.hl.oper.call.proc;
	tproc->flg |= REG_ARGS;

	/* Get registers and index into target procedure's local list */
	ps = ticode->ic.hl.oper.call.args;
	ts = &tproc->args;
	lhs = picode->ic.hl.oper.asgn.lhs;
	type = lhs->expr.ident.idType;
	if (type == REGISTER)
	{
		regL = pproc->localId.id[lhs->expr.ident.idNode.regiIdx].id.regi;
		if (regL < rAL)
			tidx = newByteWordRegId (&tproc->localId, TYPE_WORD_SIGN, regL);
		else
			tidx = newByteWordRegId (&tproc->localId, TYPE_BYTE_SIGN, regL);
	}
	else if (type == LONG_VAR)
	{
		regL = pproc->localId.id[lhs->expr.ident.idNode.longIdx].id.longId.l;
		regH = pproc->localId.id[lhs->expr.ident.idNode.longIdx].id.longId.h;
		tidx = newLongRegId (&tproc->localId, TYPE_LONG_SIGN, regH, regL, 0);
	}

	/* Check if register argument already on the formal argument list */
	regExist = FALSE;
	for (i = 0; i < ts->csym; i++)
	{
		if (type == REGISTER)
		{
			if ((ts->sym[i].regs != NULL) && 
				(ts->sym[i].regs->expr.ident.idNode.regiIdx == tidx))
			{
				regExist = TRUE;
				i = ts->csym;
			}
		}
		else if (type == LONG_VAR)
		{
			if ((ts->sym[i].regs != NULL) && 
				(ts->sym[i].regs->expr.ident.idNode.longIdx == tidx))
			{
				regExist = TRUE;
				i = ts->csym;
			}
		}
	}

	/* Do ts (formal arguments) */
	if (regExist == FALSE)
	{
    	if (ts->csym == ts->alloc) 
    	{
			ts->alloc += 5;
        	ts->sym = allocVar(ts->sym, ts->alloc * sizeof(STKSYM));
    	}
    	sprintf (ts->sym[ts->csym].name, "arg%ld", ts->csym);
		if (type == REGISTER)
		{
			if (regL < rAL)
			{
    			ts->sym[ts->csym].type = TYPE_WORD_SIGN;
    			ts->sym[ts->csym].regs = idCondExpRegIdx (tidx, WORD_REG);
			}
			else
			{
				ts->sym[ts->csym].type = TYPE_BYTE_SIGN;
    			ts->sym[ts->csym].regs = idCondExpRegIdx (tidx, BYTE_REG);
			}
			sprintf (tproc->localId.id[tidx].name, "arg%ld", ts->csym);
		}
		else if (type == LONG_VAR)
		{
    		ts->sym[ts->csym].regs = idCondExpLongIdx (tidx);
    		ts->sym[ts->csym].type = TYPE_LONG_SIGN; 
			sprintf (tproc->localId.id[tidx].name, "arg%ld", ts->csym);
			propLongId (&tproc->localId, regL, regH,
										tproc->localId.id[tidx].name);
		}

		ts->csym++;
		ts->numArgs++;
	}

	/* Do ps (actual arguments) */
    if (ps->csym == ps->alloc) 
    {
        ps->alloc += 5;
        ps->sym = allocVar(ps->sym, ps->alloc * sizeof(STKSYM));
    }
    sprintf (ps->sym[ps->csym].name, "arg%ld", ps->csym);
    ps->sym[ps->csym].actual = picode->ic.hl.oper.asgn.rhs;
    ps->sym[ps->csym].regs = lhs;

	/* Mask off high and low register(s) in picode */
	switch (type) {
	  case REGISTER:	
		id = &pproc->localId.id[lhs->expr.ident.idNode.regiIdx];
		picode->du.def &= maskDuReg[id->id.regi];
		if (id->id.regi < rAL)
    		ps->sym[ps->csym].type = TYPE_WORD_SIGN;
		else
			ps->sym[ps->csym].type = TYPE_BYTE_SIGN;
		break;
	  case LONG_VAR: 
		id = &pproc->localId.id[lhs->expr.ident.idNode.longIdx];
		picode->du.def &= maskDuReg[id->id.longId.h];
		picode->du.def &= maskDuReg[id->id.longId.l];
    	ps->sym[ps->csym].type = TYPE_LONG_SIGN; 
		break;
	}

	ps->csym++;
	ps->numArgs++;
}


void allocStkArgs (PICODE picode, Int num)
/* Allocates num arguments in the actual argument list of the current
 * icode picode.	*/
{ PSTKFRAME ps;

	ps = picode->ic.hl.oper.call.args;
    ps->alloc = num;
	ps->csym = num;
	ps->numArgs = num;
    ps->sym = allocVar(ps->sym, ps->alloc * sizeof(STKSYM));
}


boolT newStkArg (PICODE picode, COND_EXPR *exp, llIcode opcode, PPROC pproc)
/* Inserts the new expression (ie. the actual parameter) on the argument
 * list.
 * Returns: TRUE if it was a near call that made use of a segment register.
 *			FALSE elsewhere	*/
{ PSTKFRAME ps;
  byte regi;

	/* Check for far procedure call, in which case, references to segment
	 * registers are not be considered another parameter (i.e. they are
	 * long references to another segment) */
	if (exp)
	{
		if ((exp->type == IDENTIFIER) && (exp->expr.ident.idType == REGISTER))
		{
			regi =  pproc->localId.id[exp->expr.ident.idNode.regiIdx].id.regi;
			if ((regi >= rES) && (regi <= rDS))
				if (opcode == iCALLF)
					return (FALSE);
				else
					return (TRUE);
		}
	}

	/* Place register argument on the argument list */
	ps = picode->ic.hl.oper.call.args;
    if (ps->csym == ps->alloc) 
    {
        ps->alloc += 5;
        ps->sym = allocVar(ps->sym, ps->alloc * sizeof(STKSYM));
    }
    ps->sym[ps->csym].actual = exp;
	ps->csym++;
	ps->numArgs++;
	return (FALSE);
}


void placeStkArg (PICODE picode, COND_EXPR *exp, Int pos)
/* Places the actual argument exp in the position given by pos in the
 * argument list of picode.	*/
{ PSTKFRAME ps;

	ps = picode->ic.hl.oper.call.args;
	ps->sym[pos].actual = exp;
	sprintf (ps->sym[pos].name, "arg%ld", pos);
}


void adjustActArgType (COND_EXPR *exp, hlType forType, PPROC pproc)
/* Checks to determine whether the expression (actual argument) has the
 * same type as the given type (from the procedure's formal list).  If not,
 * the actual argument gets modified */
{ hlType actType;
  Int offset, offL, size;

	if (exp == NULL)
		return;

	actType = expType (exp, pproc);
	if ((actType != forType) && (exp->type == IDENTIFIER))
	{
		switch (forType) {
		  case TYPE_UNKNOWN: case TYPE_BYTE_SIGN:
		  case TYPE_BYTE_UNSIGN: case TYPE_WORD_SIGN:
		  case TYPE_WORD_UNSIGN: case TYPE_LONG_SIGN:
		  case TYPE_LONG_UNSIGN: case TYPE_RECORD:
				break;

		  case TYPE_PTR:
		  case TYPE_CONST:
				break;

		  case TYPE_STR:
				switch (actType) {
				  case TYPE_CONST:
					/* It's an offset into image where a string is
					 * found.  Point to the string.	*/
					offL = exp->expr.ident.idNode.kte.kte;
					if (prog.fCOM)
						offset = (pproc->state.r[rDS]<<4) + offL + 0x100;
					else
						offset = (pproc->state.r[rDS]<<4) + offL;
					exp->expr.ident.idNode.strIdx = offset;
					exp->expr.ident.idType = STRING;
					break;

				  case TYPE_PTR:
					/* It's a pointer to a char rather than a pointer to 
					 * an integer */
					/***HERE - modify the type ****/
					break;

				  case TYPE_WORD_SIGN:

					break;
				} /* eos */
				break;
		}
	}
}


void adjustForArgType (PSTKFRAME pstkFrame, Int numArg, hlType actType)
/* Determines whether the formal argument has the same type as the given
 * type (type of the actual argument).  If not, the formal argument is 
 * changed its type */
{ hlType forType;
  PSTKSYM psym, nsym;
  Int off, i;

	/* Find stack offset for this argument */
	off = pstkFrame->minOff;
	for (i = 0; i < numArg; i++)
		off += pstkFrame->sym[i].size;

	/* Find formal argument */
	if (numArg < pstkFrame->csym)
	{
		psym = &pstkFrame->sym[numArg];
		i = numArg;
		while ((i < pstkFrame->csym) && (psym->off != off))
		{
			psym++;
			i++;
		}
		if (numArg == pstkFrame->csym)
			return;
	}
	/* If formal argument does not exist, do not create new ones, just
	 * ignore actual argument */
	else
		return;

	forType = psym->type;
	if (forType != actType)
	{
		switch (actType) {
		  case TYPE_UNKNOWN: case TYPE_BYTE_SIGN:
		  case TYPE_BYTE_UNSIGN: case TYPE_WORD_SIGN:
		  case TYPE_WORD_UNSIGN: case TYPE_RECORD:
				break;

		  case TYPE_LONG_UNSIGN: case TYPE_LONG_SIGN:
				if ((forType == TYPE_WORD_UNSIGN) || 
					(forType == TYPE_WORD_SIGN) ||
					(forType == TYPE_UNKNOWN))
				{
					/* Merge low and high */
					psym->type = actType;
					psym->size = 4;
					nsym = psym + 1;
					sprintf (nsym->macro, "HI");
					sprintf (psym->macro, "LO");
					nsym->hasMacro = TRUE;
					psym->hasMacro = TRUE;
					sprintf (nsym->name, "%s", psym->name);
					nsym->invalid = TRUE;
					pstkFrame->numArgs--;
				}
				break;

		  case TYPE_PTR:
		  case TYPE_CONST:
		  case TYPE_STR:
				break;
		} /* eos */
	}
}

