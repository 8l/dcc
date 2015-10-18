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
 *      dcc project Universal Decompilation Module
 *   This is supposedly a machine independant and language independant module
 *   that just plays with abstract cfg's and intervals and such like.
 * (C) Cristina Cifuentes
 ****************************************************************************/

#include "dcc.h"
#include <stdio.h>

static void displayCFG(PPROC pProc);
static void displayStats(PPROC pProc);
static void displayDfs(PBB pBB);

/****************************************************************************
 * udm
 ****************************************************************************/
void udm(void)
{   PPROC   pProc;
    derSeq *derivedG;

	/* Build the control flow graph, find idioms, and convert low-level
	 * icodes to high-level ones */
    for (pProc = pLastProc; pProc; pProc = pProc->prev) 
	{
        if (pProc->flg & PROC_ISLIB) 
			continue;         /* Ignore library functions */

        /* Create the basic control flow graph */
        pProc->cfg = createCFG(pProc);
        if (option.VeryVerbose)
            displayCFG(pProc);

        /* Remove redundancies and add in-edge information */
        compressCFG(pProc);

        /* Print 2nd pass assembler listing */
        if (option.asm2)
            disassem(2, pProc);

		/* Idiom analysis and propagation of long type */
		lowLevelAnalysis (pProc);

		/* Generate HIGH_LEVEL icodes whenever possible */
		highLevelGen (pProc);
	}

    /* Data flow analysis - eliminate condition codes, extraneous registers
	 * and intermediate instructions.  Find expressions by forward
	 * substitution algorithm */
    dataFlow (pProcList, 0); 

	/* Control flow analysis - structuring algorithm */
    for (pProc = pLastProc; pProc; pProc = pProc->prev) 
	{
        if (pProc->flg & PROC_ISLIB) 
			continue;         /* Ignore library functions */

        /* Make cfg reducible and build derived sequences */
        checkReducibility(pProc, &derivedG); 

        if (option.VeryVerbose)
           displayDerivedSeq(derivedG);  

        /* Structure the graph */
        structure(pProc, derivedG); 

		/* Check for compound conditions */
		compoundCond (pProc);

        if (option.verbose) {
           printf("\nDepth first traversal - Proc %s\n", pProc->name);
           displayDfs(pProc->cfg);
        } 

        /* Free storage occupied by this procedure */
        freeDerivedSeq(derivedG); 

        if (option.Stats)
            displayStats(pProc);
    }
}


static char *nodeType[] = {"branch", "if", "case", "fall", "return", "call",
               "loop", "repeat", "interval", "cycleHead", 
               "caseHead", "terminate",
               "nowhere" };

static char *loopType[] = {"noLoop", "while", "repeat", "loop", "for"};


/****************************************************************************
 * displayCFG - Displays the Basic Block list
 ***************************************************************************/
static void displayCFG(PPROC pProc)
{
    Int i;
    PBB pBB;

    printf("\nBasic Block List - Proc %s", pProc->name);

    for (pBB = pProc->cfg; pBB; pBB = pBB->next) {
        printf("\nnode type = %s, ", nodeType[pBB->nodeType]);
        printf("start = %ld, length = %ld, #out edges = %ld\n",
            pBB->start, pBB->length, pBB->numOutEdges);

        for (i = 0; i < pBB->numOutEdges; i++)
            printf(" outEdge[%2d] = %ld\n",i, pBB->edges[i].BBptr->start);
    }
}


/****************************************************************************
 * displayStats - Displays statistics on nodes and arcs of the CFG
 ***************************************************************************/
static void displayStats(PPROC pProc)
{
   printf("\nStatistics - Proc %s\n", pProc->name);
   printf("Number of BBs:\n");
   printf("   Before: %4d\n   After : %4d\n", stats.numBBbef, stats.numBBaft);
   printf("   Ratio : %2.2f%%\n", 100.0 - (stats.numBBaft * 100.0)/stats.numBBbef);
   printf("Number outEdges:\n");
   printf("   Before: %4d\n   After : %4d\n", stats.numEdgesBef, stats.numEdgesAft);
   printf ("nth order = %ld\n\n", stats.nOrder);
}


/*****************************************************************************
 * displayDfs - Displays the CFG using a depth first traversal
 ****************************************************************************/
static void displayDfs(PBB pBB)
{
    Int i;

    if (! pBB)
        return;
    pBB->traversed = DFS_DISP;

    printf("node type = %s, ", nodeType[pBB->nodeType]);
    printf("start = %ld, length = %ld, #in-edges = %ld, #out-edges = %ld\n",
        pBB->start, pBB->length, pBB->numInEdges, pBB->numOutEdges);
    printf("dfsFirst = %ld, dfsLast = %ld, immed dom = %ld\n",
        pBB->dfsFirstNum, pBB->dfsLastNum, 
		pBB->immedDom == MAX ? -1 : pBB->immedDom);
    printf("loopType = %s, loopHead = %ld, latchNode = %ld, follow = %ld\n",
            loopType[pBB->loopType], 
        pBB->loopHead == MAX ? -1 : pBB->loopHead, 
        pBB->latchNode == MAX ? -1 : pBB->latchNode, 
        pBB->loopFollow == MAX ? -1 : pBB->loopFollow);
    printf ("ifFollow = %ld, caseHead = %ld, caseTail = %ld\n", 
        pBB->ifFollow == MAX ? -1 : pBB->ifFollow,
        pBB->caseHead == MAX ? -1 : pBB->caseHead,
        pBB->caseTail == MAX ? -1 : pBB->caseTail);

    if (pBB->nodeType == INTERVAL_NODE)
        printf("corresponding interval = %ld\n", pBB->correspInt->numInt);
    else for (i = 0; i < pBB->numInEdges; i++)
        printf ("  inEdge[%ld] = %ld\n", i, pBB->inEdges[i]->start); 

    /* Display out edges information */
    for (i = 0; i < pBB->numOutEdges; i++)
        if (pBB->nodeType == INTERVAL_NODE)
            printf(" outEdge[%ld] = %ld\n", i,
                pBB->edges[i].BBptr->correspInt->numInt);
        else 
            printf(" outEdge[%ld] = %ld\n", i, pBB->edges[i].BBptr->start);
    printf("----\n");

    /* Recursive call on successors of current node */
    for (i = 0; i < pBB->numOutEdges; i++)
        if (pBB->edges[i].BBptr->traversed != DFS_DISP)
            displayDfs(pBB->edges[i].BBptr);
}
