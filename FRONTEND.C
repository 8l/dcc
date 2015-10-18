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
 *			dcc project Front End module
 * Loads a program into simulated main memory and builds the procedure list.
 * (C) Cristina Cifuentes
 ****************************************************************************/

#include "dcc.h"
#include <stdio.h>
#include <string.h>
#ifdef __BORLAND__
#include <alloc.h>
#else
#include <malloc.h>
#endif

typedef struct {			/*        PSP structure					*/
	word int20h;			/* interrupt 20h						*/
	word eof;				/* segment, end of allocation block		*/
	byte res1;				/* reserved                         	*/
	byte dosDisp[5];		/* far call to DOS function dispatcher	*/
	byte int22h[4];			/* vector for terminate routine			*/
	byte int23h[4];			/* vector for ctrl+break routine		*/
	byte int24h[4];			/* vector for error routine				*/
	byte res2[22];			/* reserved								*/
	word segEnv;			/* segment address of environment block	*/
	byte res3[34];			/* reserved								*/
	byte int21h[6];			/* opcode for int21h and far return		*/
	byte res4[6];			/* reserved								*/
	byte fcb1[16];			/* default file control block 1			*/
	byte fcb2[16];      	/* default file control block 2			*/
	byte res5[4];			/* reserved								*/
	byte cmdTail[0x80];		/* command tail and disk transfer area	*/
} PSP;

static struct {				/*      EXE file header		 	 */
	 byte	sigLo;			/* .EXE signature: 0x4D 0x5A	 */
	 byte	sigHi;
	 word	lastPageSize;	/* Size of the last page		 */
	 word	numPages;		/* Number of pages in the file	 */
	 word	numReloc;		/* Number of relocation items	 */
	 word	numParaHeader;	/* # of paragraphs in the header */
	 word	minAlloc;		/* Minimum number of paragraphs	 */
	 word	maxAlloc;		/* Maximum number of paragraphs	 */
	 word	initSS;			/* Segment displacement of stack */
	 word	initSP;			/* Contents of SP at entry       */
	 word	checkSum;		/* Complemented checksum         */
	 word	initIP;			/* Contents of IP at entry       */
	 word	initCS;			/* Segment displacement of code  */
	 word	relocTabOffset;	/* Relocation table offset       */
	 word	overlayNum;		/* Overlay number                */
} header;

#define EXE_RELOCATION  0x10		/* EXE images rellocated to above PSP */

static void LoadImage(char *filename);
static void displayLoadInfo(void);
static void displayMemMap(void);

/*****************************************************************************
 * FrontEnd - invokes the loader, parser, disassembler (if asm1), icode
 * rewritter, and displays any useful information.
 ****************************************************************************/
void FrontEnd (char *filename, PCALL_GRAPH *pcallGraph)
{
	PPROC pProc;
	PSYM psym;
	Int	i, c;
	
	/* Load program into memory */
	LoadImage(filename);

	if (option.verbose)
	{
		displayLoadInfo();
	}

	/* Do depth first flow analysis building call graph and procedure list,
	 * and attaching the I-code to each procedure          */
	parse (pcallGraph);

	if (option.asm1)
	{
		printf ("%s: writing assembler file %s\n", progname, asm1_name);
	}

	/* Search through code looking for impure references and flag them */
	for (pProc = pProcList; pProc; pProc = pProc->next)
	{
		for (i = 0; i < pProc->Icode.numIcode; i++)
		{
			if (pProc->Icode.icode[i].ic.ll.flg & (SYM_USE | SYM_DEF))
			{
				psym = &symtab.sym[pProc->Icode.icode[i].ic.ll.caseTbl.numEntries];
				for (c = (Int)psym->label; c < (Int)psym->label+psym->size; c++)
				{
					if (BITMAP(c, BM_CODE))
					{
						pProc->Icode.icode[i].ic.ll.flg |= IMPURE;
						pProc->flg |= IMPURE;
						break;
					}
				}
			}
		}
		/* Print assembler listing */
		if (option.asm1)
			disassem(1, pProc);
	}

	if (option.Interact)
	{
		interactDis(pProcList, 0);			/* Interactive disassembler */
	}

	/* Converts jump target addresses to icode offsets */
	for (pProc = pProcList; pProc; pProc = pProc->next)
		bindIcodeOff (pProc); 

	/* Print memory bitmap */
	if (option.Map)
		displayMemMap();
}


/****************************************************************************
 * displayLoadInfo - Displays low level loader type info.
 ***************************************************************************/
static void displayLoadInfo(void)
{
	Int	i;

	printf("File type is %s\n", (prog.fCOM)?"COM":"EXE");
	if (! prog.fCOM) {
	    printf("Signature            = %02X%02X\n", header.sigLo, header.sigHi);
	    printf("File size %% 512      = %04X\n", LH(&header.lastPageSize));
	    printf("File size / 512      = %04X pages\n", LH(&header.numPages));
	    printf("# relocation items   = %04X\n", LH(&header.numReloc));
	    printf("Offset to load image = %04X paras\n", LH(&header.numParaHeader));
		printf("Minimum allocation   = %04X paras\n", LH(&header.minAlloc));
		printf("Maximum allocation   = %04X paras\n", LH(&header.maxAlloc));
	}
	printf("Load image size      = %04X\n", prog.cbImage - sizeof(PSP));
	printf("Initial SS:SP        = %04X:%04X\n", prog.initSS, prog.initSP);
	printf("Initial CS:IP        = %04X:%04X\n", prog.initCS, prog.initIP);

	if (option.VeryVerbose && prog.cReloc)
	{
		printf("\nRelocation Table\n");
		for (i = 0; i < prog.cReloc; i++)
		{
			printf("%06X -> [%04X]\n", prog.relocTable[i],
					LH(prog.Image + prog.relocTable[i]));
		}
	}
	printf("\n");
}


/*****************************************************************************
 * fill - Fills line for displayMemMap()
 ****************************************************************************/
static void fill(Int ip, char *bf)
{
	static byte type[4] = {'.', 'd', 'c', 'x'};
	byte	i;

	for (i = 0; i < 16; i++, ip++)
	{
		*bf++ = ' ';
		*bf++ = (ip < prog.cbImage)? 
			type[(prog.map[ip >> 2] >> ((ip & 3) * 2)) & 3]: ' ';
	}
	*bf = '\0';
}


/*****************************************************************************
 * displayMemMap - Displays the memory bitmap
 ****************************************************************************/
static void displayMemMap(void)
{
	char	c, b1[33], b2[33], b3[33];
	byte i;
	Int ip = 0;

	printf("\nMemory Map\n");
	while (ip < prog.cbImage)
	{
		fill(ip, b1);
		printf("%06X %s\n", ip, b1);
		ip += 16;
		for (i = 3, c = b1[1]; i < 32 && c == b1[i]; i += 2)
			;		/* Check if all same */
		if (i > 32)
		{
			fill(ip, b2);	/* Skip until next two are not same */
			fill(ip+16, b3);
			if (! (strcmp(b1, b2) || strcmp(b1, b3)))
			{
				printf("                   :\n");
				do
				{
					ip += 16;
					fill(ip+16, b1);
				} while (! strcmp(b1, b2));
			}
		}
	}
	printf("\n");
}


/*****************************************************************************
 * LoadImage
 ****************************************************************************/
static void LoadImage(char *filename)
{
	FILE   *fp;
	Int		i, cb;
	byte	buf[4];

	/* Open the input file */
	if ((fp = fopen(filename, "rb")) == NULL)
	{
		fatalError(CANNOT_OPEN, filename);
	}

	/* Read in first 2 bytes to check EXE signature */
	if (fread(&header, 1, 2, fp) != 2)
	{
		fatalError(CANNOT_READ, filename);
	}

	if (! (prog.fCOM = (boolT)(header.sigLo != 0x4D || header.sigHi != 0x5A))) {
		/* Read rest of header */
		fseek(fp, 0, SEEK_SET);
		if (fread(&header, sizeof(header), 1, fp) != 1)
		{
			fatalError(CANNOT_READ, filename);
		}

		/* This is a typical DOS kludge! */
		if (LH(&header.relocTabOffset) == 0x40)
		{
			fatalError(NEWEXE_FORMAT);
		}

		/* Calculate the load module size.
		 * This is the number of pages in the file
		 * less the length of the header and reloc table
		 * less the number of bytes unused on last page
		*/
		cb = LH(&header.numPages) * 512 - LH(&header.numParaHeader) * 16;
		if (header.lastPageSize)
		{
			cb -= 512 - LH(&header.lastPageSize);
		}
		
		/* We quietly ignore minAlloc and maxAlloc since for our
		 * purposes it doesn't really matter where in real memory
		 * the program would end up.  EXE programs can't really rely on
		 * their load location so setting the PSP segment to 0 is fine.
		 * Certainly programs that prod around in DOS or BIOS are going
		 * to have to load DS from a constant so it'll be pretty 
		 * obvious.
		*/
		prog.initCS = (int16)LH(&header.initCS) + EXE_RELOCATION;
		prog.initIP = (int16)LH(&header.initIP);
		prog.initSS = (int16)LH(&header.initSS) + EXE_RELOCATION;
		prog.initSP = (int16)LH(&header.initSP);
		prog.cReloc = (int16)LH(&header.numReloc);

		/* Allocate the relocation table */
		if (prog.cReloc)
		{
			prog.relocTable = allocMem(prog.cReloc * sizeof(Int));
			fseek(fp, LH(&header.relocTabOffset), SEEK_SET);

			/* Read in seg:offset pairs and convert to Image ptrs */
			for (i = 0; i < prog.cReloc; i++)
			{
				fread(buf, 1, 4, fp);
				prog.relocTable[i] = LH(buf) + 
					(((Int)LH(buf+2) + EXE_RELOCATION)<<4);
			}
		}
		/* Seek to start of image */
		fseek(fp, (Int)LH(&header.numParaHeader) * 16, SEEK_SET);
	}
	else
	{	/* COM file
		 * In this case the load module size is just the file length
		*/
		fseek(fp, 0, SEEK_END);
		cb = ftell(fp);

		/* COM programs start off with an ORG 100H (to leave room for a PSP)
		 * This is also the implied start address so if we load the image
		 * at offset 100H addresses should all line up properly again.
		*/
		prog.initCS = 0;
		prog.initIP = 0x100;
		prog.initSS = 0;
		prog.initSP = 0xFFFE;
		prog.cReloc = 0;

		fseek(fp, 0, SEEK_SET);
	}

	/* Allocate a block of memory for the program. */
	prog.cbImage  = cb + sizeof(PSP);
	prog.Image    = allocMem(prog.cbImage);
	prog.Image[0] = 0xCD;		/* Fill in PSP Int 20h location */
	prog.Image[1] = 0x20;		/* for termination checking     */

	/* Read in the image past where a PSP would go */
	if (cb != (Int)fread(prog.Image + sizeof(PSP), 1, (size_t)cb, fp))
	{
		fatalError(CANNOT_READ, filename);
	}

	/* Set up memory map */
	cb = (prog.cbImage + 3) / 4;
	prog.map = (byte *)memset(allocMem(cb), BM_UNKNOWN, (size_t)cb);

	/* Relocate segment constants */
	if (prog.cReloc)
	{
		for (i = 0; i < prog.cReloc; i++)
		{
			byte *p = &prog.Image[prog.relocTable[i]];
			word  w = (word)LH(p) + EXE_RELOCATION;
			*p++    = (byte)(w & 0x00FF);
			*p      = (byte)((w & 0xFF00) >> 8);
		}
	}

	fclose(fp);
}


/*****************************************************************************
 * allocMem - malloc with failure test
 ****************************************************************************/
void *allocMem(Int cb)
{
	byte *p;

/*printf("Attempt to allocMem %5ld bytes\n", cb);/**/

/*switch (_heapset('Z'))
{
	case _HEAPBADBEGIN: printf("aM: Bad heap begin\n"); getchar(); break;
	case _HEAPBADNODE:	printf("aM: Bad heap node\n");  getchar(); break;
	case _HEAPEMPTY:	printf("aM: Heap empty\n");		getchar(); break;
	case _HEAPOK:putchar('.');break;
}/**/

	if (! (p = malloc((size_t)cb)))
	{
		fatalError(MALLOC_FAILED, cb);
	}
/*printf("allocMem: %p\n", p);/**/
	return p;
}


/*****************************************************************************
 * allocVar - reallocs extra variable space
 ****************************************************************************/
void *allocVar(void *p, Int newsize)
{
/*printf("Attempt to allocVar %5d bytes\n", newsize);/**/
#if 0
switch (_heapset('Z'))
{
	case _HEAPBADBEGIN: printf("aV: Bad heap begin\n"); getchar(); break;
	case _HEAPBADNODE:	printf("aV: Bad heap node\n");
printf("Attempt to allocVar %5d bytes at %p\n", newsize, p);/**/
{
	_HEAPINFO hinfo;
	int heapstatus;
	boolT cont = TRUE;

	hinfo._pentry = NULL;

	while (cont)
	{
		switch (heapstatus = _heapwalk(&hinfo))
		{
			case _HEAPOK:
				printf("%6s block at %Fp of size %4.4X\n",
					(hinfo._useflag == _USEDENTRY ? "USED" : "FREE"),
					hinfo._pentry, hinfo._size);
				break;
			case _HEAPBADBEGIN:
				printf("Heap bad begin\n");
				break;
			case _HEAPBADNODE:
				printf("BAD NODE %6s block at %Fp of size %4.4X\n",
					(hinfo._useflag == _USEDENTRY ? "USED" : "FREE"),
					hinfo._pentry, hinfo._size);
				break;
			case _HEAPEND:
				cont = FALSE;
				break;
			case _HEAPBADPTR:
				printf("INFO BAD %6s block at %Fp of size %4.4X\n",
					(hinfo._useflag == _USEDENTRY ? "USED" : "FREE"),
					hinfo._pentry, hinfo._size);
				cont=FALSE;

		}
	}
}
getchar();
break;

	case _HEAPEMPTY:	printf("aV: Heap empty\n");		getchar(); break;
	case _HEAPOK:putchar(',');break;
}/**/
#endif

	if (! (p = realloc((byte *)p, (size_t)newsize)))
	{
		fatalError(MALLOC_FAILED, newsize);
	}

/*printf("allocVar: %p\n", p);/**/
	return p;
}

#if 0
void free(void *p)
{
	_ffree(p);
switch (_heapset('Z'))
{
	case _HEAPBADBEGIN: printf("f: Bad heap begin\n"); getchar(); break;
	case _HEAPBADNODE:	printf("f: Bad heap node\n");  getchar(); break;
	case _HEAPEMPTY:	printf("f: Heap empty\n");		getchar(); break;
	case _HEAPOK:putchar('!');break;
}/**/
}
#endif

