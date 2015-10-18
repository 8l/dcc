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
 * Perfect hashing function library. Contains functions to generate perfect
 * hashing functions
 * (C) Mike van Emmerik
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "perfhlib.h"

/* Private data structures */

static  int     NumEntry;   /* Number of entries in the hash table (# keys) */
static  int     EntryLen;   /* Size (bytes) of each entry (size of keys) */
static  int     SetSize;    /* Size of the char set */
static  char    SetMin;     /* First char in the set */
static  int     NumVert;    /* c times NumEntry */

static  word    *T1base, *T2base;   /* Pointers to start of T1, T2 */
static  word    *T1, *T2;   /* Pointers to T1[i], T2[i] */

static  int     *graphNode; /* The array of edges */
static  int     *graphNext; /* Linked list of edges */
static  int     *graphFirst;/* First edge at a vertex */

static  short   *g;         /* g[] */

static  int     numEdges;   /* An edge counter */
static  bool    *visited;   /* Array of bools: whether visited */

/* Private prototypes */
static void initGraph(void);
static void addToGraph(int e, int v1, int v2);
static bool isCycle(void);
static void duplicateKeys(int v1, int v2);
                     
void
hashParams(int _NumEntry, int _EntryLen, int _SetSize, char _SetMin,
                    int _NumVert)
{
    /* These parameters are stored in statics so as to obviate the need for
        passing all these (or defererencing pointers) for every call to hash()
    */

    NumEntry = _NumEntry;
    EntryLen = _EntryLen;
    SetSize  = _SetSize;
    SetMin   = _SetMin;
    NumVert  = _NumVert;

    /* Allocate the variable sized tables etc */
    if ((T1base = (word *)malloc(EntryLen * SetSize * sizeof(word))) == 0)
    {
        goto BadAlloc;
    }
    if ((T2base = (word *)malloc(EntryLen * SetSize * sizeof(word))) == 0)
    {
        goto BadAlloc;
    }

    if ((graphNode = (int *)malloc((NumEntry*2 + 1) * sizeof(int))) == 0)
    {
        goto BadAlloc;
    }
    if ((graphNext = (int *)malloc((NumEntry*2 + 1) * sizeof(int))) == 0)
    {
        goto BadAlloc;
    }
    if ((graphFirst = (int *)malloc((NumVert + 1) * sizeof(int))) == 0)
    {
        goto BadAlloc;
    }

    if ((g = (short *)malloc((NumVert+1) * sizeof(short))) == 0)
    {
        goto BadAlloc;
    }
    if ((visited = (bool *)malloc((NumVert+1) * sizeof(bool))) == 0)
    {
        goto BadAlloc;
    }
    return;

BadAlloc:
    printf("Could not allocate memory\n");
    hashCleanup();
    exit(1);
}

void
hashCleanup(void)
{
    /* Free the storage for variable sized tables etc */
    if (T1base) free(T1base);
    if (T2base) free(T2base);
    if (graphNode) free(graphNode);
    if (graphNext) free(graphNext);
    if (graphFirst) free(graphFirst);
    if (g) free(g);
}

void
map(void)
{
    int i, j, c;
    word f1, f2;
    bool cycle;
    byte *keys;

    c = 0;

    do
    {
        initGraph();
        cycle = FALSE;

        /* Randomly generate T1 and T2 */
        for (i=0; i < SetSize*EntryLen; i++)
        {
            T1base[i] = rand() % NumVert;
            T2base[i] = rand() % NumVert;
        }

        for (i=0; i < NumEntry; i++)
        {
            f1 = 0; f2 = 0;
            getKey(i, &keys);
            for (j=0; j < EntryLen; j++)
            {
                T1 = T1base + j * SetSize;
                T2 = T2base + j * SetSize;
                f1 += T1[keys[j] - SetMin];
                f2 += T2[keys[j] - SetMin];
            }
            f1 %= (word)NumVert;
            f2 %= (word)NumVert;
            if (f1 == f2)
            {
                /* A self loop. Reject! */
                printf("Self loop on vertex %d!\n", f1);
                cycle = TRUE;
                break;
            }
            addToGraph(numEdges++, f1, f2);
        }
        if (cycle || (cycle = isCycle()))   /* OK - is there a cycle? */
        {
            printf("Iteration %d\n", ++c);
        }
        else
        {
            break;
        }
    }
    while (/* there is a cycle */ 1);

}

/* Initialise the graph */
static void
initGraph(void)
{
    int i;

    for (i=1; i <= NumVert; i++)
    {
        graphFirst[i] = 0;
    }

    for (i= -NumEntry; i <= NumEntry; i++)
    {
        /* No need to init graphNode[] as they will all be filled by successive
            calls to addToGraph() */
        graphNext[NumEntry+i] = 0;
    }

    numEdges = 0;
}

/* Add an edge e between vertices v1 and v2 */
/* e, v1, v2 are 0 based */
static void
addToGraph(int e, int v1, int v2)
{
    e++; v1++; v2++;                        /* So much more convenient */

    graphNode[NumEntry+e] = v2;             /* Insert the edge information */
    graphNode[NumEntry-e] = v1;

    graphNext[NumEntry+e] = graphFirst[v1]; /* Insert v1 to list of alphas */
    graphFirst[v1]= e;
    graphNext[NumEntry-e] = graphFirst[v2]; /* Insert v2 to list of omegas */
    graphFirst[v2]= -e;

}

bool DFS(int parentE, int v)
{
    int e, w;

    /* Depth first search of the graph, starting at vertex v, looking for
        cycles. parent and v are origin 1. Note parent is an EDGE,
        not a vertex */

    visited[v] = TRUE;

    /* For each e incident with v .. */
    for (e = graphFirst[v]; e; e = graphNext[NumEntry+e])
    {
        byte *key1;
        
        getKey(abs(e)-1, &key1);
        if (*(long *)key1 == 0)
        {
            /* A deleted key. Just ignore it */
            continue;
        }
        w = graphNode[NumEntry+e];
        if (visited[w])
        {
            /* Did we just come through this edge? If so, ignore it. */
            if (abs(e) != abs(parentE))
            {
                /* There is a cycle in the graph. There is some subtle code here
                    to work around the distinct possibility that there may be
                    duplicate keys. Duplicate keys will always cause unit
                    cycles, since f1 and f2 (used to select v and w) will be the
                    same for both. The edges (representing an index into the
                    array of keys) are distinct, but the key values are not.
                    The logic is as follows: for the candidate edge e, check to
                    see if it terminates in the parent vertex. If so, we test
                    the keys associated with e and the parent, and if they are
                    the same, we can safely ignore e for the purposes of cycle
                    detection, since edge e adds nothing to the cycle. Cycles
                    involving v, w, and e0 will still be found. The parent
                    edge was not similarly eliminated because at the time when
                    it was a candidate, v was not yet visited.
                    We still have to remove the key from further consideration,
                    since each edge is visited twice, but with a different
                    parent edge each time.
                */
                /* We save some stack space by calculating the parent vertex
                    for these relatively few cases where it is needed */
                int parentV = graphNode[NumEntry-parentE];

                if (w == parentV)
                {
                    byte *key2;

                    getKey(abs(parentE)-1,  &key2);
                    if (memcmp(key1, key2, EntryLen) == 0)
                    {
                        printf("Duplicate keys with edges %d and %d (",
                            e, parentE);
                        dispKey(abs(e)-1);
                        printf(" & ");
                        dispKey(abs(parentE)-1);
                        printf(")\n");
/*                      *(long *)key1 = 0;      /* Wipe the key */
memset(key1, 0, EntryLen);
                    }
                    else 
                    {
                        /* A genuine (unit) cycle. */
printf("There is a unit cycle involving vertex %d and edge %d\n", v, e);
                        return TRUE;
                    }

                }
                else
                {
                    /* We have reached a previously visited vertex not the
                        parent. Therefore, we have uncovered a genuine cycle */
printf("There is a cycle involving vertex %d and edge %d\n", v, e);
                    return TRUE;

                }
            }
        }
        else                                /* Not yet seen. Traverse it */
        {
            if (DFS(e, w))
            {
                /* Cycle found deeper down. Exit */
                return TRUE;
            }
        }
    }
    return FALSE;
}

static bool
isCycle(void)
{
    int v;

    for (v=1; v <= NumVert; v++)
    {
        visited[v] = FALSE;
    }
    for (v=1; v <= NumVert; v++)
    {
        if (!visited[v])
        {
            if (DFS(-32767, v))
            {
                return TRUE;
            }
        }
    }
    return FALSE;
}

void
traverse(int u)
{
    int w, e;

    visited[u] = TRUE;
    /* Find w, the neighbours of u, by searching the edges e associated with u */
    e = graphFirst[1+u];
    while (e)
    {
        w = graphNode[NumEntry+e]-1;
        if (!visited[w])
        {
            g[w] = (abs(e)-1 - g[u]) % NumEntry;
            if (g[w] < 0) g[w] += NumEntry;     /* Keep these positive */
            traverse(w);
        }
        e = graphNext[NumEntry+e];
    }

}

void
assign(void)
{
    int v;

    
    for (v=0; v < NumVert; v++)
    {
        g[v] = 0;                           /* g is sparse; leave the gaps 0 */
        visited[v] = FALSE;
    }

    for (v=0; v < NumVert; v++)
    {
        if (!visited[v])
        {
            g[v] = 0;
            traverse(v);
        }
    }
}

int
hash(byte *string)
{
    word u, v;
    int  j;

    u = 0;
    for (j=0; j < EntryLen; j++)
    {
        T1 = T1base + j * SetSize;
        u += T1[string[j] - SetMin];
    }
    u %= NumVert;

    v = 0;
    for (j=0; j < EntryLen; j++)
    {
        T2 = T2base + j * SetSize;
        v += T2[string[j] - SetMin];
    }
    v %= NumVert;

    return (g[u] + g[v]) % NumEntry;
}

word *
readT1(void)
{
    return T1base;
}

word *
readT2(void)
{
    return T2base;
}

word *
readG(void)
{
    return (word *)g;
}

#if 0
void dispRecord(int i);

void
duplicateKeys(int v1, int v2)
{
    int i, j;
    byte *keys;
    int u, v;

    v1--; v2--;             /* These guys are origin 1 */

    printf("Duplicate keys:\n");

    for (i=0; i < NumEntry; i++)
    {
        getKey(i, &keys);
        u = 0;
        for (j=0; j < EntryLen; j++)
        {
            T1 = T1base + j * SetSize;
            u += T1[keys[j] - SetMin];
        }
        u %= NumVert;
        if ((u != v1) && (u != v2)) continue;

        v = 0;
        for (j=0; j < EntryLen; j++)
        {
            T2 = T2base + j * SetSize;
            v += T2[keys[j] - SetMin];
        }
        v %= NumVert;

        if ((v == v2) || (v == v1))
        {
            printf("Entry #%d key: ", i+1);
            for (j=0; j < EntryLen; j++) printf("%02X ", keys[j]);
            printf("\n");
            dispRecord(i+1);
        }
    }
    exit(1);


}
#endif
