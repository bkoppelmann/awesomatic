/* $Id: header.h,v 1.4 1994/03/26 20:54:31 waite Exp $ */
/* Exported definitions for PDL header output generation */
/* Copyright 1994, The Regents of the University of Colorado */

#ifndef HEADER_H
#define HEADER_H

#include "ptg_gen.h"	/* definition for PTGNode */
#include "deftbl.h"	/* definition for DefTableKey */
#include "envmod.h"	/* definition for Environment */
#include "func.h"	/* definition for funclist */

typedef struct {
	PTGNode type;
	PTGNode opname;
	PTGNode paramlist;
} ExtOutput;

/* exported functions */

#if defined(__cplusplus) || defined(__STDC__)
extern PTGNode GenExterns(DefTableKey key, int type, 
                            Environment env, funclist list);
extern PTGNode GenMacros(DefTableKey key, int prop,
			    Environment env, funclist list);
extern ExtOutput BuildExternSpec(PTGNode type, PTGNode opname,
				 PTGNode paramlist);
#else
extern PTGNode GenExterns();
extern PTGNode GenMacros();
extern ExtOutput BuildExternSpec();
#endif

#endif
