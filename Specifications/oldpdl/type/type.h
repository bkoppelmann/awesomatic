/* $Id: type.h,v 1.1 1992/06/20 19:16:26 kadhim Exp $ */
/* Exported definitions for type analysis attribute computations */
/* Copyright (c) 1992, The Regents of the University of Colorado */

#ifndef TYPE_H
#define TYPE_H

#include "deftbl.h"	/* definition table module */

/* These are used to indicate whether a property has been declared more */
/* than once with different types */
#define Unknown           0
#define Defined           1
#define MultDefined       2

/* exported functions */

#if defined(__cplusplus) || defined(__STDC__)
extern void TypeIs(DefTableKey key, DefTableKey type);
#else
extern void TypeIs();
#endif

#endif
