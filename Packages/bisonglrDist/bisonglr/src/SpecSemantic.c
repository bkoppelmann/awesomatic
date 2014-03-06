static char RCSid[] = "$Id: SpecSemantic.c,v 1.1.1.1 2009/07/20 19:38:16 profw Exp $";
/* Copyright 1996, The Regents of the University of Colorado */

/* This file is part of the Eli Module Library.

The Eli Module Library is free software; you can redistribute it and/or
modify it under the terms of the GNU Library General Public License as
published by the Free Software Foundation; either version 2 of the
License, or (at your option) any later version.

The Eli Module Library is distributed in the hope that it will be
useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Library General Public License for more details.

You should have received a copy of the GNU Library General Public
License along with the Eli Module Library; see the file COPYING.LIB.
If not, write to the Free Software Foundation, Inc., 59 Temple Place -
Suite 330, Boston, MA 02111-1307, USA.  */

/* As a special exception, when this file is copied by Eli into the
   directory resulting from a :source derivation, you may use that
   created filProduction: Lhs_nterm ':' SequenceList '.' &'_nsp -= 1;_nst[_nsp]=Mkrule_002(&curpos, _nst[_nsp+0], _nst[_nsp+1]);' .
e as a part of that directory without restriction. */

#include "err.h"
#include "source.h"
#include "tabsize.h"
#include "gla.h"
#include "ScanProc.h"




/***/
char *
#if defined(__cplusplus) || defined(__STDC__)
SpecSemantic(char *start, int len)
#else
SpecSemantic(start, len)
char *start; int len;
#endif
/* Scan a C compound statement after the opening brace
 *    On entry
 *       start points to the opening brace
 *       len=length of the opening delimiter
 *    On exit-
 *       Ctext points to the character position following the
 *          closing brace
 ***/
{
   register char c ;
   register char *p = start + len;
   int count = 0;
   register char pbackup = *p;
   //printf("MYSCANNER!\n");
   for (;;) {
      //printf("p: \"%s\"\n", p);
      if (*p == '\0') {
         int current = p - start;
	 TokenStart = start = auxNUL(start, current);
         p = start + current;
         if (*p == '\0') {
	    message(ERROR,"file ends in Semantic Entry",0,&curpos);
            return p;
         }
      }
      c = *p++;
      count++;


      switch (c) {
      case '\'':
     	 return p;
         break;
      default:
         ;
      }
   }
}
