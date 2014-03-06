#ifndef KEYWDS_H
#define KEYWDS_H

/* $Id: keywds.h,v 1.2 1995/10/14 22:17:51 waite Exp $ */
/* Copyright 1993, The Regents of the University of Colorado
 * Permission is granted to use any portion of this file for any purpose,
 * commercial or otherwise, provided that this notice remains unchanged.
 */

#define MAXKWD 44		/* Maximum index in KeyTable */

static Keywd KeyTable[] = {
  "assign", KwASSIGN, 6,
  "backspace", KwBACKSPACE, 9,
  "blockdata", KwBLOCKDATA, 9,
  "call", KwCALL, 4,
  "character", KwCHARACTER, 9,
  "close", KwCLOSE, 5,
  "common", KwCOMMON, 6,
  "complex", KwCOMPLEX, 7,
  "continue", KwCONTINUE, 8,
  "data", KwDATA, 4,
  "dimension", KwDIMENSION, 9,
  "do", KwDO, 2,
  "doubleprecision", KwDOUBLEPRECISION, 15,
  "else", KwELSE, 4,
  "elseif", KwELSEIF, 6,
  "end", KwEND, 3,
  "endfile", KwENDFILE, 7,
  "endif", KwENDIF, 5,
  "entry", KwENTRY, 5,
  "equivalence", KwEQUIVALENCE, 11,
  "external", KwEXTERNAL, 8,
  "format", KwFORMAT, 6,
  "function", KwFUNCTION, 8,
  "goto", KwGOTO, 4,
  "if", KwIF, 2,
  "implicit", KwIMPLICIT, 8,
  "inquire", KwINQUIRE, 7,
  "integer", KwINTEGER, 7,
  "intrinsic", KwINTRINSIC, 9,
  "logical", KwLOGICAL, 7,
  "open", KwOPEN, 4,
  "parameter", KwPARAMETER, 9,
  "pause", KwPAUSE, 5,
  "print", KwPRINT, 5,
  "program", KwPROGRAM, 7,
  "read", KwREAD, 4,
  "real", KwREAL, 4,
  "return", KwRETURN, 6,
  "rewind", KwREWIND, 6,
  "save", KwSAVE, 4,
  "stop", KwSTOP, 4,
  "subroutine", KwSUBROUTINE, 10,
  "then", KwTHEN, 4,
  "to", KwTO, 2,
  "write", KwWRITE, 5
};
#endif
