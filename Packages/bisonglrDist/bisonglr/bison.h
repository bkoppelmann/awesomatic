#include "err.h"
#include <stdio.h>
#include <stdlib.h>
#include "HEAD.h"
#include "gsdescr.h"
#include "treestack.h"


/*** Size of internally used static sized stacks ***/
#ifndef STACKSIZE
#define	STACKSIZE	500
#endif

/*** Default attribute value of inserted semantic terminals ***/
#ifndef	DEFAULT_ATTRVAL
#define	DEFAULT_ATTRVAL	0
#endif

/*** Types ***/

#define	TOKENTYPE	GRUNDSYMBOLDESKRIPTOR
#define	CODETYPE	TERMINALSYMBOL
#define	ATTRTYPE	ATTRTYPE

/*** TOKENTYPE-Macros ***/

#define	CODE(tok)	(T_CODE((*tok)))
#define	POS(tok)	(T_POS((*tok)))
#define	LINE(tok)	(LineOf(T_POS((*tok))))
#define	ATTR(tok)	(T_ATTR((*tok)))

/*** Function Calls ***/

/* Scanner interface: read next token from input stream */
/*	GET_TOKEN must be defined in "gsdescr.h" */

static	TOKENTYPE	curtok;
#define TokenStack(i) curtok
int yylex(void);
void yyerror (const char*);
