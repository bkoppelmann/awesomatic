/* transformsemaction.c*/
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include "transformsemaction.h"

char *transformsemaction(char * inp)
{
   /* transforms all occurrences of "TokenStack(i)" by "$(i+1)" in parameter inp */

	char * res = (char*) calloc(strlen(inp)+1, sizeof(char));
        char* start = inp;
        char* patternstart;
        char* patternend;
        int depth;
	char stackacc[5]; /* holds $i notation for bison */
 
        patternstart = strstr(start, "(TokenStack");
        while (patternstart) {
		strncat(res, start, patternstart-start);
                patternend=strstr(patternstart,")");
		sscanf(patternstart, "(TokenStack(%d)", &depth);		
		sprintf(stackacc,"($%d", depth+1);                
		strcat(res, stackacc);
		start = patternend + 1;
	        patternstart = strstr(start, "(TokenStack");
	}
	strcat(res, start);
        return res;
}


