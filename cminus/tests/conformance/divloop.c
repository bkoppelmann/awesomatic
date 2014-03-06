int
divloop(int x, int y)
/* returns the answer to the following question (y raised to ??? = x) */
/* eg. divloop(64,2) = 6 because (2**6 = 64) */
/* if y does not divide into x evenly 0 is returned */
{ int ctr = 0;
  while (x - 1 > 0)
    { 
      if ( !((x % y) ? x=0 : (ctr=ctr+1)))  continue; else x = x / y ;
    }
  return ctr;
}
