int
opident(int x, int y)
{
   float a = 10 + y;
   int i = x == 0 ? y : a;
   int j = x*y + i; int k = x*y % i;
   while ( a<0 || x==0 && y<0 ) {
      if ( !i ) continue;
      i = x/4 || i;
      if ( i>3 ) break;
   }
   return -i > 0;
}
