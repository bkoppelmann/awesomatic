int
blocks(int x, int y)
{int q;
   {int a = x+y; q = a+1;}
   {int a = x-y; q = a+q;}
return q;
}
