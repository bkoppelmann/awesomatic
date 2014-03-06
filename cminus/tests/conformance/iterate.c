int
iterate(int v)
{
   int a = 0;
   while (a < 3) {
      if (v == 0) break;
      a = a + 1;
      if (v == 1) continue;
      a = 10;
   }
   return a;
}
