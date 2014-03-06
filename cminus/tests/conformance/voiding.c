int
voiding(int v)
{
   int a = 0;
   (v ? (a = 1) + v : v - (a = 2)) % 10;
   return a;
}
