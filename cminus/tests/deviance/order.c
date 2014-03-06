int
order(int x)
{ x = x + 1;		/* Statement can't precede declaration */
  int y = x + 2;
  return y;
}
