int
sqrt(int num)
{ int low = 0, high = num + 1;
  while (!(low + 1 == high))
    { int diff = (low + high) / 2, sq = diff * diff;
      if (sq > num) high = diff ; else low = diff;
    }
  return low;
}

