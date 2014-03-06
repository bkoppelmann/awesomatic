#include <stdio.h>
#include <stdlib.h>

#define Variable(Level,Displ) \
  {int x = b, l = Level; while (l--) x = St[x]; St[++s] = x + Displ; }

#define ValParam(Level,Displ) \
  {int x = b, l = Level; while (l--) x = St[x]; St[++s] = St[x+2] + Displ; }

#define VarParam(Level,Displ) \
  {int x = b, l = Level; while (l--) x = St[x]; St[++s] = St[St[x+2] + Displ]; }

#define Index(Lower,Upper,Length,LineNo) \
  {int i = St[s--]; if (i<Lower || i>Upper) RangeErr(LineNo); \
  St[s] += (i - Lower) * Length; }

#define Field(Displ) St[s] = St[s] + Displ;

#define Constant(Value) St[++s] = Value;

#define Value(Length) \
  {int *p = St + s, *q = St + *p, i = Length; s += Length - 1; \
  while (i--) *p++ = *q++; }

#define Less s--; St[s] = (St[s]<St[s+1]?1:0);
#define Equal s--; St[s] = (St[s]==St[s+1]?1:0);
#define Greater s--; St[s] = (St[s]>St[s+1]?1:0);
#define NotLess s--; St[s] = (St[s]>=St[s+1]?1:0);
#define NotEqual s--; St[s] = (St[s]!=St[s+1]?1:0);
#define NotGreater s--; St[s] = (St[s]<=St[s+1]?1:0);

#define Add s--; St[s] += St[s+1];
#define Subtract s--; St[s] -= St[s+1];
#define Multiply s--; St[s] *= St[s+1];
#define Divide s--; St[s] /= St[s+1];
#define Modulo s--; St[s] %= St[s+1];
#define Minus St[s] = -St[s];

#define And s--; St[s] &= St[s+1];
#define Or s--; St[s] |= St[s+1];
#define Not St[s] = 1 - St[s];

#define Assign(Length) \
  s -= Length + 1; {int *p = St + St[s+1], *q = St + s+2, i = Length; \
  while (i--) *p++ = *q++; }

#define DefAddr(Label) Label:
#define Do(Label) if (!St[s--]) goto Label;
#define Goto(Label) goto Label;

#define Read (void)scanf(" %d", &St[St[s--]]);
#define Write (void)printf("%d\n", St[s--]);

#define ProcCall(Level,Label) \
  {int x = b, l = Level; while (l--) x = St[x]; \
  St[s+1] = x; St[s+2] = b; b = s + 1; (void)Label(); }

#define Procedure(ParLength,VarLength,TempLength,Label,Line) \
  Label(){St[b+2] = b - ParLength; s += VarLength;

#define EndProc(Length) \
  s = b - Length -1; b = St[b + 1]; return; }

#define Program(VarLength,TempLength,Line) \
  main(){b = 0; s = VarLength - 1;

#define EndProg return 0; }

#define MaxStack 8192

void
RangeErr(Line)
int Line;
{
  (void)fprintf(stderr, "Index out of bounds at line %d\n", Line);
  exit(1);
}

static int St[MaxStack], b, s;
