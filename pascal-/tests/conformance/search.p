Program Search;
const n=100;
type table=array[1..n] of integer;
var A: table; i,x: integer; yes: Boolean;

procedure search(value: integer; var found: Boolean; var index: integer);
  var limit: integer;
  begin index:=1; limit:=n;
  while index<limit do
    if A[index]=value then limit:=index else index:=index+1;
  found:=A[index]=value
  end;

begin {input table} i:=1;
while i<=n do
  begin read(A[i]); i:=i+1 end;
{test search} read(x);
while x<>0 do
  begin search(x,yes,i);
  write(x);
  if yes then write(i);
  read(x);
  end
end.
