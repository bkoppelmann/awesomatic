{Greatest common divisor}
program GCD;
var
  x,y: integer;
begin
read(x);
while x<>0 do
  begin read(y);
  while x<>y do if x>y then x:=x-y else y:=y-x;
  write(x);
  read(x);
  end;
end.
