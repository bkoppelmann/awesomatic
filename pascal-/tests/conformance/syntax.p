{Test syntax analysis}
program Syntax;
  const
    a = 1; b = a;
  type
    T = array [1..2] of integer;
    U = record f, g: integer; h: boolean end;
    V = record f: integer end;
  var
    x, y: T; z: U;
  procedure P(var x: integer; y: boolean);
    const a = 1;
    procedure Q(x: integer);
      type T = array [a..2] of integer;
      begin
      x := -1;
      x := x;
      x := (2-1)*(2+1) div 2 mod 2;
      if x < x then while x=x do Q(x);
      if x > x then while x <= x do P(X, false)
      else if not (x<>x) then { Empty}
      end;
    begin if x>=x then y:= true end;
  procedure R;
  var x: T;
  begin x[1]:=5 end;
begin z.f:=6 end.
