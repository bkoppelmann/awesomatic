{Type errors}
program TypeError;
  type T = array [1..10] of integer;
  var x: integer; y: boolean; z: T;
  procedure P(x: integer);
    begin end;
  begin
  y := not 1 and 2;
  y := false * true;
  z := z mod z;
  x := 1 or 2;
  y := false + true - true;
  z[true] := 3;
  P(true);
  end.
