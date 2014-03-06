{Miscellaneous errors}
program MiscError;
  const
    b = c;
  type
    T = array [5..1] of integer;
    U = record x: true end;
    V = array [false..true] of integer;
  var
    x, y, x: integer;
    z: V;
  begin
  y := 1 and 2;
  y := 2 * (3+4;
  z[1] := &2;
  end.
