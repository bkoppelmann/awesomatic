{Kind errors}
program KindError;
  const
    a = integer;
  type
    T = array [2..1] of integer;
    U = record f: a end;
  var
    x: integer;
    y: U;
    z: false;
  procedure P(var x: integer; y: true);
    begin end;
  begin
  x[1] := 1;
  x.f := 1;
  P(false, true);
  x := P;
  false := true;
  y.g := 1;
  end.
