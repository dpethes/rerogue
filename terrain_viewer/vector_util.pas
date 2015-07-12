unit vector_util;
{$mode objfpc}{$H+}

interface

uses
  matrix;

function GetNormal(v0, v1, v2: Tvector3_single): Tvector3_single;

implementation

//cross product + normalize
function GetNormal(v0, v1, v2: Tvector3_single): Tvector3_single;
var
  a, b: Tvector3_single;
  len: single;
begin
  a := v0 - v1;
  b := v1 - v2;

  result.data[0] := (a.data[1] * b.data[2]) - (a.data[2] * b.data[1]);
  result.data[1] := (a.data[2] * b.data[0]) - (a.data[0] * b.data[2]);
  result.data[2] := (a.data[0] * b.data[1]) - (a.data[1] * b.data[0]);

  len := result.length;
  if len = 0 then len := 1;

  result /= len;
end;

end.

