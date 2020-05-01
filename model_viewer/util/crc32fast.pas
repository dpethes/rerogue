(*******************************************************************************
crc32 slicing by 4 - http://create.stephan-brumme.com/crc32/

*******************************************************************************)
unit crc32fast;
{$mode objfpc}{$H+}

interface

function crc32(crc: longword; const data: Pbyte; length: longword): longword;


implementation

var
  Crc32Table: array[0..3, 0..255] of longword;

procedure InitTable;
const
  POLY = $EDB88320;
var
  i, j: integer;
  crc: longword;
begin
  for i := 0 to 255 do begin
      crc := i;
      for j := 0 to 7 do begin
          crc := (crc >> 1) xor ((crc and 1) * POLY);
      end;
      Crc32Table[0, i] := crc;
  end;

  for i := 0 to 255 do begin
      Crc32Table[1, i] := (Crc32Table[0, i] >> 8) xor Crc32Table[0, Crc32Table[0, i] and $ff];
      Crc32Table[2, i] := (Crc32Table[1, i] >> 8) xor Crc32Table[0, Crc32Table[1, i] and $ff];
      Crc32Table[3, i] := (Crc32Table[2, i] >> 8) xor Crc32Table[0, Crc32Table[2, i] and $ff];
  end;
end;


function crc32(crc: longword; const data: Pbyte; length: longword): longword;
var
  current: plongword;
  current_byte: pbyte;
  x: longword;
begin
  if data = nil then
    exit(0);

  crc := crc xor $FFFFFFFF;
  current := plongword(data);
  while length >= 4 do begin
      x := crc xor current^;
      current += 1;
      crc := Crc32Table[0, (x >> 24) and $ff] xor
             Crc32Table[1, (x >> 16) and $ff] xor
             Crc32Table[2, (x >>  8) and $ff] xor
             Crc32Table[3, (x >>  0) and $ff];
      length -= 4;
  end;

  current_byte := pbyte(current);
  while length > 0 do begin
      crc := (crc >> 8) xor Crc32Table[0, (crc and $ff) xor current_byte^];
      current_byte += 1;
      length -= 1;
  end;

  crc := crc xor $FFFFFFFF;
  result := crc;
end;


initialization
InitTable;

end.

