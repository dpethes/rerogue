program hmt_parser;

 uses
   Classes;

type
  THmtMaterial = record
      type1, type2: shortint;
      unknown_float1, unknown_float2: single;
      zero: integer;
      hex_a: integer;
      name: array[0..15] of byte;
  end;

  THmtTexture = record
      data_offset: integer;
      unknown: array[0..47] of byte;
  end;

  THmtFile = record
      material_count: integer;
      texture_offset: integer;
      texture_count: integer;
      materials: array of THmtMaterial;
      textures: array of THmtTexture;
  end;


function NameToString(name: array of byte): string;
var
  i: Integer;
begin
  result := '';
  for i := 0 to length(name) - 1 do begin
      if name[i] = 0 then break;
      result += char( name[i] );
  end;
end;

procedure ParseHmtFile(const fname: string);
var
  f: TMemoryStream;
  hmt: THmtFile;
  mat: THmtMaterial;
  tex: THmtTexture;
  i: Integer;
begin
  f := TMemoryStream.Create;
  f.LoadFromFile(fname);

  //read main info
  hmt.material_count := f.ReadDWord;
  hmt.texture_offset := f.ReadDWord;
  f.Seek(hmt.texture_offset, TSeekOrigin.soBeginning);
  hmt.texture_count := f.ReadDWord;
  f.Seek(8, TSeekOrigin.soBeginning);

  writeln('materials: ', hmt.material_count);
  writeln('textures: ', hmt.texture_count);
  writeln('  texture bytes: ', f.Size - sizeof(hmt.texture_count) - hmt.texture_offset);

  //read materials
  SetLength(hmt.materials, hmt.material_count);
  for i := 0 to hmt.material_count - 1 do begin
      mat.type1 := f.ReadWord;
      mat.type2 := f.ReadWord;
      mat.unknown_float1 := f.ReadDWord;
      mat.unknown_float2 := f.ReadDWord;
      mat.zero := f.ReadDWord;
      mat.hex_a := f.ReadDWord;
      f.ReadBuffer(mat.name, 16);

      hmt.materials[i] := mat;
  end;

  for mat in hmt.materials do begin
      writeln(NameToString(mat.name));
      if (mat.zero <> 0) or (mat.hex_a <> $A) then
          writeln('unusual file');
  end;

  //read textures
  if hmt.texture_count = 0 then
      exit;
  f.Seek(hmt.texture_offset + sizeof(hmt.texture_count), TSeekOrigin.soBeginning);
  SetLength(hmt.textures, hmt.texture_count);
  for i := 0 to hmt.texture_count - 1 do begin
      tex.data_offset := f.ReadDWord;
      f.ReadBuffer(tex.unknown, 48);
  end;
  
  f.Free;
end;

var
  fname: string;

begin
  if ParamCount < 1 then begin
      writeln ('no input file specified');
      exit;
  end;

  fname := ParamStr(1);
  writeln('parsing file: ', fname);
  try
      ParseHmtFile(fname);
  except
      writeln('parsing failed!');
  end;
  writeln('done.');
end.

