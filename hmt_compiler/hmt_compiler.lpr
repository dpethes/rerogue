program hmt_compiler;

uses
  sysutils,
  hmt_parser;

var
  hmt: THmtFile;

procedure pgm_read(const fname: string; var p: pbyte; var w, h: word);
var
  f: file;
  c: char;
  magic: array [0..1] of char;
  s: string;
begin
  if not FileExists(fname) then begin
      writeln('pgm_read: subor neexistuje: ', fname);
      halt;
  end;
  s := '';
  AssignFile (f, fname);
  Reset (f, 1);

  //precitaj magic id
  blockread (f, magic, 2);
  if (magic <> 'P5') then begin
      writeln (stderr, 'error: bad input type');
      halt();
  end;
  blockread (f, c, 1);

  //sirka
  blockread (f, c, 1);
  while not(c = ' ') do begin
      s := s + c;
      blockread (f, c, 1);
  end;
  w := StrToInt(s);
  //vyska
  s := '';
  while not(c in [#10, #13]) do begin
      s := s + c;
      blockread (f, c, 1);
  end;
  h := StrToInt(s);
  //maxval ignorujeme
  repeat blockread (f, c, 1) until (c in [#10, #13]);

  //alokuj a nacitaj data
  GetMem (p, w * h);
  blockread (f, p^, w * h);
  CloseFile (f);
end;

procedure Pack8To4bit(const src: PByte; const samples: integer; const dst: PByte);
var
  i: Integer;
  a, b: byte;
begin
  for i := 0 to samples div 2 - 1 do begin
      a := src[i * 2];
      b := src[i * 2 + 1];
      dst[i] := a or (b shr 4);
  end;
end;

procedure HmtAddMaterial(const mat_name: string);
var
  mat: THmtMaterial;
  i: Integer;
begin
  mat.hex_a := $a;
  mat.type_ := 2;
  mat.tex_index := 0;
  mat.unknown_float1 := 1.0;
  mat.unknown_float2 := 1.0;
  for i := 0 to Length(mat_name) do begin
      mat.name[i] := byte( mat_name[i+1] );
  end;

  if mat_name = 'tie_wing' then
      mat.type_ := 1;

  hmt.material_count += 1;
  SetLength(hmt.materials, hmt.material_count);
  hmt.materials[hmt.material_count - 1] := mat;
end;

procedure HmtAddTexture(const fname, tex_name: string);
var
  w, h: word;
  tex: THmtTexture;
  i: Integer;
  pixbuf: pbyte;
begin
  pgm_read(fname, pixbuf, w, h);
  tex.image.pixels := getmem(w*h);
  Pack8To4bit(pixbuf, w*h, tex.image.pixels);
  freemem(pixbuf);

  tex.palette_offset := 0;
  tex.width := w;
  tex.height := h;
  for i := 0 to Length(tex_name) do begin
      tex.name[i] := byte( tex_name[i+1] );
  end;

  hmt.texture_count += 1;
  SetLength(hmt.textures, hmt.texture_count);
  hmt.textures[hmt.texture_count - 1] := tex;
end;

procedure WriteHmt(const fname: string);
var
  f: File;
  mat: THmtMaterial;
  tex: THmtTexture;
  i, k: Integer;
  zero: integer;
  texdata_offset: integer;
  texformat: array[0..3] of byte;
  alpha: longword;
begin
  hmt.texture_offset := hmt.material_count * 36 + 8;
  zero := 0;
  AssignFile(f, fname);
  Rewrite(f, 1);
  BlockWrite(f, hmt.material_count, 4);
  BlockWrite(f, hmt.texture_offset, 4);
  for i := 0 to hmt.material_count - 1 do begin
      mat := hmt.materials[i];
      BlockWrite(f, mat.type_, 2);
      BlockWrite(f, mat.tex_index, 2);
      BlockWrite(f, mat.unknown_float1, 4);
      BlockWrite(f, mat.unknown_float2, 4);
      BlockWrite(f, zero, 4);
      BlockWrite(f, mat.hex_a, 4);
      BlockWrite(f, mat.name, 16);
  end;

  //textures
  BlockWrite(f, hmt.texture_count, 4);
  texdata_offset := FilePos(f) + hmt.texture_count * 52;

  for i := 0 to hmt.texture_count - 1 do begin
      tex := hmt.textures[i];
      k := texdata_offset + 16;
      BlockWrite(f, k, 4);
      for k := 1 to 7 do
          BlockWrite(f, zero, 4);
      BlockWrite(f, zero, 4);   //palette offset, 0 = no palette
      BlockWrite(f, texdata_offset, 4);  //texname offset
      BlockWrite(f, tex.width, 2);
      BlockWrite(f, tex.height, 2);
      //format
      texformat[0] := 1;  //1?
      texformat[1] := 0;  //?
      texformat[2] := 4;  //subtype
      texformat[3] := $40;  //?
      BlockWrite(f, texformat, 4);
      alpha := $80808080;
      BlockWrite(f, alpha, 4);  //4B RGBA transparent color?
//      texdata_offset += pixsize;
  end;

  //texdata
  for i := 0 to hmt.texture_count - 1 do begin
      tex := hmt.textures[i];
      BlockWrite(f, tex.name, 16);
      BlockWrite(f, tex.image.pixels^, tex.width * tex.height div 2);
  end;
  Closefile(f);
end;

const
  mats: array[0..12] of string[16] = (
      'mat_0',
      'tie_hull_bk',
      'tie_hatch_top',
      'tie_hull_frt',
      'tie_wingdet',
      'arm_frt',
      'tie_wing',
      'mat_7',
      'tie_hull_bk_lod',
      'tie_hull_frt_lo',
      'tie_wing_lod',
      'mat_11',
      'mat_12'
    );

var
  i: integer;
begin
  hmt.material_count := 0;
  hmt.texture_count := 0;
  for i := 0 to 12 do
      HmtAddMaterial(mats[i]);
  HmtAddTexture('tie_wing.pgm', 'tie_wing');
  WriteHmt('tiefighter.HMT');
end.

