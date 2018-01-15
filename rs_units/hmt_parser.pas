unit hmt_parser;
{$mode objfpc}{$H+}

interface

uses
  sysutils, Classes,
  rs_image;

type
  THmtMaterial = record
      type_: shortint;
      tex_index: shortint;
      unknown_float1, unknown_float2: single;
      zero: integer;
      hex_a: integer;
      name: array[0..15] of byte;
      name_string: string;
  end;

  THmtTexture = record
      data_offset: integer;
      palette_offset: integer;
      name_offset: integer;
      width, height: word;
      name: array[0..15] of byte;
      name_string: string;
      image: TRSImage;
  end;

  THmtFile = record
      material_count: integer;
      texture_offset: integer;
      texture_count: integer;
      materials: array of THmtMaterial;
      textures: array of THmtTexture;
  end;

  function ParseHmtFile(f: TMemoryStream): THmtFile;

//**************************************************************************************************
implementation

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


procedure ReadTexture(var tex: THmtTexture; var f: TMemoryStream);
var
  image: TRSImage;
  buf: array[0..27] of byte;
  description: TImageDescription;
  color_rgba: longword;
  pos: int64;
  u0, u1, bits_per_sample: byte;
begin
  tex.data_offset := f.ReadDWord;
  f.ReadBuffer(buf, 28);
  tex.palette_offset := f.ReadDWord;
  tex.name_offset := f.ReadDWord;
  tex.width := f.ReadWord;
  tex.height := f.ReadWord;

  u0 := f.ReadByte; //always 1 ?
  bits_per_sample := f.ReadByte;
  image.type_ := f.ReadByte;
  u1 := f.ReadByte;
  color_rgba := f.ReadDWord; //unused?

  pos := f.Position;
  f.Seek(tex.name_offset, TSeekOrigin.soBeginning);
  f.ReadBuffer(tex.name, 16);
  tex.name_string := NameToString(tex.name);
  f.Seek(pos, TSeekOrigin.soBeginning);

  description := ImageDescription[image.type_];
  image.sampleBits := description.sample_bits;
  image.paletteEntries := description.palette_entries;
  image.width := tex.width;
  image.height := tex.height;

  //fix stride in some images?
  //if (image.width and 1) > 1 then image.width += 1;
  //if (image.type_ = 4) and (image.sampleBits = 4) then
  //    if (image.width and 1) = 1 then begin
  //        image.width += 1;
  //        writeln('fix width');
  //    end;

  writeln('name: ', tex.name_string);
  writeln('size: ', tex.width, 'x', tex.height);
  writeln('subtype: ', image.type_);
  writeln('sample bits: ', image.sampleBits);
  writeln('palette offset: ', tex.palette_offset);
  writeln('data offset: ', tex.data_offset);
  writeln('u0:', u0, ' u1:', u1);

  if tex.palette_offset > 0 then begin
      writeln('palette entries: ', image.paletteEntries);
      f.Seek(tex.palette_offset, TSeekOrigin.soBeginning);
      LoadPalette(image, f);
  end;
  f.Seek(tex.data_offset, TSeekOrigin.soBeginning);
  LoadSamples(image, f);
  DecodePixels(image);

  f.Seek(pos, TSeekOrigin.soBeginning);
  writeln;
  tex.image := image;
end;


procedure ReadMaterial(var mat: THmtMaterial; var f: TMemoryStream);
begin
  mat.type_ := f.ReadWord;
  mat.tex_index := f.ReadWord;
  mat.unknown_float1 := f.ReadDWord;
  mat.unknown_float2 := f.ReadDWord;
  mat.zero := f.ReadDWord;
  mat.hex_a := f.ReadDWord;
  f.ReadBuffer(mat.name, 16);
  mat.name_string := NameToString(mat.name);

  writeln(mat.name_string);
  if (mat.zero <> 0) or (mat.hex_a <> $A) then
      writeln('unusual file');
  writeln(' type1: ', mat.type_);
  writeln(' type2: ', mat.tex_index);
end;


function ParseHmtFile(f: TMemoryStream): THmtFile;
var
  hmt: THmtFile;
  i: Integer;
begin
  //read main info
  hmt.material_count := f.ReadDWord;
  hmt.texture_offset := f.ReadDWord;

  //read materials
  writeln('materials: ', hmt.material_count);
  SetLength(hmt.materials, hmt.material_count);
  for i := 0 to hmt.material_count - 1 do begin
      ReadMaterial(hmt.materials[i], f);
  end;

  //read textures
  f.Seek(hmt.texture_offset, TSeekOrigin.soBeginning);
  hmt.texture_count := f.ReadDWord;
  writeln('textures: ', hmt.texture_count);
  if hmt.texture_count > 0 then begin
      SetLength(hmt.textures, hmt.texture_count);
      for i := 0 to hmt.texture_count - 1 do begin
          ReadTexture(hmt.textures[i], f);
      end;
  end;

  result := hmt;
end;

end.

