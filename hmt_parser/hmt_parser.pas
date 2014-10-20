unit hmt_parser;
{$mode objfpc}{$H+}

interface

uses
  sysutils, Classes,
  rs_image;

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
      palette_offset: integer;
      name_offset: integer;
      width, height: word;
      name: array[0..15] of byte;
  end;

  THmtFile = record
      material_count: integer;
      texture_offset: integer;
      texture_count: integer;
      materials: array of THmtMaterial;
      textures: array of THmtTexture;
  end;

  function ParseHmtFile(const fname: string): THmtFile;

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
const
  ImageDescription: array[0..5] of TImageDescription = (
      (palette_entries: 16;  sample_bits: 4),
      (palette_entries: 256; sample_bits: 8),
      (palette_entries: 0; sample_bits: 16),
      (palette_entries: 0; sample_bits: 32),
      (palette_entries: 0; sample_bits: 4),
      (palette_entries: 0; sample_bits: 16)
    );
var
  image: TRSImage;
  buf: array[0..27] of byte;
  description: TImageDescription;
  bpp: byte;
  color_rgba: integer;
  pos: int64;
begin
  tex.data_offset := f.ReadDWord;
  f.ReadBuffer(buf, 28);
  tex.palette_offset := f.ReadDWord;
  tex.name_offset := f.ReadDWord;
  tex.width := f.ReadWord;
  tex.height := f.ReadWord;

  f.ReadByte; //0x01
  bpp := f.ReadByte;
  image.type_ := f.ReadByte;
  f.ReadByte;
  color_rgba := f.ReadDWord;

  pos := f.Position;
  f.Seek(tex.name_offset, TSeekOrigin.soBeginning);
  f.ReadBuffer(tex.name, 16);
  f.Seek(pos, TSeekOrigin.soBeginning);

  description := ImageDescription[image.type_];
  image.sampleBits := description.sample_bits;
  image.paletteEntries := description.palette_entries;
  image.width := tex.width;
  image.height := tex.height;

  writeln(NameToString(tex.name));
  writeln('size: ', tex.width, 'x', tex.height);
  writeln('subtype: ', image.type_, ' bpp: ', bpp);
  writeln('sample bits: ', image.sampleBits);
  writeln('palette offset: ', tex.palette_offset);
  writeln('data offset: ', tex.data_offset);

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
end;


procedure ReadMaterial(var mat: THmtMaterial; var f: TMemoryStream);
begin
  mat.type1 := f.ReadWord;
  mat.type2 := f.ReadWord;
  mat.unknown_float1 := f.ReadDWord;
  mat.unknown_float2 := f.ReadDWord;
  mat.zero := f.ReadDWord;
  mat.hex_a := f.ReadDWord;
  f.ReadBuffer(mat.name, 16);

  writeln(NameToString(mat.name));
  if (mat.zero <> 0) or (mat.hex_a <> $A) then
      writeln('unusual file');
end;


function ParseHmtFile(const fname: string): THmtFile;
var
  f: TMemoryStream;
  hmt: THmtFile;
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

  //read materials
  writeln('materials: ', hmt.material_count);
  SetLength(hmt.materials, hmt.material_count);
  for i := 0 to hmt.material_count - 1 do begin
      ReadMaterial(hmt.materials[i], f);
  end;

  if hmt.texture_count = 0 then begin
      result := hmt;
      f.Free;
      exit;
  end;

  //read textures
  writeln('textures: ', hmt.texture_count);
  f.Seek(hmt.texture_offset + 4, TSeekOrigin.soBeginning);
  SetLength(hmt.textures, hmt.texture_count);
  for i := 0 to hmt.texture_count - 1 do begin
      ReadTexture(hmt.textures[i], f);
  end;

  f.Free;
  result := hmt;
end;

end.

