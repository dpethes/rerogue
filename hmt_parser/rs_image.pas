unit rs_image;
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TRGB = array[0..2] of byte;
  PRGB = ^TRGB;
  TPalette = array[0..255] of TRGB;

  TRSImage = record
      data_size: integer;
      width, height: integer;
      type_: byte;
      sampleBits: byte;
      paletteEntries: integer;
      pixels: pbyte;
      samples: pbyte;
      palette: TPalette;
  end;

type
  TImageDescription = record
      palette_entries: integer;
      sample_bits: integer;
      //alpha: byte;
  end;

procedure LoadPalette(var image: TRSImage; var f: TMemoryStream);
procedure LoadSamples(var image: TRSImage; var f: TMemoryStream);
procedure DecodePixels(var img: TRSImage);

//**************************************************************************************************
implementation

procedure Unpack4To8bit(const src: PByte; const samples: integer; const dst: PByte);
var
  i: Integer;
  v: byte;
begin
  for i := 0 to samples div 2 - 1 do begin
      v := src[i];
      dst[i * 2    ] := ((v shr 4) and %1111) shl 4;
      dst[i * 2 + 1] := (v and %1111) shl 4;
  end;
end;


procedure Unpack4bitTo24bitRGB(const src: PByte; const size: integer; const dst: PByte; const pal: TPalette);
var
  i: Integer;
  index: integer;
  dest: PRGB;
begin
  dest := PRGB(dst);
  for i := 0 to size div 2 - 1 do begin
      index := src[i];
      dest[i * 2    ] := pal[(index shr 4) and 15];
      dest[i * 2 + 1] := pal[index and 15];
  end;
end;


procedure Unpack8bitTo24bitRGB(const src: PByte; const size: integer; const dst: PByte; const pal: TPalette);
var
  i: Integer;
  index: integer;
  dest: PRGB;
begin
  dest := PRGB(dst);
  for i := 0 to size - 1 do begin
      index := src[i];
      dest[i] := pal[index];
  end;
end;


procedure UseOddBytes(const src: PByte; const size: integer; const dst: pbyte);
var
  i: integer;
begin
  for i := 0 to size - 1 do begin
      dst[i] := src[i * 2 + 1];
  end;
end;


procedure DecodePixels(var img: TRSImage);
var
  size: integer;
begin
  img.pixels := nil;
  if not(img.type_ in [0, 1, 2, 3, 4, 5]) then exit;

  if img.sampleBits = 32 then begin
      size := img.width * img.height * 4;
      img.pixels := GetMem(size);
      Move(img.samples^, img.pixels^, size);
  end;

  if img.sampleBits = 4 then begin
      //4bit grayscale
      if img.paletteEntries = 0 then begin
          size := img.width * img.height;
          img.pixels := GetMem(size);
          Unpack4To8bit(img.samples, size, img.pixels);
      end;
      //24bit RGB palettized
      if img.paletteEntries = 16 then begin
          size := img.width * img.height;
          img.pixels := GetMem(size * 3);
          Unpack4bitTo24bitRGB(img.samples, size, img.pixels, img.palette);
      end;
  end;

  if img.sampleBits = 8 then begin
      //8bit grayscale
      if img.paletteEntries = 0 then begin
          size := img.width * img.height;
          img.pixels := GetMem(size);
          move(img.samples^, img.pixels^, size);
      end;
      //24bit RGB palettized
      if img.paletteEntries = 256 then begin
          size := img.width * img.height;
          img.pixels := GetMem(size * 3);
          Unpack8bitTo24bitRGB(img.samples, size, img.pixels, img.palette);
      end;
  end;

  if img.sampleBits = 16 then begin
      size := img.width * img.height;
      img.pixels := GetMem(size);
      UseOddBytes(img.samples, size, img.pixels);
  end;
end;


procedure LoadPalette(var image: TRSImage; var f: TMemoryStream);
var
  entries: integer;
begin
  entries := image.paletteEntries;
  case entries of
      16, 256: f.ReadBuffer(image.palette, entries * 3); //RGB
  end;
end;


procedure LoadSamples(var image: TRSImage; var f: TMemoryStream);
var
  sample_bits: integer;
  size: integer;
begin
  sample_bits := image.sampleBits;
  size := image.width * image.height * sample_bits div 8;
  image.samples := getmem(size);
  f.ReadBuffer(image.samples^, size);
  if image.type_ = 2 then
      f.ReadBuffer(image.samples^, size div 4);
end;


end.

