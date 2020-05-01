unit png_writer;
{$mode objfpc}{$H+}

interface

uses
  sysutils, classes,
  crc32fast, prediction, dc2_simple_api;

function ImageToPngStream(const p: pbyte; const w, h: integer): TMemoryStream;
procedure png_write(const fname: string; const p: pbyte; const w, h, bitdepth: integer);

implementation

const
  CompressionLevel = 2;

var
  //le global hack
  g_SamplesPerPixel: byte;  //use 3 for rgb, 1 for grayscale

function Adler32(const checksum: longword; const data: pbyte; const size: longword): longword;
const
  BASE = 65521;
  BLOCK_SIZE = 4*1024;
var
  i: integer;
  s1, s2: longword;
  k, blocks: integer;
  p: pbyte;

begin
  s1 := checksum and $ffff;
  s2 := (checksum shr 16) and $ffff;
  p := data;

  //process stream in blocks
  blocks := size div BLOCK_SIZE;
  for k := 0 to blocks - 1 do begin
      for i := 0 to BLOCK_SIZE - 1 do begin
          s1 += p[i];
          s2 += s1;
      end;
      s1 := s1 mod BASE;
      s2 := s2 mod BASE;
      p += BLOCK_SIZE;
  end;

  //final bytes
  for i := 0 to (size mod BLOCK_SIZE) - 1 do begin
      s1 += p[i];
      s2 += s1;
  end;
  s1 := s1 mod BASE;
  s2 := s2 mod BASE;

  result := s2 shl 16 + s1;
end;


procedure StreamWriteDataCRC(var f: TMemoryStream; const p: pbyte; const size: longword);
var
  checksum: longword;
begin
  checksum := crc32(0, nil, 0);
  checksum := NtoBE( crc32(checksum, p, size) );
  f.Write(checksum, 4);  //chunk crc
end;


procedure WriteHeader(var f: TMemoryStream; const width, height: integer);
const
  CHUNK_IHDR: array[0..3] of char = ('I','H','D','R');
  HEADER_DATA_SIZE: longword = 13;
var
  chunk_start_position: integer;
  chunk_data: pbyte;
  chunk_size: integer;
begin
  f.WriteDWord(NtoBE(HEADER_DATA_SIZE));
  chunk_start_position := f.Position;
  f.WriteBuffer(CHUNK_IHDR, 4);  //id
  f.WriteDWord(longword(NtoBE(width)));    //width
  f.WriteDWord(longword(NtoBE(height)));   //height
  f.WriteByte(8);                //bits per component
  //color type: rgb, grayscale
  if g_SamplesPerPixel = 3 then
      f.WriteByte(2)
  else
      f.WriteByte(0);
  f.WriteByte(0);                //Compression method, Filter method, Interlace method
  f.WriteByte(0);
  f.WriteByte(0);
  chunk_data := pbyte(f.Memory) + chunk_start_position;
  chunk_size := f.Position - chunk_start_position;
  StreamWriteDataCRC(f, chunk_data, chunk_size);
end;


procedure WriteEnding(var f: TMemoryStream);
const
  CHUNK_IEND: array[0..3] of char = ('I','E','N','D');
begin
  f.WriteDWord(0);
  f.Write(CHUNK_IEND, 4);
  StreamWriteDataCRC(f, @CHUNK_IEND, 4);
end;


procedure CompressAndWriteData(var f: TMemoryStream; const pixels: pbyte; const width, height: integer);
var
  predictor: TPngPredict;
  data: pbyte;
  data_size: integer;
  stride: integer;
  checksum: longword;
  encoder: TLzEncoder;
begin
  //pixel prediction & proper data layout
  stride := width * g_SamplesPerPixel;
  data := predictor.PredictData(pixels, width, height, g_SamplesPerPixel);
  data_size := height * (stride + 1);

  //checksum for raw data
  checksum := 1;
  checksum := Adler32(checksum, data, data_size);

  //compression
  encoder := TLzEncoder.Create(CompressionLevel);
  encoder.EncodeBytesToStream(data, data_size, f);
  encoder.Free;
  freemem(data);

  f.WriteDWord(NtoBE(checksum));  //adler32
end;


procedure WritePixelData(var f: TMemoryStream; const w, h: integer; const p: pbyte);
const
  CHUNK_IDAT: array[0..3] of char = ('I','D','A','T');
var
  chunk_start_position: integer;
  chunk_data: pbyte;
  chunk_size: integer;
begin
  f.WriteDWord(0);                //data size placeholder
  chunk_start_position := f.Position;
  f.WriteBuffer(CHUNK_IDAT, 4);   //id
  f.WriteByte($78);               //zlib headers: compression method, flags
  f.WriteByte($DA);               //clevel, fdict, parity
  CompressAndWriteData(f, p, w, h);

  chunk_data := pbyte(f.Memory) + chunk_start_position;
  chunk_size := f.Position - chunk_start_position;
  StreamWriteDataCRC(f, chunk_data, chunk_size);

  //set proper chunk data size
  f.Seek(chunk_start_position - 4, soBeginning);
  chunk_size -= 4;
  f.WriteDWord(longword(NtoBE(chunk_size)));
  f.Seek(0, soEnd);
end;


function ImageToPngStream(const p: pbyte; const w, h: integer): TMemoryStream;
const
  HEAD_MAGIC: array[0..7] of byte = (137, 80, 78, 71, 13, 10, 26, 10);
var
  stream: TMemoryStream;
begin
  stream := TMemoryStream.Create;
  stream.Write(HEAD_MAGIC, 8);
  WriteHeader(stream, w, h);
  WritePixelData(stream, w, h, p);
  WriteEnding(stream);
  result := stream;
end;

procedure png_write(const fname: string; const p: pbyte; const w, h,
  bitdepth: integer);
var
  stream: TMemoryStream;
begin
  g_SamplesPerPixel := bitdepth div 8;
  stream := ImageToPngStream(p, w, h);
  stream.SaveToFile(fname);
  stream.Free;
end;


end.

