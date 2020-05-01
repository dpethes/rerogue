unit dc2_simple_api;
{$mode objfpc}{$H+}
{define use_zstream}

interface

uses
  Classes, SysUtils,
  {$ifdef use_zstream}zstream,{$endif}
  dc2core, dc2_encoder;

type
  { TLzEncoder }
  TLzEncoder = class
  private
      _ctx: TDc2Encoder;
      _encoded_size: integer;

  public
      constructor Create(const compression_level: byte = 2);
      destructor Destroy; override;
      function EncodeBytesToStream(const src: pbyte; const size: integer; var dest: TMemoryStream): integer;
      procedure WriteStats;
  end;

implementation

{ TLzEncoder }

constructor TLzEncoder.Create(const compression_level: byte);
begin
  _ctx := TDc2Encoder.Create(compression_level);
  _encoded_size := 0;
end;

destructor TLzEncoder.Destroy;
begin
  inherited Destroy;
  _ctx.free;
end;

function TLzEncoder.EncodeBytesToStream(const src: pbyte; const size: integer; var dest: TMemoryStream): integer;
var
  src_buffer: pbyte;
  chunk_size: integer;
  bytes_to_process: integer;
  encdata: TEncodedSlice;
  {$ifdef use_zstream}
  zs: Tcompressionstream;
  {$endif}
begin
  {$ifdef use_zstream}
  zs := Tcompressionstream.create(cldefault, dest, true);
  zs.WriteBuffer(src^, size);
  zs.Free;
  result := _encoded_size;
  exit;
  {$endif}
  _encoded_size := 0;
  src_buffer := src;
  chunk_size := MAX_BLOCK_SIZE;
  bytes_to_process := size;

  while bytes_to_process > 0 do begin
      if bytes_to_process <= chunk_size then begin
          chunk_size := bytes_to_process;
          _ctx.SetLastSlice();
      end;

      _ctx.EncodeSlice(src_buffer, chunk_size, encdata);
      dest.Write(encdata.data^, encdata.size);
      _encoded_size += encdata.size;

      src_buffer += chunk_size;
      bytes_to_process -= chunk_size;
  end;

  result := _encoded_size;
end;

procedure TLzEncoder.WriteStats;
begin
  //if _ctx^.stats.onb > 0 then
  //    writeln('avg. offset: ', _ctx^.stats.osum / _ctx^.stats.onb:8:1);
  //if _ctx^.stats.mnb > 0 then
  //    writeln('avg. match : ', _ctx^.stats.msum / _ctx^.stats.mnb:8:2);
  //with _ctx^.stats do
  //    writeln('block types (raw/fix/dyn): ', blocks[0]:6, blocks[1]:6, blocks[2]:6);
  //writeln('deflate bytes: ', _encoded_size);
end;

end.

