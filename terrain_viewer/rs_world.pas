unit rs_world;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, gvector, rs_dat;

const
  TEX_WIDTH = 64;
  TEX_HEIGHT = 64;
  TEXTURE_FNAME = 'level_tex.pnm';

type
  TRGB = array[0..2] of byte;
  PRGB = ^TRGB;
  TPalette_4bit = array[0..15] of TRGB;

  TTile = packed record
      texture_index: word;
      unknown_attrib: byte;
      height_lo: shortint;
      height_hi: shortint;
      heights: array[0..24] of shortint;
  end;
  PTile = ^TTile;

  THeightmap = record
      y_scale: single;
      width, height: word;
      blk: pword;
      tile_count: integer;
      tiles: PTile;
      texture_count: integer;
      textures: array of pbyte;
      texture_index_map: array of integer;
  end;

  TLevelListItem = record
      name: string;
      hmp: PRsDatFileNode;
      texture: PRsDatFileNode;
      texture_index: PRsDatFileNode;
  end;
  TLevelList = specialize TVector<TLevelListItem>;


  { TWorld }

  TWorld = class
    private
      world_texture: pbyte;
      height_texture: pbyte;

      procedure LoadTextures(tex_node, texindex_node: PRsDatFileNode);
      procedure LoadHeightmap(node: PRsDatFileNode);

    public
      heightmap: THeightmap;
      vertex_count: integer;

      property TileWidth: word read heightmap.width;
      property TileHeight: word read heightmap.height;

      procedure LoadFromNodes(level: TLevelListItem);

      constructor Create;
      destructor Destroy; override;
  end;

procedure pnm_save(const fname: string; const p: pbyte; const w, h: integer);

//**************************************************************************************************
implementation

procedure pnm_save(const fname: string; const p: pbyte; const w, h: integer);
var
  f: file;
  c: PChar;
Begin
  c := PChar(format('P6'#10'%d %d'#10'255'#10, [w, h]));
  AssignFile (f, fname);
  Rewrite (f, 1);
  BlockWrite (f, c^, strlen(c));
  BlockWrite (f, p^, w * h * 3);
  CloseFile (f);
end;

procedure pgm_save(fname: string; p: pbyte; w, h: integer) ;
var
  f: file;
  c: PChar;
Begin
  c := PChar(format('P5'#10'%d %d'#10'255'#10, [w, h]));
  AssignFile (f, fname);
  Rewrite (f, 1);
  BlockWrite (f, c^, strlen(c));
  BlockWrite (f, p^, w * h);
  CloseFile (f);
end;

procedure convert_4bit_to_24bit(const indices: PByte; const w, h: Word; const image: PByte; const pal: TPalette_4bit);
var
  i: Integer;
  index: integer;
  dst: PRGB;
begin
  dst := PRGB(image);
  for i := 0 to w * h div 2 - 1 do begin
      index := indices[i];
      dst[i * 2    ] := pal[(index shr 4) and 15];
      dst[i * 2 + 1] := pal[index and 15];
  end;
end;

procedure CopyTexToXY(image: PByte; texture: PByte; const x, y, stride: integer);
var
  i: integer;
  src, dst: pbyte;
begin
  src := texture;
  dst := image + y * stride + x * 3;
  for i := 0 to TEX_HEIGHT - 1 do begin
      move(src^, dst^, TEX_WIDTH * 3);
      dst += stride;
      src += TEX_WIDTH * 3;
  end;
end;

procedure CopyTileToXY(image: PByte; tile: PByte; const x, y, stride: integer);
var
  i: integer;
  src, dst: pbyte;
begin
  src := tile + 5 * 4;
  dst := image + y * stride + x;
  for i := 0 to 3 do begin
      move(src^, dst^, 4);
      dst += stride;
      src -= 5;
  end;
end;

{ TWorld }

procedure TWorld.LoadTextures(tex_node, texindex_node: PRsDatFileNode);
var
  f: TMemoryStream;
  buf: pbyte;
  tex_size: integer;
  i: Integer;
  palette: TPalette_4bit;
  image: pbyte;
  palette_size: Integer;
  texture_count: integer;
begin
  f := TMemoryStream.Create;
  f.WriteBuffer(tex_node^.Data^, tex_node^.size);
  f.Seek(0, soBeginning);

  palette_size := 48;  //16x RGB
  tex_size := TEX_WIDTH * TEX_HEIGHT div 2;
  texture_count := f.Size div (tex_size + palette_size);
  //writeln('texture_count: ', texture_count);

  SetLength(heightmap.textures, texture_count);
  heightmap.texture_count := texture_count;

  buf := getmem(tex_size);
  for i := 0 to texture_count - 1 do begin
      image := getmem(TEX_WIDTH * TEX_HEIGHT * 3);
      f.Read(buf^, tex_size);
      f.Read(palette, palette_size);
      convert_4bit_to_24bit(buf, TEX_WIDTH, TEX_HEIGHT, image, palette);
      heightmap.textures[i] := image;
  end;
  freemem(buf);
  f.Free;

  f := TMemoryStream.Create;
  f.WriteBuffer(texindex_node^.Data^, texindex_node^.size);
  f.Seek(0, soBeginning);

  texture_count := f.Size div 4 - 1;  //should match previous texture_count from texture atlas?
  SetLength(heightmap.texture_index_map, texture_count);
  f.Read(heightmap.texture_index_map[0], texture_count * 4);

  f.Free;
end;

procedure TWorld.LoadHeightmap(node: PRsDatFileNode);
var
  f: TMemoryStream;
  buffer: array[0..15] of byte;
  tile_offset: integer;
  blk: pword;
  blk_size: integer;
  tile_count: word;
  i: integer;
begin
  f := TMemoryStream.Create;
  f.WriteBuffer(node^.Data^, node^.size);
  f.Seek(0, soBeginning);

  //header
  f.Read(buffer, 12);
  f.Read(buffer, 4);
  f.Read(heightmap.y_scale, 4);  //float
  f.Read(buffer, 4);
  tile_count := f.ReadWord;
  f.Read(buffer, 2);
  tile_offset  := f.ReadDWord;
  f.Read(buffer, 4);
  heightmap.width := f.ReadWord;
  heightmap.height := f.ReadWord;

  //blocks / tile indices
  blk_size := heightmap.width * heightmap.height * 2;
  blk := getmem(blk_size);
  f.Read(blk^, blk_size);
  heightmap.blk := blk;

  //tiles
  //writeln('filepos: ', FilePos(f)); writeln('tile pos: ', tile_offset);
  f.Seek(tile_offset, soBeginning);
  heightmap.tile_count := tile_count;
  heightmap.tiles := getmem(tile_count * 30);
  for i := 0 to tile_count - 1 do
      f.Read(heightmap.tiles[i], 30);

  f.Free;
end;

procedure TWorld.LoadFromNodes(level: TLevelListItem);
var
  i: Integer;
begin
  LoadHeightmap(level.hmp);
  LoadTextures(level.texture, level.texture_index);
  for i := 0 to heightmap.tile_count - 1 do begin
      heightmap.tiles[i].texture_index := heightmap.texture_index_map[heightmap.tiles[i].texture_index];
  end;
end;

constructor TWorld.Create;
begin
  height_texture := nil;
end;

destructor TWorld.Destroy;
var
  tex: PByte;
begin
  if height_texture <> nil then Freemem(height_texture);
  for tex in heightmap.textures do
      freemem(tex);
  freemem(heightmap.tiles);
  freemem(heightmap.blk);
  heightmap.textures := nil;
  inherited Destroy;
end;

end.

