unit rs_world;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

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
      height_lo: byte;
      height_hi: byte;
      heights: array[0..24] of byte;
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

  TVertex3f = record
      x, y, z: single;
      u, v: single
  end;
  PVertex3f = ^TVertex3f;

  { TWorld }

  TWorld = class
    private
      heightmap: THeightmap;
      world_texture: pbyte;
      height_texture: pbyte;
      vertex_array: PVertex3f;
      vertex_count: integer;

      procedure LoadTextures(const tex_fname, texidx_fname: string);
      procedure LoadHeightmap(fname: string);
      procedure GenerateCompositeTexture;
      procedure HeightmapToTexture;
      procedure GenerateVertices;
      procedure WriteToObj(const objFname: string);

    public
      property TileWidth: word read heightmap.width;
      property TileHeight: word read heightmap.height;
      property MapScale: single read heightmap.y_scale;

      procedure LoadFromFiles(const hmp, tex, texmap: string);
      procedure ExportToObj(const objfname: string);
      procedure ExportToRaw(const rawfname: string);

      constructor Create;
      destructor Destroy; override;
  end;


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

procedure TWorld.LoadTextures(const tex_fname, texidx_fname: string);
var
  f: file;
  buf: pbyte;
  tex_size: integer;
  i: Integer;
  palette: TPalette_4bit;
  image: pbyte;
  palette_size: Integer;
  texture_count: integer;
begin
  AssignFile(f, tex_fname);
  reset(f, 1);

  palette_size := 48;  //16x RGB
  tex_size := TEX_WIDTH * TEX_HEIGHT div 2;
  texture_count := filesize(f) div (tex_size + palette_size);
  //writeln('texture_count: ', texture_count);

  SetLength(heightmap.textures, texture_count);
  heightmap.texture_count := texture_count;

  buf := getmem(tex_size);
  for i := 0 to texture_count - 1 do begin
      image := getmem(TEX_WIDTH * TEX_HEIGHT * 3);
      Blockread(f, buf^, tex_size);
      Blockread(f, palette, palette_size);
      convert_4bit_to_24bit(buf, TEX_WIDTH, TEX_HEIGHT, image, palette);
      heightmap.textures[i] := image;
  end;
  freemem(buf);
  CloseFile(f);

  AssignFile(f, texidx_fname);
  Reset(f, 1);

  texture_count := filesize(f) div 4 - 1;
  SetLength(heightmap.texture_index_map, texture_count);
  Blockread(f, heightmap.texture_index_map[0], texture_count * 4);

  CloseFile(f);
end;

procedure TWorld.LoadHeightmap(fname: string);
var
  f: file;
  buffer: array[0..15] of byte;
  tile_offset: integer;
  blk: pword;
  blk_size: integer;
  tile_count: word;
  i: integer;
  float: single;
begin
  AssignFile(f, fname);
  reset(f, 1);

  //header
  Blockread(f, buffer, 12);
  Blockread(f, float, 4);
  Blockread(f, heightmap.y_scale, 4);
  Blockread(f, float, 4);
  Blockread(f, tile_count, 2);   //tile count
  Blockread(f, buffer, 2);       //2B?
  Blockread(f, tile_offset, 4);  //tile offset
  Blockread(f, buffer, 4);       //offset?
  Blockread(f, heightmap.width, 2);
  Blockread(f, heightmap.height, 2);

  //blocks / tile indices
  blk_size := heightmap.width * heightmap.height * 2;
  blk := getmem(blk_size);
  Blockread(f, blk^, blk_size);
  heightmap.blk := blk;

  //tiles
  //writeln('tiles: ', tile_count);
  Seek(f, tile_offset);
  heightmap.tile_count := tile_count;
  heightmap.tiles := getmem(tile_count * 30);
  for i := 0 to tile_count - 1 do
      Blockread(f, heightmap.tiles[i], 30);

  CloseFile(f);
end;

procedure TWorld.GenerateCompositeTexture;
var
  image: pbyte;
  image_size: integer;
  x, y, stride: integer;
  tile_idx, texture_idx, texmap_idx: integer;
  texture: pbyte;
begin
  image_size := heightmap.width * heightmap.height * TEX_WIDTH * TEX_HEIGHT * 3;
  image := GetMem(image_size);
  stride := heightmap.width * TEX_WIDTH * 3;

  for y := 0 to heightmap.height - 1 do
      for x := 0 to heightmap.width - 1 do begin
          tile_idx := heightmap.blk[y * heightmap.width + x];

          texmap_idx := heightmap.tiles[tile_idx].texture_index;
          if texmap_idx > Length(heightmap.texture_index_map) - 1 then
              texmap_idx := 0;

          texture_idx := heightmap.texture_index_map[texmap_idx];
          texture := heightmap.textures[texture_idx];
          CopyTexToXY(image, texture, x * TEX_WIDTH, (heightmap.height - y - 1) * TEX_HEIGHT, stride);
      end;

  world_texture := image;
  pnm_save(TEXTURE_FNAME, image, heightmap.width * TEX_WIDTH, heightmap.height * TEX_HEIGHT);
end;

procedure TWorld.HeightmapToTexture;
const
  TILE_WIDTH = 4;
  SCALE = 128;
var
  x, y: integer;
  tile_idx: integer;
  i: integer;
  image_size: integer;
  image: pbyte;
begin
  image_size := heightmap.width * heightmap.height * TILE_WIDTH * TILE_WIDTH;
  image := GetMem(image_size);

  for y := 0 to heightmap.height - 1 do begin
      for x := 0 to heightmap.width - 1  do begin
          tile_idx := heightmap.blk[y * heightmap.width + x];

          CopyTileToXY(image, @(heightmap.tiles[tile_idx].heights),
                              x * TILE_WIDTH, (heightmap.height - y - 1) * TILE_WIDTH, heightmap.width * TILE_WIDTH);
      end;
  end;

  //scale
  for i := 0 to image_size - 1 do
      image[i] := byte(image[i] + SCALE);

  height_texture := image;
  //pgm_save('map_height.pgm', image, heightmap.width * TILE_WIDTH, heightmap.height * TILE_WIDTH);
end;

procedure TWorld.GenerateVertices;
const
  scale = 0.1;
var
  va_size: integer;
  x, y: integer;
  vert: TVertex3f;
  width_half, height_half: integer;
  i: integer;
  height_scale: single;
begin
  vertex_count := heightmap.width * 4 * heightmap.height * 4;
  va_size := vertex_count * SizeOf(TVertex3f);
  vertex_array := getmem(va_size);

  width_half  := heightmap.width * 2;     //half of width & height in vertex count
  height_half := heightmap.height * 2;
  height_scale := heightmap.y_scale * 2 / 10;  //this is just a guesswork, no idea what's the real calculation

  for y := 0 to heightmap.height * 4 - 1 do
      for x := 0 to heightmap.width * 4 - 1 do begin
          vert.x := (-width_half  + x) * scale;
          vert.z := (-height_half + y) * scale;
          vert.u := x / (heightmap.width  * 4);
          vert.v := y / (heightmap.height * 4);
          i := y * heightmap.width * 4 + x;
          vert.y := (255 - height_texture[i]) * height_scale;
          vertex_array[i] := vert;
      end;
end;


procedure SaveMaterialFile(const obj_fname, mtl_name, texture_fname: string);
var
  f: TextFile;
begin
  AssignFile(f, obj_fname + '.mtl');
  Rewrite(f);

  writeln(f, '# RS heightmap');
  writeln(f, 'newmtl ', mtl_name);     //begin new material
  writeln(f, 'map_Kd ', texture_fname);  //texture
  writeln(f, 'Ka 1.000 1.000 1.000');  //ambient color
  writeln(f, 'Kd 1.000 1.000 1.000');  //diffuse color
  writeln(f, 'Ks 1.000 1.000 1.000');  //specular color
  writeln(f, 'Ns 100.0');              //specular weight
  writeln(f, 'illum 2');               //Color on and Ambient on, Highlight on

  CloseFile(f);
end;


procedure TWorld.WriteToObj(const objFname: string);
const
  MAT_NAME = 'default';
var
  f: textfile;
  i: integer;
  v: TVertex3f;
  x, y, stride: integer;
  i2, i3: integer;
begin
  AssignFile(f, objFname);
  Rewrite(f);

  writeln(f, '# RS heightmap');
  writeln(f, 'mtllib ', objFname + '.mtl');

  //vertices
  for i := 0 to vertex_count - 1 do begin
      v := vertex_array[i];
      writeln(f, 'v ', v.x:0:2, ' ', v.y:0:2, ' ', v.z:0:2);
  end;

  //uv-s
  for i := 0 to vertex_count - 1 do begin
      v := vertex_array[i];
      writeln(f, 'vt ', v.u:0:4, ' ', v.v:0:4);
  end;

  //select material
  writeln(f, 'usemtl ' + MAT_NAME);

  //faces
  {
    12 2
    3 34
  }
  stride := heightmap.width * 4;
  for y := 0 to heightmap.height * 4 - 2 do
      for x := 0 to heightmap.width * 4 - 2 do begin
          i := y * stride + x + 1;
          i2 := i + 1;
          i3 := i + stride;
          writeln(f,  Format('f %d/%d %d/%d %d/%d', [i, i, i2, i2, i3, i3]));
          i := i3 + 1;
          writeln(f,  Format('f %d/%d %d/%d %d/%d', [i2, i2, i, i, i3, i3]));
      end;

  CloseFile(f);

  SaveMaterialFile(objFname, MAT_NAME, TEXTURE_FNAME);
end;

procedure TWorld.LoadFromFiles(const hmp, tex, texmap: string);
begin
  LoadHeightmap(hmp);
  LoadTextures(tex, texmap);
end;

procedure TWorld.ExportToObj(const objfname: string);
begin
  GenerateCompositeTexture;
  HeightmapToTexture;
  GenerateVertices;
  WriteToObj(objfname);
end;

procedure TWorld.ExportToRaw(const rawfname: string);
var
  f: file;
  image_size, i: integer;
  y: Byte;
begin
  AssignFile(f, rawfname);
  Rewrite(f,1);
  image_size := heightmap.width * heightmap.height;
  for i := 0 to image_size - 1 do begin
      y := 255 - height_texture[i];  //negative scale - Unity uses it like this, for example
      BlockWrite(f, y, 1);
  end;
  CloseFile(f);
end;

constructor TWorld.Create;
begin
  height_texture := nil;
  vertex_array := nil;
end;

destructor TWorld.Destroy;
begin
  if height_texture <> nil then Freemem(height_texture);
  if vertex_array <> nil then Freemem(vertex_array);
  inherited Destroy;
end;

end.

