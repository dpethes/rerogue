unit terrain_mesh;
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  gl,
  rs_world;

type
 TRenderOpts = record
      wireframe: boolean;
      points: boolean;
      vcolors: boolean;
      textures: boolean;
      fg_to_draw: integer;
  end;

 TTerrainBlock = packed record
     texture_index: integer;
     vertices: array[0..25] of TVertex3f;
 end;

  { TTerrainMesh }
  TTerrainMesh = class
    private
      terrain: TWorld;
      blocks: array of array of TTerrainBlock;
      textures_glidx: array of integer;
      procedure TransformTiles;
    public
      destructor Destroy; override;
      procedure Load(const hmp_filename: string);
      procedure InitGL;
      procedure DrawGL(opts: TRenderOpts);
  end;

implementation

{
  While vertical scaling is stored within file, horizontal seems to be always 0.5
  The generated y coords are flipped for (opengl) rendering
}
procedure TTerrainMesh.TransformTiles;

  //basex/y - offset in vertices
  //TODO solve flipped coords
  procedure TileToBlock(var blk: TTerrainBlock; var tile: TTile; basex, basey: integer);
  const
    h_scale = 0.5;
  var
    x, y: integer;
    v: TVertex3f;
    width_half, height_half: integer;  //size in vertices
    v_scale: single;
  begin
    width_half  := terrain.TileWidth  * 2;
    height_half := terrain.TileHeight * 2;
    v_scale := terrain.heightmap.y_scale;
    for y := 0 to 4 do
        for x := 0 to 4 do begin
            v.x := (-width_half  + basex + x) * h_scale;
            v.z := (-height_half + basey + y) * h_scale;
            v.v := (1 - x/4);
            v.u := y/4;
            v.y := shortint(tile.heights[y+x*5]) * v_scale;
            v.y := -v.y;

            blk.vertices[y * 5 + x] := v;
        end;
    blk.texture_index := tile.texture_index;
  end;

var
  x, y, i, tile_idx: integer;
  blk: TTerrainBlock;
  tile: TTile;
begin
  SetLength(blocks, terrain.TileWidth, terrain.TileHeight);
  for y := 0 to terrain.TileHeight - 1 do begin
      for x := 0 to terrain.TileWidth - 1 do begin
          tile_idx := terrain.heightmap.blk[y * terrain.TileWidth + x];
          tile := terrain.heightmap.tiles[tile_idx];

          TileToBlock(blk, tile, y * 4, x * 4);
          blocks[y, x] := blk;
      end;
  end;
end;

destructor TTerrainMesh.Destroy;
begin
  inherited Destroy;
end;

procedure TTerrainMesh.Load(const hmp_filename: string);
begin
   terrain := TWorld.Create;
   terrain.LoadFromFiles('hmp_0', 'lv_0.text', 'lv_0.tex');
   //terrain.LoadFromFiles('hmp_1', 'lv_1.text', 'lv_1.tex');
   TransformTiles;
end;

//generate textures. TODO texture atlas?
procedure TTerrainMesh.InitGL;

  procedure GenTexture(tex_idx: integer);
  const
    TexW = 64;
    TexH = 64;
  var
    tex: pbyte;
    y: Integer;
    x: Integer;
  begin
    glGenTextures(1, @textures_glidx[tex_idx]);
    glBindTexture(GL_TEXTURE_2D, textures_glidx[tex_idx]);

    tex := terrain.heightmap.textures[tex_idx];
    //pnm_save('tex'+IntToStr(tex_idx) + '.pnm', tex, TexW, TexH);  //debug
    glTexImage2D(GL_TEXTURE_2D, 0, GL_RGB8, TexW, TexH, 0, GL_RGB, GL_UNSIGNED_BYTE, tex);

    glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR );
    glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR );
    //glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP);
    //glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP);
  end;

var
  i: integer;
begin
  glEnable(GL_TEXTURE_2D);
  SetLength(textures_glidx, terrain.heightmap.texture_count);
  for i := 0 to terrain.heightmap.texture_count - 1 do
      GenTexture(i);
end;

//draw vertices from each block
procedure TTerrainMesh.DrawGL(opts: TRenderOpts);

  procedure RenderBlock(var blk: TTerrainBlock);
    procedure RenderTri(i0, i1, i2:integer);
    var
      v: TVertex3f;
    begin
      v := blk.vertices[i0];
      glTexCoord2f(v.u, v.v);
      glVertex3fv(@v);
      v := blk.vertices[i1];
      glTexCoord2f(v.u, v.v);
      glVertex3fv(@v);
      v := blk.vertices[i2];
      glTexCoord2f(v.u, v.v);
      glVertex3fv(@v);
    end;
  var
    i, x, y, stride: integer;
  begin
    stride := 5;
    glBindTexture(GL_TEXTURE_2D, textures_glidx[blk.texture_index]);

    glBegin(GL_TRIANGLES);
    //glColor3f(0, 1, 0);
    for y := 0 to 3 do
        for x := 0 to 3 do begin
            //do two triangles
            i := y * stride + x;
            RenderTri(i+1, i, i+stride);
            RenderTri(i+1, i+stride, i+stride+1);
        end;
    glEnd;
  { //only 2 tris per block
    glBegin(GL_TRIANGLES);
      i := y * stride + x;
      RenderTri(0, 4, 24);
      RenderTri(24, 20, 0);
    glEnd;
  }
  end;

var
  i, x, y, stride: integer;
  blk: TTerrainBlock;
  v: TVertex3f;
begin
  if opts.wireframe then
      glPolygonMode(GL_FRONT_AND_BACK, GL_LINE)
  else
      glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);
  glEnable(GL_TEXTURE_2D);
  for y := 0 to terrain.TileHeight - 1 do
      for x := 0 to terrain.TileWidth - 1 do begin
          blk := blocks[y, x];
          RenderBlock(blk);
      end;
end;

end.

