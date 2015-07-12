unit terrain_mesh;
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, matrix,
  gl, glext, glu,
  rs_world, vector_util;

type
 TRenderOpts = record
      wireframe: boolean;
      points: boolean;
      vcolors: boolean;
      textures: boolean;
      fg_to_draw: integer;
  end;

 TTerrainBlock = record
     texture_index: integer;
     vertices: array[0..24] of Tvector3_single;         //25*3*4 = 300B
     normals:  array[0..24] of Tvector3_single;         //25*3*4 = 300B
 end; //~600B per block

  { TTerrainMesh }
  TTerrainMesh = class
    private
      terrain: TWorld;
      blocks: array of array of TTerrainBlock;
      block_texcoords: array[0..24] of Tvector2_single;  //static, 25*2*4 = 200B
      block_face_indices: array[0..16*2*3 - 1] of byte;  //static, 96B
      textures_glidx: array of integer;
      function TileToBlock(var tile: TTile; basey, basex: integer): TTerrainBlock;
      procedure TransformTiles;
      procedure InitBlockStaticData;
    public
      destructor Destroy; override;
      procedure Load(const hmp_filename: string);
      procedure InitGL;
      procedure DrawGL(opts: TRenderOpts);
  end;

implementation

{ TileToBlock
  Create single terrain block at given position using RS3D tile.
  Basex/y is the position in block units for given dimension.
  While vertical scaling is stored within file, horizontal seems to be always 0.5.
  The generated x, y coords are flipped for (opengl) rendering.
}
function TTerrainMesh.TileToBlock(var tile: TTile; basey, basex: integer): TTerrainBlock;
const
  h_scale = 0.5;
var
  x, y, idx: integer;
  v_scale: single;
begin
  result.texture_index := tile.texture_index;
  //dim * vertices_per_tile - half_tile_dim * vertices_per_tile
  basey := basey * 4 - terrain.TileHeight * 2;
  basex := basex * 4 - terrain.TileWidth  * 2;
  v_scale := terrain.heightmap.y_scale;
  for y := 0 to 4 do
      for x := 0 to 4 do begin
          idx := y * 5 + x;
            result.vertices[idx].init( //x,y,z, rotated by 180 around z
            (basex + x) * h_scale * -1,
            tile.heights[idx] * v_scale * -1,
            (basey + y) * h_scale
          );
      end;
end;

{ TransformTiles
  Create terrain blocks from RS3D data
}
procedure TTerrainMesh.TransformTiles;

  //todo do proper per-vertex normal:
  //this only calculates face normals and sets them to face's vertices, overwriting the value set
  //from previous face
  procedure FakeNormals(var blk: TTerrainBlock);
    procedure SetTriData(const i0, i1, i2: byte);
    var
      normal: Tvector3_single;
    begin
      normal := GetNormal(blk.vertices[i0], blk.vertices[i1], blk.vertices[i2]);
      blk.normals[i0] := normal;
      blk.normals[i1] := normal;
      blk.normals[i2] := normal;
    end;
  const
    VertexStride = 5;
  var
    x, y, i: integer;
  begin
    for y := 0 to 3 do
        for x := 0 to 3 do begin
            i := y * VertexStride + x;
            SetTriData(i, i+1, i+VertexStride);
            SetTriData(i+1, i+VertexStride+1, i+VertexStride);
        end;
  end;

var
  x, y, tile_idx: integer;
  blk: TTerrainBlock;
  tile: TTile;
begin
  SetLength(blocks, terrain.TileWidth, terrain.TileHeight);
  for y := 0 to terrain.TileHeight - 1 do begin
      for x := 0 to terrain.TileWidth - 1 do begin
          tile_idx := terrain.heightmap.blk[y * terrain.TileWidth + x];
          tile := terrain.heightmap.tiles[tile_idx];

          blk := TileToBlock(tile, y, x);
          FakeNormals(blk);
          blocks[y, x] := blk;
      end;
  end;
end;

{ InitBlockStaticData
  Initializes data shared between tiles: face vertex indices, texture coords
}
procedure TTerrainMesh.InitBlockStaticData;

  procedure SetTriData(const tri_idx: integer; const i0, i1, i2: byte);
  begin
    block_face_indices[tri_idx * 3 + 0] := i0;
    block_face_indices[tri_idx * 3 + 1] := i1;
    block_face_indices[tri_idx * 3 + 2] := i2;
  end;

const
  VertexStride = 5;
var
  x, y, i, tri_idx: integer;
begin
  tri_idx := 0;
  //init face indices
  for y := 0 to 3 do
      for x := 0 to 3 do begin
          i := y * VertexStride + x;
          SetTriData(tri_idx,   i, i+1, i+VertexStride);
          SetTriData(tri_idx+1, i+1, i+VertexStride+1, i+VertexStride);
          tri_idx += 2;
      end;
  //init uv coords
  for y := 0 to 4 do
      for x := 0 to 4 do begin
          block_texcoords[y * 5 + x].init(x/4, 1 - y/4);  //u, v
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
   InitBlockStaticData;
   WriteLn(Format('terrain size: %dx%d, tris: %d',
                           [terrain.TileWidth, terrain.TileHeight,
                           terrain.TileWidth * terrain.TileHeight * 4 * 4 * 2]));
end;

//generate textures. TODO texture atlas?
procedure TTerrainMesh.InitGL;

  procedure GenTexture(tex_idx: integer; use_mip: boolean);
  const
    TexW = 64;
    TexH = 64;
  var
    tex: pbyte;
  begin
    tex := terrain.heightmap.textures[tex_idx];
    //pnm_save('tex'+IntToStr(tex_idx) + '.pnm', tex, TexW, TexH);  //debug
    glGenTextures(1, @textures_glidx[tex_idx]);
    glBindTexture(GL_TEXTURE_2D, textures_glidx[tex_idx]);
    glTexImage2D(GL_TEXTURE_2D, 0, GL_RGB8, TexW, TexH, 0, GL_RGB, GL_UNSIGNED_BYTE, tex);
    if use_mip then begin
        glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR );
        glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR_MIPMAP_LINEAR );
        gluBuild2DMipmaps(GL_TEXTURE_2D, GL_RGB8, TexW, TexH, GL_RGB, GL_UNSIGNED_BYTE, tex);
    end else begin
        glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR );
        glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR );
    end;
    glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
    glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
  end;

var
  i: integer;
begin
  glEnable(GL_TEXTURE_2D);
  SetLength(textures_glidx, terrain.heightmap.texture_count);
  for i := 0 to terrain.heightmap.texture_count - 1 do
      GenTexture(i, true);
end;


//draw vertices from each block
procedure TTerrainMesh.DrawGL(opts: TRenderOpts);
var
  x, y: integer;
  blk: TTerrainBlock;
  last_tex_index: integer;
begin
  if opts.wireframe then
      glPolygonMode(GL_FRONT_AND_BACK, GL_LINE)
  else
      glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);

  glEnable(GL_TEXTURE_2D);
  glEnableClientState(GL_VERTEX_ARRAY);
  glEnableClientState(GL_NORMAL_ARRAY);
  glEnableClientState(GL_TEXTURE_COORD_ARRAY);
  last_tex_index := -1;

  for y := 0 to terrain.TileHeight - 1 do
      for x := 0 to terrain.TileWidth - 1 do begin
          blk := blocks[y, x];

          //repeated texture binding slows this down a lot (68->30fps)
          //todo sort by texture?
          if last_tex_index <> blk.texture_index then begin
              last_tex_index := blk.texture_index;
              glBindTexture(GL_TEXTURE_2D, textures_glidx[last_tex_index]);
          end;

          glVertexPointer(3, GL_FLOAT, sizeof(Tvector3_single), @blk.vertices[0].data[0]);
          glNormalPointer(GL_FLOAT, sizeof(Tvector3_single), @blk.normals[0].data[0]);
          glTexCoordPointer(2, GL_FLOAT, sizeof(Tvector2_single), @block_texcoords[0].data[0]);

          if opts.points then
              glDrawArrays(GL_POINTS, 0, 25)
          else
              glDrawElements(GL_TRIANGLES, 16*2*3, GL_UNSIGNED_BYTE, @block_face_indices);
      end;
end;

end.

