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
  PTerrainBlock = ^TTerrainBlock;

  TRenderBlockBatch = record
      texture_gl_index: integer;
      blocks: integer;
      vertices: PSingle;
      normals: PSingle;
      face_indices: PInteger;
  end;

  { TTerrainMesh }
  TTerrainMesh = class
    const
      FacesPerBlock = 4 * 4 * 2;
    private
      terrain: TWorld;
      blocks: array of array of TTerrainBlock;
      block_texcoords: array of Tvector2_single;  //static, 25*2*4 = 200B
      block_face_indices: array[0..FacesPerBlock*3 - 1] of byte;  //static, 96B
      render_batches: array of TRenderBlockBatch;

      function TileToBlock(var tile: TTile; basey, basex: integer): TTerrainBlock;
      procedure TransformTiles;
      procedure InitBlockStaticData;
    public
      destructor Destroy; override;
      procedure Load(level: TLevelListItem);
      procedure InitGL;
      procedure DrawGL(opts: TRenderOpts);
  end;

implementation

const
  VerticesPerBlock = 5 * 5;

function TerrainBlockOrderByTex(Item1: Pointer; Item2: Pointer): integer;
var
  a, b: PTerrainBlock;
begin
  a := PTerrainBlock(Item1);
  b := PTerrainBlock(Item2);
  result := a^.texture_index - b^.texture_index;
end;

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
  vx, vy, vz: single;
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
          //x,y,z, rotated by 180 around z
          vx := (basex + x) * h_scale * -1;
          vy := tile.heights[idx] * v_scale * -1;
          vz := (basey + y) * h_scale;
          result.vertices[idx].init(vx, vy, vz);
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
  blk: integer;
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
  //init uv coords. block_texcoords is quite pessimistic
  SetLength(block_texcoords, VerticesPerBlock * terrain.TileHeight * terrain.TileWidth);
  for blk := 0 to Length(block_texcoords) div VerticesPerBlock - 1 do
  for y := 0 to 4 do
      for x := 0 to 4 do begin
          block_texcoords[blk*25 + y * 5 + x].init(x/4, 1 - y/4);  //u, v
      end;
end;

destructor TTerrainMesh.Destroy;
begin
  inherited Destroy;
end;

procedure TTerrainMesh.Load(level: TLevelListItem);
begin
  terrain := TWorld.Create;
  terrain.LoadFromNodes(level);
  TransformTiles;
  InitBlockStaticData;
  WriteLn(Format('terrain size: %dx%d, tris: %d',
                           [terrain.TileWidth, terrain.TileHeight,
                           terrain.TileWidth * terrain.TileHeight * 4 * 4 * 2]));
end;

{ InitGL
  Prepare data for rendering: generate textures, sort blocks by texture and merge them to batches.
}
procedure TTerrainMesh.InitGL;

  function GenTexture(tex_idx: integer; use_mip: boolean): integer;
  const
    TexW = 64;
    TexH = 64;
  var
    texture_glidx: integer;
    tex: pbyte;
  begin
    tex := terrain.heightmap.textures[tex_idx];
    //pnm_save('tex'+IntToStr(tex_idx) + '.pnm', tex, TexW, TexH);  //debug
    glGenTextures(1, @texture_glidx);
    glBindTexture(GL_TEXTURE_2D, texture_glidx);
    glTexImage2D(GL_TEXTURE_2D, 0, GL_RGB8, TexW, TexH, 0, GL_RGB, GL_UNSIGNED_BYTE, tex);
    if use_mip then begin
        glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR );
        gluBuild2DMipmaps(GL_TEXTURE_2D, GL_RGB8, TexW, TexH, GL_RGB, GL_UNSIGNED_BYTE, tex);
    end else begin
        glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR );
    end;
    glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR );
    glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
    glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
    result := texture_glidx;
  end;

var
  x, y: integer;
  tex, k, vidx: integer;
  render_blocks: TList;
  start_idx, end_idx, block_count: integer;
  element_array_size: integer;
begin
  glEnable(GL_TEXTURE_2D);

  render_blocks := TList.Create;
  for y := 0 to terrain.TileHeight - 1 do
      for x := 0 to terrain.TileWidth - 1 do
          render_blocks.Add(@blocks[y, x]);
  render_blocks.Sort(@TerrainBlockOrderByTex);

  SetLength(render_batches, terrain.heightmap.texture_count);
  start_idx := 0;
  end_idx := 0;
  for tex := 0 to terrain.heightmap.texture_count - 1 do begin
      render_batches[tex].texture_gl_index := GenTexture(tex, true);

      while PTerrainBlock(render_blocks.Items[end_idx])^.texture_index = tex do begin
          end_idx += 1;
          if end_idx > render_blocks.Count - 1 then
              break;
      end;
      block_count := end_idx - start_idx;
      render_batches[tex].blocks := block_count;
      if block_count = 0 then
          continue;

      element_array_size := block_count * VerticesPerBlock * sizeof(Tvector3_single);
      render_batches[tex].vertices := getmem (element_array_size);
      render_batches[tex].normals  := getmem (element_array_size);
      render_batches[tex].face_indices := getmem (block_count * FacesPerBlock*3 * 4);

      for k := 0 to block_count - 1 do begin
          move(PTerrainBlock(render_blocks.Items[start_idx + k])^.vertices[0],
              (pbyte(render_batches[tex].vertices) + k * VerticesPerBlock * sizeof(Tvector3_single))^,
              VerticesPerBlock * sizeof(Tvector3_single));
          move(PTerrainBlock(render_blocks.Items[start_idx + k])^.normals[0],
              (pbyte(render_batches[tex].normals)  + k * VerticesPerBlock * sizeof(Tvector3_single))^,
              VerticesPerBlock * sizeof(Tvector3_single));
          for vidx := 0 to FacesPerBlock*3 - 1 do
              render_batches[tex].face_indices[k * FacesPerBlock*3 + vidx] :=
                  block_face_indices[vidx] + k * VerticesPerBlock;
      end;

      start_idx := end_idx;
      if end_idx > render_blocks.Count - 1 then
          break;
  end;
end;


{ DrawGL
  Renders terrain block batches.
}
procedure TTerrainMesh.DrawGL(opts: TRenderOpts);
var
  i: integer;
  b: TRenderBlockBatch;
begin
  if opts.wireframe then
      glPolygonMode(GL_FRONT_AND_BACK, GL_LINE)
  else
      glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);

  glEnable(GL_TEXTURE_2D);
  glEnableClientState(GL_VERTEX_ARRAY);
  glEnableClientState(GL_NORMAL_ARRAY);
  glEnableClientState(GL_TEXTURE_COORD_ARRAY);
  glTexCoordPointer(2, GL_FLOAT, sizeof(Tvector2_single), @block_texcoords[0].data[0]);

  for i := 0 to Length(render_batches) - 1 do begin
      b := render_batches[i];
      if b.blocks = 0 then
          continue;

      glBindTexture(GL_TEXTURE_2D, b.texture_gl_index);
      glVertexPointer(3, GL_FLOAT, sizeof(Tvector3_single), b.vertices);
      glNormalPointer(GL_FLOAT, sizeof(Tvector3_single), b.normals);

      if opts.points then
          glDrawArrays(GL_POINTS, 0, b.blocks * VerticesPerBlock)
      else
          glDrawElements(GL_TRIANGLES, b.blocks * FacesPerBlock * 3, GL_UNSIGNED_INT, b.face_indices);
  end;
end;

end.

