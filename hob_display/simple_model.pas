unit simple_model;
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, gl,
  fgl, GenericStructList, hob_parser, hmt_parser;

type
  TVertex = record
    x, y, z: single;
  end;

  TQuad = record
    vertices: array [0..3] of TVertex;
    material_index: integer;
    tex_coords: array [0..3, 0..1] of single;
    colors: array[0..3] of TRGBA;
  end;

  TMaterial = record
      has_texture: boolean;
      bpp: byte;
      gl_tex_id: integer;
      width, height: integer;
      pixels: pbyte;
  end;

  TVertexList = specialize TGenericStructList<TVertex>;
  TPolyList = specialize TGenericStructList<TQuad>;
  TMaterialArray = array of TMaterial;

  { TModel }

  TModel = class
    private
      _vertices: TVertexList;
      _triangles: TPolyList;
      _quads: TPolyList;
      _materials: array of TMaterial;
      _hmt: THmtFile;
      procedure HmtRead(const filename: string);
      procedure HobRead(const filename: string);
      procedure HobReadMesh(const mesh: THobObject);
    public
      destructor Destroy; override;
      procedure Load(const hob_filename, hmt_filename: string);
      function GetTriangles: TPolyList;
      function GetQuads: TPolyList;
      function GetVertices: TVertexList;
      function GetMaterials: TMaterialArray;
      procedure InitGL;
  end;

implementation

{ TModel }

function FixRange(const coord_i16: smallint): single;
begin
  result := 0;
  if coord_i16 <> 0 then
      result := coord_i16 * (1 / 4000);
end;

function FixUvRange(const coord_i16: smallint): single;
begin
  result := 0;
  if coord_i16 <> 0 then
      result := coord_i16 * (1 / 4096);
end;


procedure TModel.HobReadMesh(const mesh: THobObject);
var
  i, k, idx: Integer;
  fg: THobFaceGroup;
  v: TVertex;
  group_vertices: TVertexList;
  quad: TQuad;
begin
  group_vertices := TVertexList.Create;
  for fg in mesh.face_groups do begin
      for i := 0 to fg.vertex_count - 1 do begin
          v.x := FixRange(fg.vertices[i].x);
          v.y := FixRange(fg.vertices[i].y);
          v.z := FixRange(fg.vertices[i].z);
          //flip Y for OpenGL coord system, otherwise the model is upside down.
          //Flip x coord too, otherwise the model looks mirrored
          v.y := -v.y;
          v.x := -v.x;
          _vertices.Add(v);
          group_vertices.Add(v);
      end;

      for i := 0 to fg.face_count - 1 do begin
          for k := 0 to fg.faces[i].ftype - 1 do begin
              idx := fg.faces[i].indices[k];
              quad.vertices[k] := group_vertices[idx];
              quad.colors[k] := fg.faces[i].vertex_colors[k];

              quad.material_index := fg.faces[i].material_index;
              quad.tex_coords[k, 0] := FixUvRange(fg.faces[i].tex_coords[k].u);
              quad.tex_coords[k, 1] := FixUvRange(fg.faces[i].tex_coords[k].v);
          end;
          if fg.faces[i].ftype = 3 then
              _triangles.Add(quad)
          else
              _quads.Add(quad);
      end;
      group_vertices.Clear;
  end;
  group_vertices.Free;
end;


procedure TModel.HobRead(const filename: string);
var
  i: Integer;
  hob: THobFile;
begin
  hob := ParseHobFile(filename);
  for i := 0 to hob.obj_count - 1 do
      HobReadMesh(hob.objects[i]);
  WriteLn('vertices: ', _vertices.Count);
  WriteLn('faces (tri/quad): ', _triangles.Count + _quads.Count, _triangles.Count:6, _quads.Count:6);
end;


procedure TModel.HmtRead(const filename: string);
  procedure SetTexByName (var mat: TMaterial; const name: string);
  var
    i: integer;
    tex: THmtTexture;
  begin
    mat.has_texture := false;
    for i := 0 to _hmt.texture_count - 1 do
        if _hmt.textures[i].name_string = name then begin
            tex := _hmt.textures[i];
            if not (tex.image.type_ in [0,1,3,4]) then
                break;

            mat.bpp := 24;
            if tex.image.type_ = 4 then
                mat.bpp := 8;

            mat.width := tex.width;
            mat.height := tex.height;
            mat.pixels := tex.image.pixels;
            mat.has_texture := true;

            writeln('material texture found: ', name);
            break;
        end;
  end;
var
  i: integer;
begin
  _hmt := ParseHmtFile(filename);
  SetLength(_materials, _hmt.material_count);
  for i := 0 to _hmt.material_count - 1 do
      SetTexByName(_materials[i], _hmt.materials[i].name_string);
end;


destructor TModel.Destroy;
begin
  inherited Destroy;
  _triangles.Free;
end;

procedure TModel.Load(const hob_filename, hmt_filename: string);
begin
  _vertices := TVertexList.Create;
  _triangles := TPolyList.Create;
  _quads := TPolyList.Create;
  WriteLn('Loading mesh file ', hob_filename);
  HobRead(hob_filename);
  WriteLn('Loading material file ', hmt_filename);
  HmtRead(hmt_filename);
end;

function TModel.GetTriangles: TPolyList;
begin
  result := _triangles;
end;

function TModel.GetQuads: TPolyList;
begin
  result := _quads;
end;

function TModel.GetVertices: TVertexList;
begin
  result := _vertices;
end;

function TModel.GetMaterials: TMaterialArray;
begin
  result := _materials;
end;

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

procedure TModel.InitGL;

  procedure GenTexture(var mat: TMaterial);
  begin
    glGenTextures(1, @mat.gl_tex_id);
    glBindTexture(GL_TEXTURE_2D, mat.gl_tex_id);
    glPixelStorei(GL_UNPACK_ALIGNMENT, 1);

    if mat.bpp = 24 then begin
        glTexImage2D(GL_TEXTURE_2D, 0, GL_RGB8, mat.width, mat.height, 0, GL_RGB, GL_UNSIGNED_BYTE, mat.pixels);
        //pnm_save(IntToStr(mat.gl_tex_id)+'.pnm', mat.pixels, mat.width, mat.height);
    end;
    if mat.bpp = 8 then begin
        glTexImage2D(GL_TEXTURE_2D, 0, GL_LUMINANCE, mat.width, mat.height, 0, GL_RED, GL_UNSIGNED_BYTE, mat.pixels);
        //pgm_save(IntToStr(mat.gl_tex_id)+'.pgm', mat.pixels, mat.width, mat.height);
    end;

    glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR );
    glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR );
    //which mode?
   // glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP);
   // glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP);
  end;

var
  i: integer;
begin
  for i := 0 to _hmt.material_count - 1 do begin
      if _materials[i].has_texture then
          GenTexture(_materials[i]);
  end;
end;

end.

