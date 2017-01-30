unit hob_mesh;
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, gl, GLext, math, gvector,
  hob_parser, hmt_parser;

type
  TVertex = record
    x, y, z: single;
  end;

  TTriangle = record
    vertices: array [0..2] of TVertex;
    material_index: integer;
    tex_coords: array [0..2, 0..1] of single;
    colors: array[0..2] of TRGBA;
  end;

  TMaterial = record
      has_texture: boolean;
      bpp: byte;
      gl_tex_id: integer;
      width, height: integer;
      pixels: pbyte;
  end;

  TVertexList = specialize TVector<TVertex>;
  TTriangleList = specialize TVector<TTriangle>;
  TMaterialArray = array of TMaterial;

  TRenderOpts = record
      wireframe: boolean;
      points: boolean;
      vcolors: boolean;
      textures: boolean;
      fg_to_draw: integer;
  end;

  { TModel
    single HOB mesh
  }

  TModel = class
    private
      _vertices: TVertexList;
      _triangles: array of TTriangleList;
      _materials: array of TMaterial;
      _hmt: THmtFile;
      _hmt_loaded: boolean;
      procedure HmtRead(const filename: string);
      procedure HobRead(const filename: string);
      procedure HobReadMesh(const mesh: THobObject);
    public
      destructor Destroy; override;
      procedure Load(const hob_filename, hmt_filename: string);
      procedure InitGL;
      procedure DrawGL(opts: TRenderOpts);
      procedure ExportObj(const obj_name: string);
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


{ rearrange HOB data, triangulate quads
}
procedure TModel.HobReadMesh(const mesh: THobObject);
var
  i: Integer;
  fg: THobFaceGroup;
  v: TVertex;
  group_vertices: TVertexList;
  triangle: TTriangle;
  fg_idx: integer;
  tris: TTriangleList;
  last_idx: integer;

  function InitVertex(face: THobFace; offset: integer): TTriangle;
  var
    i, k: Integer;
  begin
    for i := 0 to 2 do begin
        k := (i + offset) and $3;
        result.vertices[i] := group_vertices[face.indices[k]];
        result.colors[i]   := face.vertex_colors[k];
        result.tex_coords[i, 0] := FixUvRange(face.tex_coords[k].u);
        result.tex_coords[i, 1] := FixUvRange(face.tex_coords[k].v);
    end;
    result.material_index := face.material_index;
  end;

begin
  group_vertices := TVertexList.Create;
  setlength(_triangles, Length(mesh.face_groups));
  fg_idx := 0;
  last_idx:=0;
  for fg in mesh.face_groups do begin
      for i := 0 to fg.vertex_count - 1 do begin
          v.x := FixRange(fg.vertices[i].x);
          v.y := FixRange(fg.vertices[i].y);
          v.z := FixRange(fg.vertices[i].z);


            v.x += fg.transform.x/16;
            v.y += fg.transform.y/16;
            v.z += fg.transform.z/16;


          //flip Y for OpenGL coord system, otherwise the model is upside down.
          //Flip x coord too, otherwise the model looks mirrored
          v.y := -v.y;
          v.x := -v.x;

          _vertices.PushBack(v);
          group_vertices.PushBack(v);
      end;
      tris := TTriangleList.Create;
      for i := 0 to fg.face_count - 1 do begin
          triangle := InitVertex(fg.faces[i], 0);
          tris.PushBack(triangle);
          if fg.faces[i].ftype <> 3 then begin
              triangle := InitVertex(fg.faces[i], 2);
              tris.PushBack(triangle);
          end;
      end;
      _triangles[fg_idx] := tris;
      fg_idx += 1;
      group_vertices.Clear;
      last_idx:=fg.fg_group_id;
  end;
  group_vertices.Free;
end;


procedure TModel.HobRead(const filename: string);
var
  i: Integer;
  hob: THobFile;
begin
  hob := ParseHobFile(filename);
  for i := 0 to 0 do
      HobReadMesh(hob.objects[i]);
  WriteLn('vertices: ', _vertices.Size);
  //WriteLn('faces (triangulated): ', _triangles.Count);
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
//  _triangles.Free;
end;

procedure TModel.Load(const hob_filename, hmt_filename: string);
begin
  _vertices := TVertexList.Create;
  //_triangles := TTriangleList.Create;
  WriteLn('Loading mesh file ', hob_filename);
  HobRead(hob_filename);
  if FileExists(hmt_filename) then begin
      WriteLn('Loading material file ', hmt_filename);
      HmtRead(hmt_filename);
      _hmt_loaded := true;
  end else begin
      _hmt_loaded := false;
  end;
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

    glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
    glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
  end;

var
  i: integer;
begin
  if not _hmt_loaded then
      exit;
  for i := 0 to _hmt.material_count - 1 do begin
      if _materials[i].has_texture then
          GenTexture(_materials[i]);
  end;
end;


procedure TModel.DrawGL(opts: TRenderOpts);
var
  vert: TVertex;
  i, k: integer;

  procedure DrawTri(tri: TTriangle);
  var
    mat: TMaterial;
    k: Integer;
  begin
    if _hmt_loaded then begin
        mat := _materials[tri.material_index];
        if mat.has_texture then begin
            glEnable(GL_TEXTURE_2D);
            glBindTexture(GL_TEXTURE_2D, mat.gl_tex_id);
        end else
            glDisable(GL_TEXTURE_2D);
    end;
    glBegin(GL_TRIANGLES);
    for k := 0 to 2 do begin
        if opts.vcolors then
            glColor4ubv(@tri.colors[k]);
        if opts.textures then
            glTexCoord2fv(@tri.tex_coords[k, 0]);
        glVertex3fv(@tri.vertices[k]);
    end;
    glEnd;
  end;

begin
  if opts.wireframe then
      glPolygonMode(GL_FRONT_AND_BACK, GL_LINE)
  else
      glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);

  glDisable(GL_TEXTURE_2D);
  if opts.points then begin
      glBegin( GL_POINTS );
      glColor3f(0, 1, 0);
      for i := 0 to _vertices.Size - 1 do begin
          vert := _vertices[i];
          glVertex3fv(@vert);
      end;
      glEnd;
  end;

  glColor3f(1, 1, 1);
 for k := 0 to Length(_triangles) - 1 do
//k := min(opts.fg_to_draw, Length(_triangles) - 1);
      for i := 0 to _triangles[k].Size - 1 do
          DrawTri(_triangles[k][i]);
end;


const
  HeaderComment = 'Exported with HOB viewer';
  DefaultMaterial = 'default';


procedure TModel.ExportObj(const obj_name: string);
const
  DesiredUnitSize = 2;
var
  objfile: TextFile;
  vt: TVertex;
  face: TTriangle;

  x, y, z: double;
  u, v: double;

  scaling_factor: double;
  coord_max: double;
  uv_counter: integer;
  last_material_index: integer;

  i,j,k: integer;
  vertex_counter: Integer;

function GetMaxCoord: double;
var
  vt: TVertex;
  i: integer;
begin
  result := 0;
  for i := 0 to _vertices.Size - 1 do begin
      vt := _vertices[i];
      x := abs(vt.x);
      y := abs(vt.y);
      z := abs(vt.z);
      coord_max := Max(z, Max(x, y));
      if coord_max > result then
          result := coord_max;
  end;
end;

begin
  AssignFile(objfile, obj_name);
  Rewrite(objfile);

  writeln(objfile, '# ' + HeaderComment);
  writeln(objfile, 'mtllib ', obj_name + '.mtl');

  //scale pass
  scaling_factor := 1;
  //scaling_factor := DesiredUnitSize / GetMaxCoord;

  //vertex pass
  for k := 0 to Length(_triangles) - 1 do
      for i := 0 to _triangles[k].Size - 1 do begin
          face := _triangles[k][i];
          for vt in face.vertices do begin
              x := (vt.x) * scaling_factor;
              y := (vt.y) * scaling_factor;
              z := (vt.z) * scaling_factor;
              writeln(objfile, 'v ', x:10:6, ' ', y:10:6, ' ', z:10:6);
          end;
      end;

  //uv pass
  for k := 0 to Length(_triangles) - 1 do
      for i := 0 to _triangles[k].Size - 1 do begin
          face := _triangles[k][i];
          for j := 0 to 2 do begin
              u := face.tex_coords[j, 0];
              v := face.tex_coords[j, 1];
              writeln(objfile, 'vt ', u:10:6, ' ', v:10:6);
          end;
      end;

  //face / material pass
  uv_counter := 1;
  vertex_counter := 1;
  last_material_index := -1;

  for k := 0 to Length(_triangles) - 1 do begin
      if _triangles[k].Size = 0 then
          continue;

      writeln(objfile, 'g ', k);

      for i := 0 to _triangles[k].Size - 1 do begin
          face := _triangles[k][i];

          if face.material_index <> last_material_index then begin
              if face.material_index = -1 then
                  writeln(objfile, 'usemtl ' + DefaultMaterial)
              else
                  writeln(objfile, 'usemtl material_id', face.material_index);
              last_material_index := face.material_index;
          end;

          write(objfile, 'f ');
          for vt in face.vertices do begin
              write(objfile, vertex_counter);
              write(objfile, '/', uv_counter);
              write(objfile, ' ');
              vertex_counter += 1;
              uv_counter += 1;
          end;
          writeln(objfile);
      end;
  end;

  CloseFile(objfile);

  //SaveMaterials(pdo, obj_name);
end;

end.

