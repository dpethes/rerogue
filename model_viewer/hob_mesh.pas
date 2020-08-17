unit hob_mesh;
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, gl, GLext, math, gvector, fpimgui, png_writer,
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
      name: string;
  end;

  TVertexList = specialize TVector<TVertex>;
  TTriangleList = specialize TVector<TTriangle>;
  TMaterialArray = array of TMaterial;

  TRenderOpts = record
      part_to_draw: integer;
      show_all_parts: boolean;
      obj_to_draw: integer;
      wireframe: boolean;
      points: boolean;
      vcolors: boolean;
      textures: boolean;
  end;

  TMeshOpts = record
      export_png_textures: boolean;
  end;

  TObjectPart = record
      vertices: TVertexList;
      triangles: TTriangleList;
  end;
  TObjectPartList = specialize TVector<TObjectPart>;

  TRenderObject = record
      parts: TObjectPartList;
  end;
  TRenderObjectList = specialize TVector<TRenderObject>;

  { TModel
    single HOB mesh
  }

  TModel = class
    private
      _materials: TMaterialArray;
      _objects: TRenderObjectList;
      _current_robject: TRenderObject;
      _model_loaded: boolean;

      procedure DeallocObjects;
      procedure HmtRead(stream: TMemoryStream);
      procedure HobRead(stream: TMemoryStream);
      procedure HobTransform(const hobject: THobObject);
      procedure SaveMaterials(const mtl_name: string; const png_textures: boolean);
    public
      constructor Create;
      destructor Destroy; override;
      procedure Load(hob, hmt: TMemoryStream);
      procedure InitGL(hmt: THmtFile);
      procedure DrawGL(var opts: TRenderOpts);
      procedure ExportObj(const obj_name: string; const png_textures: boolean);
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
procedure TModel.HobTransform(const hobject: THobObject);
var
  i: Integer;
  fg: THobFaceGroup;
  v: TVertex;
  current_block_vertices: TVertexList;
  triangle: TTriangle;
  fg_idx: integer;

  function InitVertex(face: THobFace; offset: integer): TTriangle;
  var
    i, k: Integer;
  begin
    for i := 0 to 2 do begin
        k := (i + offset) and $3;
        result.vertices[i] := current_block_vertices[face.indices[k]];
        result.colors[i]   := face.vertex_colors[k];
        result.tex_coords[i, 0] := FixUvRange(face.tex_coords[k].u);
        result.tex_coords[i, 1] := FixUvRange(face.tex_coords[k].v);
    end;
    result.material_index := face.material_index;
  end;

var
  robject: TRenderObject;
  robjpart: TObjectPart;

begin
  current_block_vertices := TVertexList.Create;  //todo just offset to vertex list
  fg_idx := 0;
  robject.parts := TObjectPartList.Create;

  for fg in hobject.face_groups do begin
      current_block_vertices.Clear;
      robjpart.triangles := TTriangleList.Create;
      robjpart.vertices := TVertexList.Create;

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

          robjpart.vertices.PushBack(v);
          current_block_vertices.PushBack(v);
      end;
      for i := 0 to fg.face_count - 1 do begin
          triangle := InitVertex(fg.faces[i], 0);
          robjpart.triangles.PushBack(triangle);

          if fg.faces[i].ftype <> 3 then begin
              triangle := InitVertex(fg.faces[i], 2);
              robjpart.triangles.PushBack(triangle);
          end;
      end;
      fg_idx += 1;

      robject.parts.PushBack(robjpart);
  end;
  current_block_vertices.Free;

  _objects.PushBack(robject);
end;


procedure TModel.HobRead(stream: TMemoryStream);
var
  i: Integer;
  hob: THobFile;
begin
  hob := ParseHobFile(stream);
  if hob.obj_count = 0 then exit;

  for i := 0 to hob.obj_count-1 do
      HobTransform(hob.objects[i]);

  DeallocHob(hob);

  //WriteLn('vertices: ', _vertices.Size);
  //WriteLn('faces (triangulated): ', _triangles.Count);
end;


procedure TModel.HmtRead(stream: TMemoryStream);
var
  hmt: THmtFile;

  procedure SetTexByName (var mat: TMaterial; const name: string);
  var
    i: integer;
    tex: THmtTexture;
  begin
    mat.has_texture := false;
    for i := 0 to hmt.texture_count - 1 do
        if hmt.textures[i].name_string = name then begin
            tex := hmt.textures[i];

            mat.bpp := 24;
            if tex.image.type_ = 3 then
                mat.bpp := 32;
            if tex.image.type_ in [4,5] then
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
  name: string;
begin
  hmt := ParseHmtFile(stream);
  SetLength(_materials, hmt.material_count);
  for i := 0 to hmt.material_count - 1 do begin
      name := hmt.materials[i].name_string;  //preserve for obj/mtl export
      _materials[i].name := name;
      writeln('material: ', name);
      SetTexByName(_materials[i], name);
  end;
  InitGL(hmt);
  DeallocHmt(hmt);
end;

procedure TModel.DeallocObjects;
var
  i, k: integer;
begin
  if _objects.Size = 0 then
      exit;
  for i := 0 to _objects.Size - 1 do begin
      for k := 0 to _objects[i].parts.Size - 1 do begin
          _objects[i].parts[k].vertices.Free;
          _objects[i].parts[k].triangles.Free;
      end;
      _objects[i].parts.Free;
  end;
end;

constructor TModel.Create;
begin
  _objects := TRenderObjectList.Create;
end;

destructor TModel.Destroy;
begin
  inherited Destroy;
  DeallocObjects;
  _materials := nil;
  _objects.Free;
end;

procedure TModel.Load(hob, hmt: TMemoryStream);
begin
  if _model_loaded then begin
      DeallocObjects;
      _objects.Clear;
  end;

  WriteLn('Loading mesh file');
  HobRead(hob);
  WriteLn('Loading material file');
  HmtRead(hmt);
  _model_loaded := true;
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

procedure TModel.InitGL(hmt: THmtFile);

  procedure GenTexture(var mat: TMaterial);
  begin
    glGenTextures(1, @mat.gl_tex_id);
    glBindTexture(GL_TEXTURE_2D, mat.gl_tex_id);
    glPixelStorei(GL_UNPACK_ALIGNMENT, 1);

    if mat.bpp = 24 then begin
        glTexImage2D(GL_TEXTURE_2D, 0, GL_RGB8, mat.width, mat.height, 0, GL_RGB, GL_UNSIGNED_BYTE, mat.pixels);
        //pnm_save(IntToStr(mat.gl_tex_id)+'.pnm', mat.pixels, mat.width, mat.height);
    end
    else if mat.bpp = 8 then begin
        glTexImage2D(GL_TEXTURE_2D, 0, GL_LUMINANCE, mat.width, mat.height, 0, GL_RED, GL_UNSIGNED_BYTE, mat.pixels);
        //pgm_save(IntToStr(mat.gl_tex_id)+'.pgm', mat.pixels, mat.width, mat.height);
    end
    else if mat.bpp = 32 then begin
        glTexImage2D(GL_TEXTURE_2D, 0, GL_RGB8, mat.width, mat.height, 0, GL_RGBA, GL_UNSIGNED_BYTE, mat.pixels);
    end;

    glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR );
    glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR );

    //this looks to differ between various meshes
    glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
    glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
  end;

var
  i: integer;
begin
  for i := 0 to hmt.material_count - 1 do begin
      if _materials[i].has_texture then
          GenTexture(_materials[i]);
  end;
end;


procedure TModel.DrawGL(var opts: TRenderOpts);

  procedure DrawTri(tri: TTriangle);
  var
    mat: TMaterial;
    k: Integer;
  begin
    if _model_loaded then begin
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

var
  vert: TVertex;
  triangle_count: integer = 0;
  robject: TRenderObject;
  part: TObjectPart;
  part_idx, i: integer;

begin
  if _objects.Size = 0 then begin
      ImGui.Begin_('Mesh');
      ImGui.Text('no objects');
      ImGui.End_;
      exit;
  end;

  //clip selected object/part
  opts.obj_to_draw := min(integer(opts.obj_to_draw), integer(_objects.Size - 1));
  robject := _objects[opts.obj_to_draw];
  _current_robject := robject;
  opts.part_to_draw := min(integer(opts.part_to_draw), integer(robject.parts.Size - 1));

  glDisable(GL_TEXTURE_2D);
  if opts.points then begin
      glBegin( GL_POINTS );
      glColor3f(0, 1, 0);
      for part in robject.parts do begin
          if part.vertices.Size = 0 then
              continue;
          for i := 0 to part.vertices.Size - 1 do begin
              vert := part.vertices[i];
              glVertex3fv(@vert);
          end;
      end;
      glEnd;
  end;

  if opts.wireframe then
      glPolygonMode(GL_FRONT_AND_BACK, GL_LINE);
  glColor3f(1, 1, 1);

  for part_idx := 0 to robject.parts.Size - 1 do begin
      if (not opts.show_all_parts) and (part_idx <> opts.part_to_draw) then
          continue;
      part := robject.parts[part_idx];
      if part.triangles.Size = 0 then
          continue;
      for i := 0 to part.triangles.Size - 1 do begin
          DrawTri(part.triangles[i]);
          triangle_count += 1;
      end;
  end;

  if opts.wireframe then
      glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);

  ImGui.Begin_('Mesh');
  ImGui.Text('triangles: %d', [triangle_count]);
  ImGui.Text('object: %d / %d', [opts.obj_to_draw + 1, _objects.Size]);
  if opts.show_all_parts then
      ImGui.Text('parts: %d', [robject.parts.Size])
  else
      ImGui.Text('part: %d / %d', [opts.part_to_draw + 1, robject.parts.Size]);
  ImGui.End_;
end;


const
  HeaderComment = 'Exported with HOB viewer';
  DefaultMaterial = 'default';

procedure TModel.ExportObj(const obj_name: string; const png_textures: boolean);
const
  DesiredUnitSize = 2;
type
  TOutputPass = (OutPassVertex, OutPassUV, OutPassFaces);
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

  i,j: integer;
  vertex_counter, part_idx: Integer;
  mat: TMaterial;
  mtl_name: string;

  robject: TRenderObject;
  part: TObjectPart;
  outpass: TOutputPass;

begin
  robject := _current_robject;
  if (robject.parts = nil) or (robject.parts.Size = 0) then
      exit;

  AssignFile(objfile, obj_name);
  Rewrite(objfile);

  writeln(objfile, '# ' + HeaderComment);
  mtl_name := obj_name + '.mtl';
  writeln(objfile, 'mtllib ', mtl_name);

  scaling_factor := 1;
  uv_counter := 1;
  vertex_counter := 1;
  last_material_index := -1;
  part_idx := 0;

  for outpass := Low(TOutputPass) to High(TOutputPass) do
      for part in robject.parts do begin
          if part.triangles.Size = 0 then
              continue;

          if outpass = OutPassFaces then begin
              writeln(objfile, 'g ', part_idx);
              part_idx += 1;
          end;

          for i := 0 to part.triangles.Size - 1 do begin
              face := part.triangles[i];

              case outpass of
                  OutPassVertex: begin
                      for vt in face.vertices do begin
                          x := (vt.x) * scaling_factor;
                          y := (vt.y) * scaling_factor;
                          z := (vt.z) * scaling_factor;
                          writeln(objfile, 'v ', x:10:6, ' ', y:10:6, ' ', z:10:6);
                      end;
                  end;
                  OutPassUV: begin
                      for j := 0 to 2 do begin
                          u := face.tex_coords[j, 0];
                          v := face.tex_coords[j, 1];
                          writeln(objfile, 'vt ', u:10:6, ' ', v:10:6);
                      end;
                  end;
                  OutPassFaces: begin
                      if face.material_index <> last_material_index then begin
                          if face.material_index = -1 then
                              writeln(objfile, 'usemtl ' + DefaultMaterial)
                          else begin
                              mat := _materials[face.material_index];
                              writeln(objfile, 'usemtl ' + mat.name);
                          end;
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
          end;
      end;

  CloseFile(objfile);
  SaveMaterials(mtl_name, png_textures);
end;


procedure TModel.SaveMaterials(const mtl_name: string; const png_textures: boolean);
var
  mtl_file:TextFile;
  tex_name: string;
  mat: TMaterial;
  pixbuf: pbyte;

  procedure Flip(const samples: integer);
  var
    y: Integer;
    src, dst: Pbyte;
  begin
    src := mat.pixels + (mat.height - 1) * mat.width * samples;
    dst := pixbuf;
    for y := 0 to mat.height - 1 do begin
        move(src^, dst^, mat.width * samples);
        src -= mat.width * samples;
        dst += mat.width * samples;
    end;
  end;

  procedure WriteMaterial;
  begin
    writeln(mtl_file, 'newmtl ', mat.name);  //begin new material
    if mat.has_texture then begin        //texture
        tex_name := '';
        if mat.bpp = 24 then begin
            tex_name := mat.name+'.pnm';
            Flip(3);
            if not png_textures then begin
                pnm_save(tex_name, pixbuf, mat.width, mat.height)
            end
            else begin
                tex_name := mat.name+'.png';
                png_write(tex_name, pixbuf, mat.width, mat.height, 24);
            end;
        end
        else if mat.bpp = 8 then begin
            tex_name := mat.name+'.pgm';
            Flip(1);
            if not png_textures then begin
                pgm_save(tex_name, pixbuf, mat.width, mat.height)
            end
            else begin
                tex_name := mat.name+'.png';
                png_write(tex_name, pixbuf, mat.width, mat.height, 8);
            end;
        end;
        if tex_name <> '' then
            writeln(mtl_file, 'map_Kd ' + tex_name);
    end;
    writeln(mtl_file, 'Ka 1.000 1.000 1.000');  //ambient color
    writeln(mtl_file, 'Kd 1.000 1.000 1.000');  //diffuse color
    writeln(mtl_file, 'Ks 1.000 1.000 1.000');  //specular color
    writeln(mtl_file, 'Ns 100.0');              //specular weight
    writeln(mtl_file, 'illum 2');               //Color on and Ambient on, Highlight on
    writeln(mtl_file);
  end;

begin
  pixbuf := GetMem(512*512);  //overkill for RS texture sizes

  AssignFile(mtl_file, mtl_name);
  Rewrite(mtl_file);

  writeln(mtl_file, '# ' + HeaderComment);
  mat.name := DefaultMaterial;
  mat.has_texture := false;
  WriteMaterial();
  for mat in _materials do begin
      WriteMaterial();
  end;

  CloseFile(mtl_file);
  Freemem(pixbuf);
end;

end.

