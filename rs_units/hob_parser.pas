unit hob_parser;
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TRGBA = record
      color: longword;
  end;

  TTexCoord = record
      u, v: smallint;
  end;

  THobFace = record
      flags: integer;
      b1, b2, b3: byte;
      bsize: byte;
      ftype: byte; //3 - tri, 4 - quad
      material_index: word;
      indices: array[0..3] of word;
      vertex_colors: array[0..3] of TRGBA;
      tex_coords: array[0..3] of TTexCoord;
  end;

  THobFaceGroup = record
      meshdef1_offset: integer;

      face_block_end_offset,
      face_block_offset,
      vertex_block_offset: integer;

      face_count: integer;
      faces: array of THobFace;

      vertex_count: integer;
      vertices: array of record
          x, y, z, unknown: smallint; //+-2^15
      end;
      transform: record
          x,y,z: single;
      end;
  end;

  THobObjectPart = record
      transform: record
          x,y,z: single;
      end;
      meshdef1_offset: UInt32;
      fg_group_id: UInt32;
  end;

  THobObject = record
      name: array[0..15] of byte;
      face_group_offset: integer;
      objects_part_header_offset: integer;
      face_group_header_offset: integer;

      object_part_count: integer;
      face_group_count: integer;

      object_parts: array of THobObjectPart;
      face_groups: array of THobFaceGroup;
  end;

  THobFile = record
      obj_count: integer;
      objects: array of THobObject;
  end;

function ParseHobFile(f: TMemoryStream): THobFile;
procedure DeallocHob(var h: THobFile);

//**************************************************************************************************
implementation

const
  DumpFaces = false;

function NameToString(name: array of byte): string;
var
  i: Integer;
begin
  result := '';
  for i := 0 to length(name) - 1 do begin
      if name[i] = 0 then break;
      result += char( name[i] );
  end;
end;

procedure ReadFaces(var group: THobFaceGroup; var f: TMemoryStream);
const
  FACE_UV      =     %100;
  FACE_QUAD    =    %1000;
  FACE_VCOLORS =   %10000;
  FACE_COLOR   =  %100000;
  FACE_EXT     = %1000000;
var
  i, k: integer;
  face: THobFace;
  zero: integer;
  file_pos: integer;
  color: longword;
begin
  zero := f.ReadQWord;
  if (zero <> 0) then
      writeln('unusual file: zero');
  file_pos := f.ReadDWord;
  if file_pos <> f.Position + 4 then
      writeln('unusual file: face data start position');
  group.face_count := f.ReadDWord;
  writeln('faces: ', group.face_count);

  SetLength(group.faces, group.face_count);
  for i := 0 to group.face_count - 1 do begin
      file_pos := f.Position;
      face.flags := f.ReadDWord;
      face.b1 := f.ReadByte;  //46/49/4B
      face.b2 := f.ReadByte;  //51/71
      face.b3 := f.ReadByte;  //0C
      face.bsize := f.ReadByte * 4;  //block size
      zero := f.ReadWord;
      if (zero <> 0) then
          writeln('unusual file: face header separator');

      //material index
      face.material_index := f.ReadWord;

      //face type: quad or triangle
      if face.flags and FACE_QUAD > 0 then
          face.ftype := 4
      else
          face.ftype := 3;

      //read vertex indices
      for k := 0 to 3 do
          face.indices[k] := f.ReadWord;

      //ext0
      if face.flags and FACE_EXT > 0 then begin
          f.ReadDWord;
          f.ReadDWord;
      end;

      //vertex colors - either per vertex, or the same for all vertices
      if face.flags and FACE_COLOR > 0 then begin
          if face.flags and FACE_VCOLORS > 0 then begin
              for k := 0 to face.ftype - 1 do
                  face.vertex_colors[k].color := f.ReadDWord;
          end else begin
              color := f.ReadDWord;
              for k := 0 to face.ftype - 1 do
                  face.vertex_colors[k].color := color;
          end;
      end;

      //uv coords
      if face.flags and FACE_UV > 0 then begin
          for k := 0 to face.ftype - 1 do begin
              face.tex_coords[k].u := smallint(f.ReadWord);
              face.tex_coords[k].v := smallint(f.ReadWord);
          end;
      end;

      if DumpFaces then begin
          if (face.flags and FACE_QUAD) = 0 then write('t') else write('q');
          write(face.flags:5, face.b1:3, face.b2:4, face.b3:3, face.bsize:3);
          write(' mt: ', face.material_index);
          write(' verts: ');
          for k := 0 to face.ftype - 1 do
              write(face.indices[k]:4);
          write(' colors: ');
          for k := 0 to face.ftype - 1 do
              write(IntToHex(face.vertex_colors[k].color, 8), ' ');
          if (face.flags and FACE_UV) > 0 then begin
              write(' uvs: ');
              for k := 0 to face.ftype - 1 do
                  write('(', face.tex_coords[k].u, ', ', face.tex_coords[k].v, ') ');
          end;
      end;

      //hack for awing.hob
      if f.Position <> (file_pos + face.bsize) then begin
          write(' rest:');
          for k := f.Position to file_pos + face.bsize - 1 do
              write(IntToHex(f.ReadByte, 2):3);
      end;

      if DumpFaces then
          writeln;

      group.faces[i] := face;
  end;
end;


procedure ReadVertices(var group: THobFaceGroup; var f: TMemoryStream; const vertex_count: integer);
var
  i: integer;
begin
  SetLength(group.vertices, vertex_count);
  for i := 0 to vertex_count - 1 do begin
      group.vertices[i].x := SmallInt(f.ReadWord);
      group.vertices[i].y := SmallInt(f.ReadWord);
      group.vertices[i].z := SmallInt(f.ReadWord);
      group.vertices[i].unknown := SmallInt(f.ReadWord);
  end;
end;

procedure ReadFaceGroup(var fg: THobFaceGroup; var f: TMemoryStream);
begin
    //read meshdef1
    fg.face_block_end_offset := f.ReadDWord;
    f.Seek(20, fsFromCurrent);  //zero
    fg.vertex_count := f.ReadDWord;
    f.Seek(8, fsFromCurrent);  //zero
    fg.face_block_offset := f.ReadDWord;
    fg.vertex_block_offset := f.ReadDWord;

    //faces
    writeln('faces at: ', fg.face_block_offset, hexStr(fg.face_block_offset, 4):6);
    f.Seek(fg.face_block_offset, fsFromBeginning);
    ReadFaces(fg, f);

    //vertices
    writeln('vertices at: ', fg.vertex_block_offset, hexStr(fg.vertex_block_offset, 4):6);
    f.Seek(fg.vertex_block_offset, fsFromBeginning);
    ReadVertices(fg, f, fg.vertex_count);

    //if (scale > 0) then
    //for i := 0 to fg.vertex_count - 1 do begin
    //    //fg.vertices[i].x += trunc(tx * 300);
    //    //fg.vertices[i].y += trunc(ty * 300);
    //    //fg.vertices[i].z += trunc(tz * 300);
    //  fg.vertices[i].x *= scale;
    //  fg.vertices[i].y *= scale;
    //  fg.vertices[i].z *= scale;
    //end;
end;

var partid: integer = 0;

procedure ReadObjectPart(var part: THobObjectPart; var f: TMemoryStream);
var
   fnum: single;
   i: integer;
   zero: int64;
   fg_next, fg_end: integer;
begin
   //read group/meshdef0
   fg_next := f.ReadDWord;
   f.Seek(4*2, fsFromCurrent);  //unknown
   fg_end := f.ReadDWord;
   part.meshdef1_offset := f.ReadDWord;

   writeln();
   writeln('part: ', partid); partid += 1;
   writeln('part next: ', fg_next, ' end: ', fg_end);
   writeln('part meshdef1 facegroups start offset:', part.meshdef1_offset);

   zero := f.ReadQWord;
   if zero <> 0 then
       writeln('unusual file: facegroup 0 zero');

   for i := 1 to (48) div 4 do begin
       f.ReadBuffer(fnum, 4);
       //writeln(fnum);
   end;
   part.fg_group_id := f.ReadDWord;
   for i := 1 to (3+3+4) do begin  //unknown floats
       f.ReadBuffer(fnum, 4);
       //writeln(fnum);
   end;
   f.ReadBuffer(part.transform.x, 4);
   f.ReadBuffer(part.transform.y, 4);
   f.ReadBuffer(part.transform.z, 4);

   writeln(part.fg_group_id);
   writeln(part.transform.x:7:5);
   writeln(part.transform.y:7:5);
   writeln(part.transform.z:7:5);
end;



procedure ReadObject(var mesh: THobObject; var f: TMemoryStream);
var
  i: integer;
  j: integer;
  x: integer;
  filepos: int64;
  part_offsets: array of integer;
  unknown: integer;
begin
  f.ReadBuffer(mesh.name, 16);
  mesh.face_group_offset := f.ReadDWord;
  mesh.objects_part_header_offset := f.ReadDWord;
  mesh.face_group_header_offset := f.ReadDWord;

  //TODO skipped stuff

  writeln('object: ', NameToString(mesh.name));
  writeln('face group offset: ', mesh.face_group_offset);

  //Object parts/Facegroup header

  f.Seek(mesh.objects_part_header_offset, fsFromBeginning); //skipped offsets/0s
  mesh.object_part_count  := f.ReadWord;
  mesh.face_group_count := f.ReadWord;

  SetLength(part_offsets, mesh.object_part_count);
  for i := 0 to mesh.object_part_count - 1 do begin
      unknown := f.ReadDWord;
      part_offsets[i] := f.ReadDWord;
  end;

  filepos := f.Position;
  //read object part defs
  SetLength(mesh.object_parts, mesh.object_part_count);
  for i := 0 to mesh.object_part_count - 1 do begin
      writeln('fg meshdef0 offset: ', part_offsets[i], IntToHex(part_offsets[i], 8):9);
      f.Seek(part_offsets[i], fsFromBeginning);
      ReadObjectPart(mesh.object_parts[i], f);
  end;
  writeln;

  f.Seek(mesh.face_group_header_offset, fsFromBeginning);
  mesh.object_part_count  := f.ReadWord;        // <
  mesh.face_group_count := f.ReadWord;          // < should be same as before

  i := 0; // object part
  j := 0; // face group
  SetLength(mesh.face_groups, mesh.face_group_count);
  while (i < mesh.object_part_count) and (j < mesh.face_group_count) do begin
      x := f.ReadDWord;
      if x = 0 then begin
         i += 1;
         continue;
      end;
      filepos := f.Position;
      f.Seek(x - 4, fsFromBeginning);
      ReadFaceGroup(mesh.face_groups[j], f);
      f.Seek(filepos, fsFromBeginning);
      // "inherit" transform from parent object part
      mesh.face_groups[j].transform.x := mesh.object_parts[i].transform.x;
      mesh.face_groups[j].transform.y := mesh.object_parts[i].transform.y;
      mesh.face_groups[j].transform.z := mesh.object_parts[i].transform.z;
      j += 1;
  end;
end;


function ParseHobFile(f: TMemoryStream): THobFile;
var
  hob: THobFile;
  i: integer;
  filepos: int64;
begin
  hob.obj_count := f.ReadDWord;
  f.ReadDWord;  //sometimes face block start, but useless in general

  writeln('objects: ', hob.obj_count);
  if hob.obj_count = 0 then begin
      writeln('hob file is empty!');
      result := hob;
      exit;
  end;

  SetLength(hob.objects, hob.obj_count);
  for i := 0 to hob.obj_count - 1 do begin
      filepos := f.Position;
      ReadObject(hob.objects[i], f);

      //seek to next object header
      if i + 1 < hob.obj_count then
          f.Seek(filepos + 116, fsFromBeginning);
  end;

  result := hob;
end;

procedure DeallocHob(var h: THobFile);
var
  o: THobObject;
  fg: THobFaceGroup;
  op: THobObjectPart;
  i, k: Integer;
begin
  for k := 0 to Length(h.objects) - 1 do begin
      o := h.objects[k];
      for i := 0 to Length(o.face_groups) - 1 do begin
          fg := o.face_groups[i];
          fg.vertices := nil;
          fg.faces := nil;
      end;
      o.face_groups := nil;
      o.object_parts := nil;
  end;
  h.objects := nil;
end;

end.

