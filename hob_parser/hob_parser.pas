unit hob_parser;
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  THobFace = record
      flags: integer;
      b1, b2, b3: byte;
      bsize: byte;
      ftype: byte; //3 - tri, 4 - quad
      tex_index: word;
      indices: array[0..3] of word;
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
  end;

  THobFile = record
      obj_count: integer;
      name: array[0..15] of byte;
      face_group_offset: integer;
      face_group_count: integer;
      face_group_count0: integer;

      face_groups: array of THobFaceGroup;
  end;

function ParseHobFile(const fname: string): THobFile;

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
var
  i, k: integer;
  face: THobFace;
  unknown: integer;
  file_pos: integer;
  buf: array[0..255] of byte;
begin
  unknown := f.ReadDWord;
  if (unknown <> 0) then
      writeln('unusual file: zero');
  unknown := f.ReadDWord;
  if (unknown <> 0) then
      writeln('unusual file: zero');
  file_pos := f.ReadDWord;
  if file_pos <> f.Position + 4 then
      writeln('unusual file: face data start position');
  group.face_count := f.ReadDWord;
  writeln('faces: ', group.face_count);

  SetLength(group.faces, group.face_count);
  for i := 0 to group.face_count - 1 do begin
      file_pos := f.Position;
      face.flags := f.ReadDWord;  //?
      face.b1 := f.ReadByte;  //46/49/4B
      face.b2 := f.ReadByte;  //51/71
      face.b3 := f.ReadByte;  //0C
      face.bsize := f.ReadByte * 4;  //block size: A = 40B, 9 = 36

      unknown := f.ReadWord;
      if (unknown <> 0) then
          writeln('unusual file: unknown');

      face.tex_index := f.ReadWord;

      //read vertex indices
      for k := 0 to 3 do
          face.indices[k] := f.ReadWord;

      //read rest of the face block
      unknown := file_pos + face.bsize - f.Position;
      for k := 0 to unknown - 1 do
          buf[k] := f.ReadByte;

      //face type: quad or triangle
      if face.flags and %1000 > 0 then
          face.ftype := 4
      else
          face.ftype := 3;

      group.faces[i] := face;

      if DumpFaces then begin
          if face.ftype = 3 then write('t') else write('q');
          write(face.flags:5, face.b1:3, face.b2:3, face.b3:3, face.bsize:3);
          write(' ti: ', face.tex_index);
          write(' coords: ');
          for k := 0 to 3 do
              write(face.indices[k]:4);
          write(' rest: ');
          for k := 0 to unknown - 1 do
              write(buf[k]:4);
          writeln;
      end;
  end;
end;


procedure ReadVertices(var group: THobFaceGroup; var f: TMemoryStream; const vertex_count: integer);
var
  i: integer;
begin
  SetLength(group.vertices, vertex_count);
  for i := 0 to vertex_count - 1 do begin
      group.vertices[i].x := f.ReadWord;
      group.vertices[i].y := f.ReadWord;
      group.vertices[i].z := f.ReadWord;
      group.vertices[i].unknown := f.ReadWord;
  end;
end;


procedure ReadFaceGroup(var fg: THobFaceGroup; var f: TMemoryStream);
var
  filepos: int64;
begin
  //save file position before seeking to face/vertex data and restore it, to read next group properly
  filepos := f.Position;

  //read group/meshdef0
  f.Seek(16, fsFromCurrent);  //unknown
  fg.meshdef1_offset := f.ReadDWord - 4;
  writeln('fg meshdef offset:', fg.meshdef1_offset);

  //read meshdef1
  f.Seek(fg.meshdef1_offset, fsFromBeginning);
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

  f.Seek(filepos + 132, fsFromBeginning);
end;


function ParseHobFile(const fname: string): THobFile;
var
  f: TMemoryStream;
  hob: THobFile;
  i: integer;
  unknown: integer;
begin
  f := TMemoryStream.Create;
  f.LoadFromFile(fname);

  hob.obj_count := f.ReadDWord;  //object count
  unknown := f.ReadDWord;    //sometimes face block start, but useless in general
  if hob.obj_count = 0 then begin
      result := hob;
      writeln('hob file is empty!');
      exit;
  end;

  f.ReadBuffer(hob.name, 16);
  hob.face_group_offset := f.ReadDWord;

  writeln(NameToString(hob.name));
  writeln('objects: ', hob.obj_count);
  writeln('face group offset: ', hob.face_group_offset);
  if hob.obj_count > 1 then begin
      writeln('reading failed: cannot read multiple objects yet!');
      halt;
  end;

  //get face group count
  f.Seek(124, fsFromBeginning); //16B zero
  hob.face_group_count  := f.ReadWord;  //which?
  hob.face_group_count0 := f.ReadWord;
  if hob.face_group_count <> hob.face_group_count0 then begin
      writeln('reading failed: facegroup counts don''t match!: ', hob.face_group_count, hob.face_group_count0:5);
  end;

  //read face group defs
  SetLength(hob.face_groups, hob.face_group_count);
  f.Seek(hob.face_group_offset, fsFromBeginning);
  for i := 0 to hob.face_group_count - 1 do begin
      ReadFaceGroup(hob.face_groups[i], f);
  end;

  f.Free;
  result := hob;
end;

end.
