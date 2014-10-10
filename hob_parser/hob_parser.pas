unit hob_parser;
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  THobFace = record
      dw1: integer;
      b1, b2, b3: byte;
      bsize: byte;
      ftype: byte; //3 - tri, 4 - quad
      tex_index: word;
      indices: array[0..3] of word;
  end;

  THobFile = record
      name: array[0..15] of byte;
      face_block_offset: integer;
      face_count: integer;
      faces: array of THobFace;
      vertex_count: integer;
      vertices: array of record
          x, y, z, unknown: smallint; //+-2^15
      end;
  end;

function ParseHobFile(const fname: string): THobFile;

//**************************************************************************************************
implementation

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

procedure ReadFaces(var hob: THobFile; var f: TMemoryStream);
var
  face_count: integer;
  i, k: integer;
  face: THobFace;
  unknown: integer;
  file_pos: integer;
begin
  face_count := hob.face_count;
  SetLength(hob.faces, face_count);
  for i := 0 to face_count - 1 do begin
      file_pos := f.Position;
      face.dw1 := f.ReadDWord;  //?
      face.b1 := f.ReadByte;  //46/49/4B
      face.b2 := f.ReadByte;  //51/71
      face.b3 := f.ReadByte;  //0C
      face.bsize := f.ReadByte * 4;  //block size: A = 40B, 9 = 36
      if face.bsize = 36 then
          face.ftype := 3
      else
          face.ftype := 4;

      write(face.dw1:8, face.b1:3, face.b2:3, face.b3:3, face.bsize:3);

      unknown := f.ReadWord;
      if (unknown <> 0) then
           writeln('unusual file: unknown');

      face.tex_index := f.ReadWord;
      write(' ti: ', face.tex_index);

      //read vertex indices
      write(' coords: ');
      for k := 0 to 3 do begin
          face.indices[k] := f.ReadWord;
          write(face.indices[k]:4);
      end;

      //read rest of the face block
      write(' rest: ');
      for k := f.Position to file_pos + face.bsize - 1 do begin
          write(f.ReadByte: 4);
      end;

      hob.faces[i] := face;
      writeln;
      Flush(stdout);
  end;
end;


function ParseHobFile(const fname: string): THobFile;
var
  f: TMemoryStream;
  hob: THobFile;
  i: integer;
  vertex_count: integer;
  obj_count, face_block_offset: integer;
  meshdef_offset: integer;
  unknown: integer;
begin
  f := TMemoryStream.Create;
  f.LoadFromFile(fname);

  obj_count := f.ReadDWord;
  unknown := f.ReadDWord; //sometimes face block start
  f.ReadBuffer(hob.name, 16);

  writeln(NameToString(hob.name));
  writeln('objects: ', obj_count);

  meshdef_offset := f.ReadDWord;
  f.Seek(meshdef_offset + 16, fsFromBeginning); //16B zero

  meshdef_offset := f.ReadDWord;
  f.Seek(meshdef_offset + 32, fsFromBeginning); //32B zero
  face_block_offset := f.ReadDWord;

  //faces
  f.Seek(face_block_offset + 8, fsFromBeginning);  //faceblock start
  {
    bark_moon: 400
    428  - 1ky.hob
    trooper: 840
    1604 - hvyshut
    prbdroid: 756
    wmvwng: 1648
    xwing: 18304
  }
  hob.face_block_offset := f.ReadDWord; //filepos + 4
  hob.face_count := f.ReadDWord;        //face count
  ReadFaces(hob, f);
  writeln('filepos: ', f.Position);

  //vertices
  vertex_count := (f.Size - f.Position) div 8;
  SetLength(hob.vertices, vertex_count);
  for i := 0 to vertex_count - 1 do begin
      hob.vertices[i].x := f.ReadWord;
      hob.vertices[i].y := f.ReadWord;
      hob.vertices[i].z := f.ReadWord;
      hob.vertices[i].unknown := f.ReadWord;
  end;
  hob.vertex_count := vertex_count;
  writeln('vertex_count: ', vertex_count);

  f.Free;
  result := hob;
end;

end.

