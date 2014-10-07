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
      indices: array[0..3] of word;
  end;

  THobFile = record
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

procedure ReadFaces(var hob: THobFile; var f: TMemoryStream);
var
  face_count: integer;
  i, k: integer;
  face: THobFace;
begin
  face_count := hob.face_count;
  SetLength(hob.faces, face_count);
  for i := 0 to face_count - 1 do begin
      face.dw1 := f.ReadDWord;  //?
      face.b1 := f.ReadByte;  //46/49/4B
      face.b2 := f.ReadByte;  //51/71
      face.b3 := f.ReadByte;  //0C
      face.bsize := f.ReadByte * 4;  //block size: A = 40B, 9 = 36
      f.ReadWord;  //?
      f.ReadWord;  //increasing after 4 faces?
      //12B
      for k := 0 to 3 do
          face.indices[k] := f.ReadWord;
      //20B
      for k := 0 to face.bsize - 20 - 1 do
          f.ReadByte;

      hob.faces[i] := face;
  end;
end;


function ParseHobFile(const fname: string): THobFile;
var
  f: TMemoryStream;
  hob: THobFile;
  i: integer;
  vertex_count: integer;
begin
  f := TMemoryStream.Create;
  f.LoadFromFile(fname);

  //faces
  f.Seek(756, fsFromBeginning);  //faceblock start
  {
    428  - 1ky.hob
    1604 - hvyshut
    prbdroid: 756
  }
  hob.face_block_offset := f.ReadDWord; //filepos + 4
  hob.face_count := f.ReadDWord;        //face count
  ReadFaces(hob, f);

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

  f.Free;
  result := hob;
end;

end.

