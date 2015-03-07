unit rsdat;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, rsdat_common;

type
  { TRSDatFile }

  TRSDatFile = class
    private
      sections: array of TSection;
      data: pbyte;

      procedure ReadDatFile(const fname: string);
      function ReadEntry(const stream: TMemoryStream): TFileNode;
      procedure ReadSectionEntries(var section: TSection);
      procedure ParseSectionStructure(var section: TSection);

    public
      procedure ReadHeader(const fname: string);
      procedure ReadSections(const fname: string);
      procedure WriteFilesToDirectory(const path: string);
      constructor Create;
      destructor Destroy; override;
  end;

//**************************************************************************************************
implementation

procedure SaveFile(const name: string; const buffer: pbyte; const buf_size: integer);
var
  f: file;
  fname: string;
begin
  fname := name;
  AssignFile(f, fname);
  Rewrite(f, 1);
  BlockWrite(f, buffer^, buf_size);
  CloseFile(f);
end;

{ TRSDatFile }

procedure TRSDatFile.ReadDatFile(const fname: string);
var
  f: file;
  fsize: integer;
begin
  AssignFile(f, fname);
  reset(f, 1);
  fsize := FileSize(f);
  data := getmem(fsize);
  Blockread(f, data^, fsize);
  closefile(f);
end;

function TRSDatFile.ReadEntry(const stream: TMemoryStream): TFileNode;
var
  entry: TFileEntry;
begin
  stream.ReadBuffer(entry, 32);
  result.name := Trim(entry.filename);
  result.offset := entry.offset;
  result.size := entry.length;
  result.is_directory := (entry.type_flag and FEDirectoryFlag) <> 0;
  result.subentries_count := entry.sub_entry_size div 32 - 1;
  result.entry := entry;
  result.data := nil;
  //if (result.offset mod 32) <> 0 then writeln('unaligned offset');
  writeln(stderr, format('name: %s size: %d dir: %s subsize: %d flags: %s',
                  [result.Name, entry.length, BoolToStr(result.is_directory),
                  entry.sub_entry_size, binStr(entry.type_flag, 16)]));
end;

procedure TRSDatFile.ReadSectionEntries(var section: TSection);
var
  entries_offset: integer;
  entries_length: integer;
  entry_count: integer;
  stream: TMemoryStream;
  i: integer;
begin
  entries_offset := (pinteger(section.data))^;      //offset relative to section beginning
  entries_length := (pinteger(section.data + 4))^;  //length in bytes
  section.size := entries_offset + entries_length;
  entry_count := entries_length div 32;  //actual count of entries
  writeln('entries: ', entry_count);

  stream := TMemoryStream.Create;
  stream.WriteBuffer(section.data^, section.size);
  stream.Seek(entries_offset, soBeginning);

  SetLength(section.nodes, entry_count);
  for i := 0 to entry_count - 1 do begin
      section.nodes[i] := ReadEntry(stream);
  end;

  stream.Free;
end;


procedure AddNode(const parent: PFileNode; var nodes: array of TFileNode; var node_index: integer);
var
  i: integer;
  node: PFileNode;
begin
  if node_index > length(nodes) - 1 then
     exit;

  //add current to parent
  node := @nodes[node_index];
  i := length(parent^.nodes);
  Setlength(parent^.nodes, i + 1);
  parent^.nodes[i] := node;
  //writeln('added node: ', node^.name, ' to parent ', parent^.name);

  //add all subentries if any
  if node^.is_directory then begin
      //writeln('  subentries: ', node^.subentries_count);
      while CountSubNodes(node) < node^.subentries_count + 1 do begin
          node_index += 1;
          AddNode(node, nodes, node_index);
      end;
  end;
end;

procedure TRSDatFile.ParseSectionStructure(var section: TSection);
var
  node_idx: integer = 0;
begin
  section.root.name := section.name;
  section.root.is_directory := true;
  section.root.data := nil;
  while node_idx < length(section.nodes) do begin
      AddNode(@section.root, section.nodes, node_idx);
      node_idx += 1;
  end;
end;

procedure TRSDatFile.ReadHeader(const fname: string);
var
  f: file;
  section: TSection;
  section_n: integer;
  i: integer;
  buffer: array[0..15] of char;
begin
  assignfile(f, fname);
  reset(f, 1);
  section_n := FileSize(f) div 32;
  SetLength(sections, section_n);

  for i := 0 to section_n - 1 do begin
      blockread(f, buffer, 16);         //name
      section.name := trim(buffer);
      blockread(f, buffer, 12);         //empty
      blockread(f, section.offset, 4);  //offset in DATA.DAT
      sections[i] := section;
  end;

  closefile(f);
end;

procedure TRSDatFile.ReadSections(const fname: string);
var
  i: integer;
begin
  ReadDatFile(fname);
  for i := 0 to length(sections) - 1 do begin
      Writeln('reading section ', sections[i].name);
      sections[i].data := data + sections[i].offset;
      ReadSectionEntries(sections[i]);
      ParseSectionStructure(sections[i]);
  end;
end;

procedure WriteDirectory(const node: PFileNode; const path: string; const data: pbyte);
var
  dir: string;
  subnode: PFileNode;
  i: Integer;
begin
  dir := path + node^.name;
  if not DirectoryExists(dir) then
      MkDir(dir);
  for i := 0 to length(node^.nodes) - 1 do begin
      subnode := node^.nodes[i];
      if subnode^.is_directory then
          WriteDirectory(subnode, dir + DirectorySeparator, data)
      else
          SaveFile(dir + DirectorySeparator + subnode^.name, data + subnode^.offset, subnode^.size);
  end;
end;

procedure TRSDatFile.WriteFilesToDirectory(const path: string);
var
  section: TSection;
begin
  for section in sections do begin
      WriteDirectory(@section.root, path, section.data);
  end;
end;

constructor TRSDatFile.Create;
begin

end;

destructor TRSDatFile.Destroy;
begin
  inherited Destroy;
  sections := nil;
  freemem(data);
end;

end.

