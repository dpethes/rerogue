unit rsdat_pack;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, rs_dat;

type

  { TRSDatPacker }

  TRSDatPacker = class
  private
      Sections: array of TRsHdrSection;
      Data: TMemoryStream;

      procedure FreeSections;
      procedure ReadSectionFiles(const basepath: string);
      procedure WriteData(path: string);
      procedure WriteHeader(path: string);
      procedure WriteNodeData(node: PRsDatFileNode);
      procedure WriteFileEntries(node: PRsDatFileNode; const base_offset: integer);

  public
      procedure PackDirectory(const path: string);
      constructor Create;
      destructor Destroy; override;
  end;

//**************************************************************************************************
implementation

procedure ReadFileNodes(parent: PRsDatFileNode; path: string);
var
  node: PRsDatFileNode;
  info: TSearchRec;
  n: integer;
  f: file;
  subdir_path: string;
begin
  path := IncludeTrailingPathDelimiter(path);
  n := 0;
  if FindFirst(path + '*', faDirectory, Info) = 0 then begin
      repeat
          if (info.Name <> '.') and (info.name <> '..') then begin
              new(node);
              node^.name := info.Name;
              node^.is_directory := false;
              node^.data := nil;
              node^.size := 0;
              node^.subentries_count := 0;
              node^.offset := 0;

              //traverse subdirectory or load file
              if (info.Attr and faDirectory) > 0 then begin
                  node^.is_directory := true;
                  subdir_path := path + node^.name;
                  Writeln('reading dir ', subdir_path);
                  ReadFileNodes(node, subdir_path);
                  node^.subentries_count := CountSubNodes(node) - 1;
                  Writeln('dir subentries: ', node^.subentries_count);
              end
              else begin
                  Writeln('reading file ', path + node^.name);
                  AssignFile(f, path + node^.name);
                  Reset(f, 1);
                  node^.size := FileSize(f);
                  node^.Data := Getmem(node^.size);
                  BlockRead(f, node^.Data^, node^.size);
                  CloseFile(f);
              end;

              n += 1;
              SetLength(parent^.nodes, n);
              parent^.nodes[n-1] := node;
          end;
      until FindNext(info) <> 0;
  end;
end;

procedure FreeFileNodes(node: PRsDatFileNode; const no_disposing: boolean = false);
var
  i: integer;
begin
  for i := 0 to Length(node^.nodes) - 1 do
      FreeFileNodes(node^.nodes[i]);
  node^.nodes := nil;
  if (not node^.is_directory) and (node^.Data <> nil) then
      freemem(node^.Data);
  if not no_disposing then
      dispose(node);
end;

{ TRSDatPacker }

procedure TRSDatPacker.ReadSectionFiles(const basepath: string);
var
  n: integer;
  info: TSearchRec;
  section: TRsHdrSection;
  node: TRsDatFileNode;
begin
  n := 0;
  if FindFirst(basepath + '*', faDirectory, Info) = 0 then begin
      repeat
          if (info.Name <> '.') and (info.name <> '..') and ((info.Attr and faDirectory) > 0) then begin
              Writeln('reading section: ', info.name);

              ReadFileNodes(@node, basepath + info.name);
              node.name := info.Name;
              node.is_directory := true;
              node.data := nil;
              node.offset := 0;
              node.subentries_count := CountSubNodes(@node) - 1;

              section.name := info.name;
              section.root := node;

              n += 1;
              SetLength(Sections, n);
              Sections[n - 1] := section;
          end;
      until FindNext(info) <> 0;
  end;
  FindClose(Info);
end;

procedure TRSDatPacker.FreeSections;
var
  i: integer;
begin
  for i := 0 to Length(Sections) - 1 do
      FreeFileNodes(@Sections[i].root, true);
  Sections := nil;
end;

procedure TRSDatPacker.WriteNodeData(node: PRsDatFileNode);
var
  i: integer;
begin
  if node^.is_directory then begin
      for i := 0 to Length(node^.nodes) - 1 do begin
          WriteNodeData(node^.nodes[i]);
      end;
  end else begin
      node^.offset := Data.Position;
      Data.WriteBuffer(node^.Data^, node^.size);
      for i := 1 to 4 - (node^.size mod 4) do
          Data.WriteByte(0);
  end;
end;

{
TFileEntry = packed record
    offset: longword;
    length: longword;
    padding: longword;
    type_flag: word;
    sub_entry_size: word;
    filename: array[0..15] of char;
end; }
procedure TRSDatPacker.WriteFileEntries(node: PRsDatFileNode; const base_offset: integer);
var
  entry: TRsDatFileEntry;
  name: string;
  i: integer;
begin
  entry.offset := node^.offset - base_offset;
  entry.length := CountSubNodeSizes(node);
  entry.padding := $ffffffff;
  entry.sub_entry_size := 0;
  if node^.is_directory then
      entry.sub_entry_size := (node^.subentries_count + 1) * 32;

  if node^.is_directory then
      entry.type_flag := RS_DATA_FEDirectoryFlag
  else
      entry.type_flag := RS_DATA_FEFileFlag;

  writeln(stderr, format('name: %s size: %d dir: %s subsize: %d',
                  [node^.Name, entry.length, BoolToStr(node^.is_directory), entry.sub_entry_size]));
  name := node^.Name;
  FillByte(entry.filename, 16, 0);
  for i := 0 to Length(name) - 1 do
      entry.filename[i] := name[i + 1];

  Data.WriteBuffer(entry, 32);

  if node^.is_directory then begin
      for i := 0 to Length(node^.nodes) - 1 do begin
          WriteFileEntries(node^.nodes[i], base_offset);
      end;
  end;
end;

procedure TRSDatPacker.WriteData(path: string);
var
  i, k: integer;
  head: pinteger;
  entries: integer;
  section: TRsHdrSection;
begin
  Data := TMemoryStream.Create;
  Data.Size := 1 shl 20;
  for i := 0 to Length(Sections) - 1 do begin
      section := Sections[i];

      Writeln('writing section: ', section.name);
      section.dat_offset := Data.Position;
      Data.WriteQWord(0);  //offset + size placeholder

      Writeln('writing file data');
      for k := 0 to Length(section.root.nodes) - 1 do
          WriteNodeData(section.root.nodes[k]);

      entries := section.root.subentries_count;
      head := pinteger (pbyte(Data.Memory) + section.dat_offset);
      head^ := Data.Position - section.dat_offset;
      head += 1;
      head^ := entries * 32;
      Writeln('writing file entries: ', entries);
      for k := 0 to Length(section.root.nodes) - 1 do
          WriteFileEntries(section.root.nodes[k], section.dat_offset);

      //align?
      for k := 1 to 4 - (Data.Position mod 4) do
          Data.WriteByte(0);

      Sections[i] := section;
  end;
  Data.SaveToFile(path + 'DATA.DAT');
end;

procedure TRSDatPacker.WriteHeader(path: string);
var
  i, k: integer;
  f: file;
  section_name: string;
  name: array[1..28] of byte;
begin
  AssignFile(f, path + 'DATA.HDR');
  Rewrite(f, 1);
  for i := 0 to Length(Sections) - 1 do begin
      section_name := Sections[i].Name;
      Fillbyte(name, 28, 0);
      for k := 1 to Length(section_name) do
          name[k] := byte( section_name[k] );

      Blockwrite(f, name, 28);
      Blockwrite(f, Sections[i].dat_offset, 4);
  end;
  CloseFile(f);
end;

procedure TRSDatPacker.PackDirectory(const path: string);
var
  basepath: string;
begin
  basepath := IncludeTrailingPathDelimiter(path);
  ReadSectionFiles(basepath);
  WriteData(basepath);
  WriteHeader(basepath);
  FreeSections;
end;

constructor TRSDatPacker.Create;
begin

end;

destructor TRSDatPacker.Destroy;
begin
  inherited Destroy;
end;

end.
