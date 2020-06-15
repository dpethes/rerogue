{
Loads DATA.DAT/HDR files to memory and parses the file structure.
RS most likely loads just the file structure and the actual file data gets loaded as needed.
We have plenty of ram nowadays, so read files just once and then load from memory.
}
unit rs_dat;
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, gvector;

const
  RS_DATA_HDR = 'DATA.HDR';
  RS_DATA_DAT = 'DATA.DAT';
  RS_DATA_FEDirectoryFlag = %10000000;
  RS_DATA_FEFileFlag = %00000010;

type
  //data file entry
  TRsDatFileEntry = packed record
      offset: longword;
      length: longword;
      padding: longword;
      type_flag: word;
      sub_entry_size: word;
      filename: array[0..15] of char;
  end;
  PRsDatFileEntry = ^TRsDatFileEntry;

  //file or directory node
  PRsDatFileNode = ^TRsDatFileNode;
  TRsDatFileNode = record
      Name: string;
      is_directory: boolean;
      subentries_count: integer;
      offset: longword;
      size: longword;
      entry: TRsDatFileEntry;
      Data: pbyte;
      nodes: array of PRsDatFileNode;  //children if directory
  end;

  //header file entry
  TRsHdrSection = record
      Name: string;                     //section name
      dat_offset: integer;              //offset in dat file
      Data: pbyte;                      //section's data pointer
      nodes: array of TRsDatFileNode;   //all file entries / nodes
      root: TRsDatFileNode;             //tree structure of nodes
  end;
  TRsDatFileNodeList = specialize TVector<TRsDatFileNode>;

  { TRSDatFile }

  TRSDatFile = class
    public
      constructor Create(const hdr_file, dat_file: string);  //loads header & data files to memory
      destructor Destroy; override;                          //cleanup loaded memory
      procedure Parse();                                     //parse file structure
      function  GetStructure(): TRsDatFileNodeList;          //list of sections, list must be freed by user
      procedure WriteFilesToDirectory(const path: string);
    private
      m_sections: array of TRsHdrSection;
      m_data: pbyte;

      procedure ReadHeaderFile(const fname: string);
      procedure ReadDatFile(const fname: string);
      function ReadEntry(const stream: TMemoryStream; const mem_data: pbyte): TRsDatFileNode;
      procedure ReadSectionEntries(var section: TRsHdrSection; const mem_data: pbyte);
      procedure ParseSectionStructure(var section: TRsHdrSection);
  end;
  
function CountSubNodes(node: PRsDatFileNode): integer;
function CountSubNodeSizes(node: PRsDatFileNode): integer;
 

//**************************************************************************************************
implementation

function CountSubNodes(node: PRsDatFileNode): integer;
var
  i: integer;
begin
  Result := 1;
  if node^.is_directory then
      for i := 0 to Length(node^.nodes) - 1 do
          Result += CountSubNodes(node^.nodes[i]);
end;

function CountSubNodeSizes(node: PRsDatFileNode): integer;
var
  i: integer;
begin
  Result := node^.size;
  if node^.is_directory then
      for i := 0 to Length(node^.nodes) - 1 do
          Result += CountSubNodeSizes(node^.nodes[i]);
end;

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

procedure FreeNode(node: PRsDatFileNode);
var
  i: Integer;
begin
  if node^.nodes <> nil then begin
      for i := 0 to Length(node^.nodes) - 1 do
          FreeNode(node^.nodes[i]);
      node^.nodes := nil;
  end;
end;

{ TRSDatFile }

constructor TRSDatFile.Create(const hdr_file, dat_file: string);
begin
  ReadHeaderFile(hdr_file);
  ReadDatFile(dat_file);
end;

destructor TRSDatFile.Destroy;
var
  section: TRsHdrSection;
  node: TRsDatFileNode;
begin
  inherited Destroy;
  for section in m_sections do
      for node in section.nodes do
          FreeNode(@node);
  m_sections := nil;
  freemem(m_data);
end;

procedure TRSDatFile.Parse;
var
  i: integer;
begin
  for i := 0 to length(m_sections) - 1 do begin
      Writeln('reading section ', m_sections[i].name);
      ReadSectionEntries(m_sections[i], m_data + m_sections[i].dat_offset);
      ParseSectionStructure(m_sections[i]);
  end;
end;

function TRSDatFile.GetStructure: TRsDatFileNodeList;
var
  i: Integer;
begin
  Assert(Length(m_sections) > 0, 'data not parsed');
  result := TRsDatFileNodeList.Create;
  for i := 0 to length(m_sections) - 1 do
      result.PushBack(m_sections[i].root);
end;

procedure TRSDatFile.ReadHeaderFile(const fname: string);
var
  f: file;
  section: TRsHdrSection;
  section_n: integer;
  i: integer;
  buffer: array[0..15] of char;
begin
  assignfile(f, fname);
  reset(f, 1);
  section_n := FileSize(f) div 32;
  SetLength(m_sections, section_n);

  for i := 0 to section_n - 1 do begin
      blockread(f, buffer, 16);             //name
      section.name := trim(buffer);
      blockread(f, buffer, 12);             //empty
      blockread(f, section.dat_offset, 4);  //offset in m_data.DAT
      m_sections[i] := section;
  end;

  closefile(f);
end;

procedure TRSDatFile.ReadDatFile(const fname: string);
var
  f: file;
  fsize: integer;
begin
  AssignFile(f, fname);
  reset(f, 1);
  fsize := FileSize(f);
  m_data := getmem(fsize);
  Blockread(f, m_data^, fsize);
  closefile(f);
end;

function TRSDatFile.ReadEntry(const stream: TMemoryStream; const mem_data: pbyte): TRsDatFileNode;
var
  entry: TRsDatFileEntry;
begin
  stream.ReadBuffer(entry, 32);
  result.name := Trim(entry.filename);
  result.offset := entry.offset;
  result.size := entry.length;
  result.is_directory := (entry.type_flag and RS_DATA_FEDirectoryFlag) <> 0;
  result.subentries_count := entry.sub_entry_size div 32 - 1;
  result.entry := entry;
  result.data := nil;
  if not result.is_directory then
      result.Data := mem_data + entry.offset;

  //if (result.offset mod 32) <> 0 then writeln('unaligned offset');
  writeln(stderr, format('name: %s size: %d dir: %s subsize: %d flags: %s',
                  [result.Name, entry.length, BoolToStr(result.is_directory),
                  entry.sub_entry_size, binStr(entry.type_flag, 16)]));
end;

procedure TRSDatFile.ReadSectionEntries(var section: TRsHdrSection; const mem_data: pbyte);
var
  entries_offset: integer;
  entries_size: integer;
  entry_count: integer;
  stream: TMemoryStream;
  i: integer;
begin
  section.data := mem_data;
  entries_offset := (pinteger(mem_data))^;        //offset relative to section beginning
  entries_size   := (pinteger(mem_data + 4))^;    //length in bytes
  entry_count := entries_size div 32;             //actual count of entries
  writeln('entries: ', entry_count);

  //load entries to stream for easier parsing
  stream := TMemoryStream.Create;
  stream.WriteBuffer((mem_data + entries_offset)^, entries_size);
  stream.Seek(0, soBeginning);

  SetLength(section.nodes, entry_count);
  for i := 0 to entry_count - 1 do begin
      section.nodes[i] := ReadEntry(stream, mem_data);
  end;

  stream.Free;
end;


procedure AddNode(const parent: PRsDatFileNode; var nodes: array of TRsDatFileNode; var node_index: integer);
var
  i: integer;
  node: PRsDatFileNode;
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

procedure TRSDatFile.ParseSectionStructure(var section: TRsHdrSection);
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


procedure WriteDirectory(const node: PRsDatFileNode; const path: string; const data: pbyte);
var
  dir: string;
  subnode: PRsDatFileNode;
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
  section: TRsHdrSection;
begin
  for section in m_sections do begin
      WriteDirectory(@section.root, path, section.data);
  end;
end;



end.

