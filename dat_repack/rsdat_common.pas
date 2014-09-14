unit rsdat_common;

{$mode objfpc}{$H+}

interface

type
  (*
  uint32 {4}   - Offset
  uint32 {4}   - Length (entry count * 32)
  uint32 {4}   - Padding (all 255's)  0xFF FF FF FF
  uint16 {2}   - type flag:
                 %10000000 - folder
                 %00000010 - file
                 %10000010 - subfolder
  uint16 {2}   - directory subentries length (entry count * 32)
  char {16}    - Filename (null) (replace "_" with ".")
  *)
  TFileEntry = packed record
      offset: longword;
      length: longword;
      padding: longword;
      type_flag: word;
      sub_entry_size: word;
      filename: array[0..15] of char;
  end;
  PFileEntry = ^TFileEntry;

const
  FEDirectoryFlag = %10000000;

type
  //file or directory node
  PFileNode = ^TFileNode;

  TFileNode = record
      Name: string;
      is_directory: boolean;
      subentries_count: integer;
      offset: longword;
      size: longword;
      entry: TFileEntry;
      Data: pbyte;
      nodes: array of PFileNode;  //children if directory
  end;

  //root
  TSection = record
      Name: string;     //section name
      offset: integer;  //offset in dat file
      size: integer;    //section length in bytes
      Data: pbyte;      //data
      nodes: array of TFileNode;  //all file entries / nodes
      root: TFileNode;  //tree structure of nodes
  end;

function CountSubNodes(node: PFileNode): integer;
function CountSubNodeSizes(node: PFileNode): integer;

//**************************************************************************************************
implementation

function CountSubNodes(node: PFileNode): integer;
var
  i: integer;
begin
  Result := 1;
  if node^.is_directory then
      for i := 0 to Length(node^.nodes) - 1 do
          Result += CountSubNodes(node^.nodes[i]);
end;

function CountSubNodeSizes(node: PFileNode): integer;
var
  i: integer;
begin
  Result := node^.size;
  if node^.is_directory then
      for i := 0 to Length(node^.nodes) - 1 do
          Result += CountSubNodeSizes(node^.nodes[i]);
end;

end.
