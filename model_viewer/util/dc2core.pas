unit dc2core;

interface

uses
  sysutils, math;

type

TBitstreamBufferState = record
    current_bits: longword;
    mask: longword;
end;

{ TBitstreamWriter }

TBitstreamWriter = class
  private
    buffer: plongword;
    cur: plongword;
    mask: longword;
    closed: boolean;

  public
    constructor Create(const memory_buffer: pbyte);
    destructor Destroy; override;

    procedure Close;
    function IsByteAligned: boolean;
    procedure ByteAlign;
    function GetBitSize: longword;
    function GetByteSize: longword;
    function GetUnbufferedByteSize: longword;
    function GetDataStart: pbyte;

    procedure Write(const bit: integer);
    procedure Write(const bits, length: longword);  //write multiple bits, lsb first

    function GetState: TBitstreamBufferState;
    procedure SetState(const state: TBitstreamBufferState);

    procedure ResetBufferPosition;
end;


{ TBitstreamReader }

TBitstreamReader = class
  private
    buffer: plongword;
    cur: plongword;
    used: longword;
  public
    constructor Create(const memory_buffer: pbyte);
    function  GetPosition(): longword;
    function  GetBitCount(): longword;
    function  GetUncachedPosition(): longword;
    function  IsByteAligned(): boolean;
    function  Read(): longword;  {$ifdef bs_inline} inline; {$endif}
    function  Read(count: longword): longword;
    function  Show(const count: longword): longword;
    procedure Skip(const count: longword);
    function  ReadInverse(bit_count: longword): longword;

    function  GetState: TBitstreamBufferState;
    procedure SetState(const state: TBitstreamBufferState);
    procedure ResetBufferPosition;

    function GetInternalState: TBitstreamBufferState;
    procedure SetInternalState(const state: TBitstreamBufferState);
end;

function SwapBits (const bits, bit_count: longword): longword;


const
  MAX_BLOCK_SIZE = 32 * 1024;

type
  TBlockTypeEnum = (BTRaw := 0, BTFixed, BTDynamic, BTError);

  //statistiky pre enkodovanie
  stats_t = record
      offsets_sum, offsets_used,             //sucet/pocet offsetov
      matches_sum, elements_encoded: int64;      //   -||-     zhod
      blocks: array[TBlockTypeEnum] of longword;
  end;


type
  { TSlidingBuffer }
  TSlidingBuffer = object
  private
      _buffer: pbyte;
      _previous_bytes_count: integer;
  public
      constructor Init();
      destructor Done;
      function GetWindow: pbyte; inline;
      procedure InsertData(const data: pbyte; const size: integer);
  end;


const
  MAX_COMPRESSION_LEVEL = 7;
  DICT_SIZE = MAX_BLOCK_SIZE;
  MAX_DEFLATE_MATCH_LENGTH = 258;

type
  TSearchResult = record
    distance,
    length: word;
  end;

  { TMatchSearcher }
  TMatchSearcher = class
  private
      _max_search_depth: integer;  //limit how many positions we want to check
      _max_search_match_length: integer;  //limit how long match needs to be to satisfy search conditions
      _links: pinteger;                   //linked list of hash occurences
      _last_seen_idx: pinteger;           //last known position of a hash in the stream
      _bytes_processed: integer;
      _current_chunk_size: integer;
      _sbuffer: TSlidingBuffer;    //sliding buffer for search window data

      function Search(const window_end_ptr, str: pbyte; const current_idx, max_match_length: integer
        ): TSearchResult;

  public
      constructor Create;
      destructor Destroy; override;
      procedure SetCompressionLevel(const level: integer);

      { New chunk of data that to be processed. }
      procedure NewData (const data: pbyte; const size: integer);

      {
        Find previous occurence of bytes in str.
          str         - searched data pointer
          data_index  - searched data index relative to current chunk
      }
      function FindMatch(const str: pbyte; const data_index: integer): TSearchResult;
  end;


const
  //konstanty pre strom
  END_OF_STREAM = 285;
  TOP_NODE      = END_OF_STREAM * 2 + 1;

type
  //huff tree definitions
  tree_node_t = record
      weight: longword;
      child_0: word;
      child_1: word;
  end;
  tree_node_p = ^tree_node_t;

  vlc_code_t = record
      bits: word;
      code_len: byte;
  end;
  vlc_code_p = ^vlc_code_t;

  huff_tree_t = record
      counts: plongword;
      nodes:  tree_node_p;
      codes:  vlc_code_p;
      root_node: longword;
  end;

  //fixed huffcodes are actually constructed using codes <0..287>, but last 2 are never used
  TDecodeTable = record
      codes_of_legth: array[0..15] of word;     //number of codes for given length
      code_value: array[0..END_OF_STREAM + 2] of word; //map code to literal/length value
  end;
  PDecodeTable = ^TDecodeTable;

procedure huff_FillCanonDecodingTable(var tab: TDecodeTable; const code_lengths: pbyte; const count: integer);
procedure huff_code2canon(const codes: vlc_code_p);

procedure huff_init  (out h: huff_tree_t);
procedure huff_free  (h: huff_tree_t);

procedure huff_raise_count (var h: huff_tree_t; const val: word); inline;
procedure huff_build_tree  (var h: huff_tree_t; max_cnt: word = 255);
procedure huff_build_distance_tree (var h: huff_tree_t; max_cnt: word = 255);


const
  END_OF_BLOCK = 1000;  //must not collide with any valid Deflate values
  END_OF_BLOCK_CODE = 256;

type

  { TVlcWriter }
  TVlcWriter = object
  private
    bs: TBitstreamWriter;
    len_tree, dist_tree: vlc_code_p;
  public
    procedure SetTrees(const bitstream: TBitstreamWriter; const length_tree, distance_tree: vlc_code_p);
    procedure WriteMatch (const len, dist: longword);
    procedure WriteLiteral (const c: byte);
    procedure WriteBlockEnd ();
  end;

  TSymbolBits = record
      symbol: word;
      nbits: byte;
  end;

const
  TAB0_BITS = 9;  //LUT bits, must be less or equal to maximum bit length of huff codes allowed by Deflate

type
  TDecodeLookupTables = record
      codes_t0: array[0..511] of TSymbolBits;  //9 bits = 512 values
      canon_table: TDecodeTable;
  end;

  { TVlcReader }

  TVlcReader = class
  private
    bs: TBitstreamReader;
    literal_dectable, distance_dectable: TDecodeLookupTables;
  public
    procedure SetTables(const bitreader: TBitstreamReader; const literal_table, distance_table: TDecodeLookupTables);
    procedure ReadCodePair (out length, distance: word);
  end;


function Length2code (const len:  longword): longword;
function Distance2code(const dist: longword): longword;

function vlc_ReadCode(const bs: TBitstreamReader; const table: TDecodeTable): integer;
function vlc_ReadCode(const bs: TBitstreamReader; const dectable: TDecodeLookupTables): integer;
function InitDecodeLut(const code_lengths: pbyte; const count: integer): TDecodeLookupTables;


const
  MIN_MATCH_LENGTH = 3;

type
  TLiteralMatch = record
      match_length: word; //match length
      offset: word;       //match offset
      literal: byte;      //byte from input stream
  end;
  PLiteralMatch = ^TLiteralMatch;

  { TBlockWriter }
  TBlockWriter = class
  private
    bitWriter: TBitstreamWriter;      //bitstream writer
    literal_match_stats: pinteger;
    distance_stats: pinteger;
    literal_codes: vlc_code_p;
    distance_codes: vlc_code_p;

    _block_type: TBlockTypeEnum;
    _last: boolean;                   //last block in stream
    bs_cache: TBitstreamBufferState;  //state of the buffer at beginning of the block

    procedure BeginBlock;
    procedure BuildHuffCodes;
    procedure BuildFixedHuffCodes;
    procedure WriteCodingTrees;
    procedure WriteBlockEncoded(const search_results: PLiteralMatch; const size: integer);
    procedure WriteBlockRaw(const rawdata: pbyte; const rawsize: integer);

  public
    constructor Create(const output_buffer: pbyte);
    destructor Destroy; override;

    procedure InitNewBlock(const block_type: TBlockTypeEnum);
    procedure SetLast;
    procedure UpdateStatsMatch(const len, dist: longword); inline;
    procedure UpdateStatsLiteral(const literal: byte); inline;

    procedure WriteBlock(const rawdata: pbyte; const rawsize: integer;
      const search_results: PLiteralMatch; const size: integer; const keep_buffer: boolean = false);
    procedure Done;

    function GetStreamSize: integer;
  end;


  TBlockContext = record
      btype: TBlockTypeEnum;
      size:  integer;
      unfinished: boolean;
      last:  boolean;       //last block flag
  end;

  { TBlockReader }
  TBlockReader = class
  private
    _block_type: TBlockTypeEnum;
    _vlc: TVlcReader;
    procedure ReadHeaderCodes(const bs: TBitstreamReader);
    procedure InitFixedCodes(const bs: TBitstreamReader);
  public
    constructor Create;
    destructor Destroy; override;
    function ReadBlockHeader(const bs: TBitstreamReader): TBlockContext;
    function GetVlcReader: TVlcReader; inline;
  end;












(*******************************************************************************
*******************************************************************************)
implementation



{ SwapBits
  Swap bit ordering in source pattern. Swaps up to 16 bits.
}
function SwapBits (const bits, bit_count: longword): longword;
var
  x: longword;
begin
  x := bits;
  x := ((x and $aaaaaaaa) >> 1) or ((x and $55555555) << 1);
  x := ((x and $cccccccc) >> 2) or ((x and $33333333) << 2);
  x := ((x and $f0f0f0f0) >> 4) or ((x and $0f0f0f0f) << 4);
  x := ((x and $ff00ff00) >> 8) or ((x and $00ff00ff) << 8);
  result := x >> (16 - bit_count);
end;

{ TBitstreamWriter }

constructor TBitstreamWriter.Create(const memory_buffer: pbyte);
begin
  buffer := plongword (memory_buffer);
  cur  := buffer;
  cur^ := 0;
  mask := 0;
end;

destructor TBitstreamWriter.Destroy;
begin
  if not closed then
      Close;

  inherited Destroy;
end;

function TBitstreamWriter.GetBitSize: longword;
begin
  result := 32 * (cur - buffer) + mask;
end;

function TBitstreamWriter.GetByteSize: longword;
begin
  result := (cur - buffer) * 4;
  result += (mask + 7) div 8;  //+ buffer
end;

function TBitstreamWriter.GetUnbufferedByteSize: longword;
begin
  result := (cur - buffer) * 4;
end;

function TBitstreamWriter.GetDataStart: pbyte;
begin
  result := pbyte(buffer);
end;

procedure TBitstreamWriter.Close;
begin
end;

function TBitstreamWriter.IsByteAligned: boolean;
begin
  result := mask mod 8 = 0;
end;

procedure TBitstreamWriter.ByteAlign;
begin
  while not IsByteAligned do
      Write(0);
end;

procedure TBitstreamWriter.Write(const bit: integer);
begin
  cur^ := cur^ or longword((bit and 1) shl mask);
  mask += 1;

  if mask = 32 then begin
      cur += 1;
      cur^ := 0;
      mask := 0;
  end;
end;

procedure TBitstreamWriter.Write(const bits, length: longword);
var
  bits_: longword;
begin
  Assert(length <= 32, 'bit_count over 32');

  //clear unused bits
  bits_ := bits and ($ffffffff shr (32 - length));

  cur^ := cur^ or (bits_ shl mask);
  mask += length;
  if mask >= 32 then begin
      mask -= 32;  //number of bits that didn't fit into buffer
      cur += 1;
      cur^ := 0;

      if mask > 0 then
          cur^ := bits_ shr (length - mask);
  end;
end;

function TBitstreamWriter.GetState: TBitstreamBufferState;
begin
  Result.mask := mask;
  Result.current_bits := cur^;
end;

procedure TBitstreamWriter.SetState(const state: TBitstreamBufferState);
begin
  mask := state.mask;
  cur^ := state.current_bits;
end;

procedure TBitstreamWriter.ResetBufferPosition;
var
  cache: TBitstreamBufferState;
begin
  cache := GetState;
  cur := buffer;
  SetState(cache);
end;


{ TBitstreamReader }

constructor TBitstreamReader.Create(const memory_buffer: pbyte);
begin
  buffer := plongword (memory_buffer);
  cur  := buffer;
  used := 0;
end;

function TBitstreamReader.GetPosition: longword;
begin
  result := (cur - buffer) << 2;  //used dword count
  result += (used + 7) shr 3;  //+ buffer
end;

function TBitstreamReader.GetBitCount: longword;
begin
  result := 32 * longword(cur - buffer) + used;
end;

function TBitstreamReader.GetUncachedPosition: longword;
begin
  result := (cur - buffer) * 4;  //used dword count
end;

function TBitstreamReader.IsByteAligned: boolean;
begin
  result := true;
  if used mod 8 > 0 then result := false;
end;

function TBitstreamReader.Read: longword;
begin
  result := (cur^ shr used) and 1;
  used += 1;
  if used = 32 then begin
      cur += 1;
      used := 0;
  end;
end;

function TBitstreamReader.Read(count: longword): longword;
var
  bits_left: integer;
begin
  result := cur^ shr used;
  if count < (32 - used) then begin
      result := result and ($ffffffff shr (32 - count));
      used += count;
  end else begin
      bits_left := count - (32 - used);
      cur += 1;
      if bits_left > 0 then
          result := result or (cur^ and ($ffffffff shr (32 - bits_left))) shl (32 - used);
      used := bits_left;
  end;
end;

function TBitstreamReader.Show(const count: longword): longword;
var
  bits_left: integer;
begin
  result := cur^ shr used;
  if count < (32 - used) then begin
      result := result and ($ffffffff shr (32 - count));
  end else begin
      bits_left := count - (32 - used);
      if bits_left > 0 then
          result := result or ((cur + 1)^ and ($ffffffff shr (32 - bits_left))) shl (32 - used);
  end;
end;

procedure TBitstreamReader.Skip(const count: longword);
begin
  if count < (32 - used) then begin
      used += count;
  end else begin
      cur += 1;
      used := count - (32 - used);
  end;
end;

function TBitstreamReader.ReadInverse(bit_count: longword): longword;
var
  i: integer;
begin
  result := 0;
  for i := bit_count - 1 downto 0 do
      result := result or Read() shl i;
end;

function TBitstreamReader.GetState: TBitstreamBufferState;
begin
  result.current_bits := cur^;
end;

procedure TBitstreamReader.SetState(const state: TBitstreamBufferState);
begin
  cur^ := state.current_bits;
end;

procedure TBitstreamReader.ResetBufferPosition;
begin
  cur := buffer;
end;

function TBitstreamReader.GetInternalState: TBitstreamBufferState;
begin
  result.current_bits := cur - buffer;
  result.mask := used;
end;

procedure TBitstreamReader.SetInternalState(const state: TBitstreamBufferState);
begin
  cur := buffer + state.current_bits;
  used := state.mask;
end;


{ TSlidingBuffer }

constructor TSlidingBuffer.Init();
begin
  _buffer := getmem(2 * MAX_BLOCK_SIZE);
  _buffer += MAX_BLOCK_SIZE;
  _previous_bytes_count := 0;
end;

destructor TSlidingBuffer.Done;
begin
  freemem(_buffer - MAX_BLOCK_SIZE);
end;

function TSlidingBuffer.GetWindow: pbyte;
begin
  result := _buffer;
end;

procedure TSlidingBuffer.InsertData(const data: pbyte; const size: integer);
begin
  Assert(size <= MAX_BLOCK_SIZE, 'cannot insert more data than allocated range');

  if _previous_bytes_count > 0 then
      move((_buffer + _previous_bytes_count - MAX_BLOCK_SIZE)^,
           (_buffer - MAX_BLOCK_SIZE)^,
           MAX_BLOCK_SIZE);

  move(data^, _buffer^, size);
  _previous_bytes_count := size;
end;

const
  SEARCH_DEPTH: array[0..MAX_COMPRESSION_LEVEL] of Integer = (0, 1, 8, 16, 32, 48, 64, 32*1024);
  SEARCH_MATCH_DIVIDER: array[0..MAX_COMPRESSION_LEVEL] of Integer = (1, 4, 4, 4, 4, 4, 2, 1);
  HASH_BITS = 18;

{
  Generate 16bit hash from first 3 bytes of a given pointer
}
function hash3(const x: pbyte): integer; inline;
begin
  result := ((x+2)^ shl 10) xor ((x+1)^ shl 5) xor x^;
end;


{ TMatchSearcher }

constructor TMatchSearcher.Create;
begin
  _sbuffer.Init();
  _max_search_depth := SEARCH_DEPTH[0];
  _max_search_match_length := MAX_DEFLATE_MATCH_LENGTH div SEARCH_MATCH_DIVIDER[0];

  _links := getmem(2 * DICT_SIZE * sizeof(integer));
  _last_seen_idx := getmem(1 shl HASH_BITS * sizeof(integer));  //must be equal to hash bits
  Filldword(_last_seen_idx^, 1 shl HASH_BITS, $ffffffff );  //negative indices don't get searched, so use -1
  _current_chunk_size := 0;
  _bytes_processed := 0;
end;

destructor TMatchSearcher.Destroy;
begin
  freemem(_links);
  freemem(_last_seen_idx);
  _sbuffer.Done;
  inherited;
end;

procedure TMatchSearcher.SetCompressionLevel(const level: integer);
begin
  Assert(level <= MAX_COMPRESSION_LEVEL, 'invalid compression level');
  _max_search_depth := SEARCH_DEPTH[level];
  _max_search_match_length := MAX_DEFLATE_MATCH_LENGTH div SEARCH_MATCH_DIVIDER[level];
end;

{
  Take next data chunk and create links between the occurences of the same hash
}
procedure TMatchSearcher.NewData(const data: pbyte; const size: integer);
var
  i, key, last_seen: integer;
begin
  _sbuffer.InsertData(data, size);
  _bytes_processed += _current_chunk_size;
  _current_chunk_size := size;

  move((_links + DICT_SIZE)^, _links^, DICT_SIZE * sizeof(integer));
  for i := 0 to size - 1 do begin
      key := hash3(data + i);
      last_seen := _last_seen_idx[key];
      _links[DICT_SIZE + i] := last_seen;
      _last_seen_idx[key] := i + _bytes_processed;
  end;
end;

{
  Compare strings, return length of the match. Loop at the last byte of the window.
}
function compare_strings_loop(const window, string_data: pbyte;
   const max_match_length, window_size: integer): integer;
var
  i, k: integer;
begin
  result := 0;
  i := 0;
  for k := 0 to max_match_length - 1 do begin
      if window[i] = string_data[k] then
          result += 1
      else
          exit;
      i += 1;
      if i = window_size then
          i := 0;
  end;
end;

{
  Compare strings, return length of the match.
  There must be at least max_match_length valid bytes in window.
}
function compare_strings(const window, string_data: pbyte;
   const max_match_length: integer): integer;
var
  i: integer;
begin
  result := 0;
  for i := 0 to max_match_length - 1 do
      if window[i] = string_data[i] then
          result += 1
      else
          exit;
end;

{
  Compare last byte of the window against current string.
}
function compare_strings_rle(const string_data: pbyte; const byte_value, max_match_length: integer): integer;
var
  i: integer;
begin
  result := 0;
  for i := 0 to max_match_length - 1 do
      if byte_value = string_data[i] then
          result += 1
      else
          exit;
end;


function InitSearchResult(const distance, best_match: longword): TSearchResult; inline;
begin
  longword(result) := longword( best_match << 16 or distance );
end;

function TMatchSearcher.Search(const window_end_ptr, str: pbyte;
  const current_idx, max_match_length: integer): TSearchResult;
var
  i: integer;
  links: pinteger;
  best_match_distance: integer;
  best_match_length: integer;
  last_seen_idx: integer;
  min_allowed_idx: integer;
  previous_idx: integer;
  length: integer;
  distance: integer;
begin
  Assert(max_match_length >= 3);

  //test if searched string is a repetition of the last byte before full search
  best_match_length := compare_strings_rle(str, window_end_ptr[-1], max_match_length);
  result := InitSearchResult(1, best_match_length);
  if best_match_length >= _max_search_match_length then
      exit;

  last_seen_idx := current_idx - _bytes_processed;
  links := _links + DICT_SIZE;
  best_match_distance := 1;
  min_allowed_idx := max(0, current_idx - DICT_SIZE);

  //early termination if links of the next searched position are much closer than current ones
  if links[last_seen_idx] < links[last_seen_idx + 1] - (DICT_SIZE shr 1) then
      exit;

  for i := _max_search_depth - 1 downto 0 do begin
      //if the position falls out of the sliding window_end_ptr range, it's too old and cannot be searched
      previous_idx := links[last_seen_idx];
      if previous_idx < min_allowed_idx then begin
          break;
      end;
      last_seen_idx := previous_idx - _bytes_processed;

      //compare data at given positions
      distance := current_idx - previous_idx;
      if previous_idx + max_match_length < current_idx then
          length := compare_strings(window_end_ptr - distance, str, max_match_length)
      else
          length := compare_strings_loop(window_end_ptr - distance, str, max_match_length, distance);

      if length > best_match_length then begin
          best_match_length := length;
          best_match_distance := distance;
          if length >= _max_search_match_length then
              break;
      end;
  end;

  Assert(best_match_distance >= 0);
  result := InitSearchResult(best_match_distance, best_match_length);
end;

{
  Find best match between current bytes and bytes already seen.
  If distance = 0 & length = 0 - no occurences were found
}
function TMatchSearcher.FindMatch(const str: pbyte; const data_index: integer): TSearchResult;
var
  max_match_length: integer;
  current_idx: integer;
  window_end_ptr: pbyte;
begin
  result := InitSearchResult(0, 0);

  //reduce maximum possible match length at the end of the stream
  //we need at least 3 bytes to be able to run search (hash function takes 3 bytes as input)
  max_match_length := min(MAX_DEFLATE_MATCH_LENGTH, _current_chunk_size - data_index);
  if max_match_length <= 2 then
      exit;

  //beginning of a stream, nothing to search
  if _bytes_processed + data_index = 0 then
      exit;

  //get proper search window and currently searched string's file index
  window_end_ptr := _sbuffer.GetWindow + data_index;
  current_idx := _bytes_processed + data_index;

  result := Search(window_end_ptr, str, current_idx, max_match_length);
end;


{ huff_FillCanonDecodingTable

  code_lengths - array of lengths, indexed by code
  count - number of codes to fill
}
procedure huff_FillCanonDecodingTable(var tab: TDecodeTable; const code_lengths: pbyte; const count: integer);
var
  len: integer; //current length; all deflate code lengths are between 1 and 15
  same_length_count: integer;
  i, j: integer;
begin
  j := 0;
  tab.codes_of_legth[0] := 0;
  for len := 1 to 15 do begin
      same_length_count := 0;

      for i := 0 to count - 1 do begin
          if code_lengths[i] = len then begin
              tab.code_value[j] := i;
              j += 1;
              same_length_count += 1;
          end;
      end;

      tab.codes_of_legth[len] := same_length_count;
  end;
end;



(*
huff_code2canon
zmen huffkody na kanonicke huffkody
*)
procedure huff_code2canon(const codes: vlc_code_p);
var
  len: integer;
  b, i: integer;
begin
  b := 0;
  for len := 1 to 15 do begin
      for i := 0 to END_OF_STREAM do begin
          if codes[i].code_len = len then begin
              codes[i].bits := b;
              b += 1;
          end;
      end;
      b := b shl 1;
  end;
end;



(*******************************************************************************
generovanie stromu
*)
//huff_init
procedure huff_init (out h: huff_tree_t);
var
  i: integer;
begin
  i := sizeof(longword) * (END_OF_STREAM + 1);
  h.counts := getmem(i);
  FillByte(h.counts^, i, 0);

  i := sizeof(tree_node_t) * (END_OF_STREAM + 1) * 2;
  h.nodes  := getmem(i);
  FillByte(h.nodes^, i, 0);

  i := sizeof(vlc_code_t) * (END_OF_STREAM + 1);
  h.codes  := getmem(i);
  FillByte(h.codes^, i, 0);

  h.root_node := 0;
end;



//huff_free
procedure huff_free (h: huff_tree_t);
begin
  freemem( h.counts );
  freemem( h.nodes  );
  freemem( h.codes  );
end;



procedure huff_raise_count(var h: huff_tree_t; const val: word);
begin
  h.counts[val] += 1;
end;


(*******************************************************************************
scale_counts
*)
procedure scale_counts(var params: huff_tree_t; const max_cnt: word);
var
  counts: plongword;
  b: integer;
  max: longword;
  new: longword;
  ratio: single;
begin
  counts := params.counts;
  max := 0;
  for b := 0 to END_OF_STREAM do
      if counts[b] > max then max := counts[b];

  if max <= max_cnt then exit;

  ratio := single( max ) / max_cnt;
  for b := 0 to END_OF_STREAM do
      if counts[b] > 0 then begin
          new := round( counts[b] / ratio );
          if new = 0 then
              counts[b] := 1
          else
              counts[b] := new;
      end;
end;



(*******************************************************************************
build_tree
zostavenie huffmanovho stromu, nastavenie indexu korenoveho uzla stromu
*)
procedure build_tree (var params: huff_tree_t);
var
  nodes:  tree_node_p;
  counts: plongword;
  next_free: integer;
  i: integer;
  min_1,
  min_2: integer;
begin
  nodes  := params.nodes;
  counts := params.counts;
  for i := 0 to END_OF_STREAM do nodes[i].weight := counts[i];
  nodes[TOP_NODE].weight := High(word);
  next_free := END_OF_STREAM;

  while true do begin
      next_free := next_free + 1;
      min_1 := TOP_NODE;
      min_2 := TOP_NODE;

      for i := 0 to next_free - 1 do
          if nodes[i].weight > 0 then begin
              if nodes[i].weight < nodes[min_1].weight then begin
                  min_2 := min_1 ;
                  min_1 := i ;
              end else
                  if nodes[i].weight < nodes[min_2].weight then min_2 := i;
          end;

      if min_2 = TOP_NODE then break;
      nodes[next_free].weight := nodes[min_1].weight + nodes[min_2].weight;
      nodes[next_free].child_0 := min_1;
      nodes[next_free].child_1 := min_2;
      nodes[min_1].weight := 0;
      nodes[min_2].weight := 0;
  end;

  params.root_node := next_free - 1;
end;



(*******************************************************************************
tree_to_code
z korenoveho uzlu rekurzivne postupuj cez jednotlive listy
a zapis cestu ako VLC kod + pocet jeho bitov do tabulky
*)
procedure tree_to_code (var p: huff_tree_t; code_len_current, node: integer);
begin
  if node <= END_OF_STREAM then
      p.codes[node].code_len := code_len_current
  else begin
      code_len_current := code_len_current + 1;
      tree_to_code (p, code_len_current, p.nodes[node].child_0 );
      tree_to_code (p, code_len_current, p.nodes[node].child_1 );
  end;
end;



(*******************************************************************************
huff_build_tree
zostav huffmanov strom, vrat index korenoveho uzla stromu
*)
procedure huff_build_tree (var h: huff_tree_t; max_cnt: word = 255);
begin
  scale_counts (h, max_cnt);
  build_tree (h);
  tree_to_code (h, 0, h.root_node);
  huff_code2canon (h.codes);
end;

//special case, when there is only one symbol in alphabet - can happen with distance trees
procedure huff_build_distance_tree (var h: huff_tree_t; max_cnt: word = 255);
const
  MAX_DIST_CODES = 32;
var
  i: integer;
  last_nonzero_idx: integer;
  used_symbols_count: integer;
begin
  used_symbols_count := 0;
  last_nonzero_idx := 0;
  for i := 0 to MAX_DIST_CODES - 1 do
      if h.counts[i] > 0 then begin
          used_symbols_count += 1;
          if used_symbols_count > 1 then
              break;
          last_nonzero_idx := i;
      end;

  if used_symbols_count > 1 then begin
      huff_build_tree(h, max_cnt);
  end else begin
      h.codes[last_nonzero_idx].code_len := 1;
      h.codes[last_nonzero_idx].bits := 0;
      h.root_node := last_nonzero_idx;
  end
end;


{ Length2code
  Map match length value to length code for huff encoding.
}
function Length2code (const len: longword): longword;
const
  table: array[byte] of byte = (
  1, 2, 3, 4, 5, 6, 7, 8, 9, 9, 10, 10, 11, 11, 12, 12, 13, 13, 13, 13, 14, 14,
  14, 14, 15, 15, 15, 15, 16, 16, 16, 16, 17, 17, 17, 17, 17, 17, 17, 17, 18, 18,
  18, 18, 18, 18, 18, 18, 19, 19, 19, 19, 19, 19, 19, 19, 20, 20, 20, 20, 20, 20,
  20, 20, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 22, 22,
  22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 23, 23, 23, 23, 23, 23,
  23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24,
  24, 24, 24, 24, 24, 24, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25,
  25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 26, 26,
  26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26,
  26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27,
  27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27,
  27, 27, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28,
  28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 29
  );
begin
  Assert(len >= 3);
  result := 256 + table[len-3];  //0..255 = literals, 256 = block end
end;


{ Code2length
  Map decoded length code to length value.
}
function Code2length(const code: longword): longword; inline;
const
  table: array[0..28] of byte = (
  3, 4, 5, 6, 7, 8, 9, 10, 11, 13, 15, 17, 19, 23, 27, 31, 35, 43, 51, 59, 67,
  83, 99, 115, 131, 163, 195, 227, 0
  );
begin
  result := table[ code - 257 ];
  if result = 0 then result := 258;
end;


{ Distance2code
  Map distance value to distance code for huff encoding.
}
function Distance2code(const dist: longword): longword;
const
  table_512: array [0..511] of byte = (
  0, 1, 2, 3, 4, 4, 5, 5, 6, 6, 6, 6, 7, 7, 7, 7, 8, 8, 8, 8, 8, 8, 8, 8, 9, 9, 9,
  9, 9, 9, 9, 9, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10,
  11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 12, 12, 12, 12,
  12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12,
  12, 12, 12, 12, 12, 12, 12, 12, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13,
  13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13,
  14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14,
  14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14,
  14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14,
  14, 14, 14, 14, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15,
  15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15,
  15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15,
  15, 15, 15, 15, 15, 15, 15, 15, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16,
  16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16,
  16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16,
  16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16,
  16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16,
  16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16,
  16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 17, 17, 17, 17,
  17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17,
  17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17,
  17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17,
  17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17,
  17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17,
  17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17,
  17, 17, 17, 17
  );
  table_128: array[2..127] of byte = (
  18, 19, 20, 20, 21, 21, 22, 22, 22, 22, 23, 23, 23, 23, 24, 24, 24, 24, 24, 24,
  24, 24, 25, 25, 25, 25, 25, 25, 25, 25, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26,
  26, 26, 26, 26, 26, 26, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27,
  27, 27, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28,
  28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 29, 29, 29, 29, 29, 29,
  29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29,
  29, 29, 29, 29, 29, 29
  );
begin
  if dist <= 512 then
      result := table_512[dist - 1]
  else begin
      result := table_128[(dist - 1) shr 8];
  end;
end;


{ Code2distance
  Map decoded distance code to distance value.
}
function Code2distance(const code: longword): longword; inline;
const
  table: array[0..29] of word = (
  1, 2, 3, 4, 5, 7, 9, 13, 17, 25, 33, 49, 65, 97, 129, 193, 257, 385, 513, 769,
  1025, 1537, 2049, 3073, 4097, 6145, 8193, 12289, 16385, 24577
  );
begin
  result := table[ code ];
end;

{ TVlcWriter }

procedure TVlcWriter.SetTrees(const bitstream: TBitstreamWriter;
  const length_tree, distance_tree: vlc_code_p);
begin
  bs := bitstream;
  len_tree := length_tree;
  dist_tree := distance_tree;
end;

procedure TVlcWriter.WriteMatch(const len, dist: longword);
var
  code, bits: longword;
begin
  //length
  code := Length2code(len);
  bs.Write(len_tree[code].bits, len_tree[code].code_len);
  if (code >= 265) and (code < 285) then begin  //extra bits
      bits := 5 - (284 - code) div 4;
      bs.Write(len - 3, bits);
  end;

  //offset / distance
  code := Distance2code(dist);
  bs.Write(dist_tree[code].bits, dist_tree[code].code_len);
  if code >= 4 then begin
      bits := code div 2 - 1;
      bs.Write(dist - 1, bits);
  end;
end;

procedure TVlcWriter.WriteLiteral(const c: byte);
begin
  bs.Write(len_tree[c].bits, len_tree[c].code_len);
end;

procedure TVlcWriter.WriteBlockEnd;
begin
  bs.Write(len_tree[END_OF_BLOCK_CODE].bits, len_tree[END_OF_BLOCK_CODE].code_len);
end;

{ vlc_ReadCode
  Read one canonical huffman code using the given decoding table. Maximum symbol length cannot
  exceed 15 bits (maximum allowed by Deflate), otherwise reading fails and bad things happen.
}
function vlc_ReadCode(const bs: TBitstreamReader; const table: TDecodeTable): integer;
var
  i, codes,
  diff, value: longword;
  value_low: longword;      //lowest value for code of given length
  codes_skipped: longword;  //how many codes we already skipped
  number_of_codes: pword;   //# codes of given length
begin
  i := 0;
  value := 0;
  codes_skipped := 0;
  value_low := 0;
  codes := 0;
  number_of_codes := @table.codes_of_legth[0];
  repeat
      codes_skipped += codes;
      value_low += codes;
      value_low := value_low shl 1;

      i += 1;
      Assert(i < 16, 'could not read vlc code');
      codes := number_of_codes[i];

      value := (value shl 1) or bs.read();
      diff := value - value_low;
  until codes > diff;

  result := table.code_value[ codes_skipped + diff ];
end;


{ vlc_ReadCode
  Read one variable-length code using the given lookup table. If the code couldn't be read, try
  to read with the canon huff decoding table.
}
function vlc_ReadCode(const bs: TBitstreamReader; const dectable: TDecodeLookupTables): integer;
var
  bits: integer;
  sb: TSymbolBits;
begin
  bits := bs.Show(TAB0_BITS);
  sb := dectable.codes_t0[bits];
  result := sb.symbol;

  if (sb.nbits = 0) then begin
      result := vlc_ReadCode(bs, dectable.canon_table);
  end else
      bs.Skip(sb.nbits);
end;


{ InitDecodeLut
  Assign canonical huff code bits to each code by its length and build a look-up table for fast
  decoding. Uses separate code bits runs for each code length. Makes 2 passes over input data,
  one pass could be removed if code length stats were provided beforehand, but it doesn't gain much.
}
function InitDecodeLut(const code_lengths: pbyte; const count: integer): TDecodeLookupTables;
var
  i, len, code_bits: integer;
  value, k, b: integer;
  sb: TSymbolBits;
  num_lengths: array[0..15] of integer;  //# of codes of given length
  length_bits: array[0..15] of integer;  //canonical bits for codes of given length
begin
  FillByte(num_lengths, sizeof(num_lengths), 0);
  FillByte(length_bits, sizeof(length_bits), 0);
  for i := 0 to count - 1 do begin
      num_lengths[code_lengths[i]] += 1;
  end;
  b := 0;
  for i := 1 to 15 do begin
      length_bits[i] := b;
      b += num_lengths[i];
      b := b << 1;
  end;

  FillByte(result.codes_t0, sizeof(result.codes_t0), 0);
  for i := 0 to count - 1 do begin
      len := code_lengths[i];
      if not (len in [1..TAB0_BITS]) then
          continue;

      code_bits := length_bits[len];
      length_bits[len] += 1;
      sb.symbol := i;
      sb.nbits := len;

      //insert each code length + junk code_bits combination
      code_bits := SwapBits(code_bits, len);
      for k := 0 to 1 << (TAB0_BITS - len) - 1 do begin
          value := (k << len) or code_bits;
          result.codes_t0[value] := sb;
      end;
  end;
end;

{ TVlcReader }

procedure TVlcReader.SetTables(const bitreader: TBitstreamReader;
  const literal_table, distance_table: TDecodeLookupTables);
begin
  bs := bitreader;
  literal_dectable := literal_table;
  distance_dectable := distance_table;
end;

procedure TVlcReader.ReadCodePair(out length, distance: word);
const
  LITERAL_EXTRA_BITS: array[257..285] of byte = (
  0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4, 5, 5, 5, 5, 0
  );
var
  code, extra_bits: longword;
begin
  length := 1;
  code := vlc_ReadCode(bs, literal_dectable);

  //decode literals, length / distance, end of block
  if code < 256 then begin
      distance := code;
  end
  else if code > 256 then begin
      length := Code2length(code);
      extra_bits := LITERAL_EXTRA_BITS[code];
      if extra_bits > 0 then begin
          length += bs.Read(extra_bits);
      end;

      code := vlc_ReadCode(bs, distance_dectable);
      distance := Code2distance(code);
      if code >= 4 then begin
          distance += bs.Read(code >> 1 - 1);
      end;
  end
  else begin
      length := END_OF_BLOCK;
      distance := 0;
  end;
end;


const
  //code ordering for header code length alphabet
  //see RFC1951 section 3.2.7. Compression with dynamic Huffman codes (BTYPE=10)
  HeaderCodeLengthOrder: array[0..18] of byte = (
      16, 17, 18, 0, 8, 7, 9, 6, 10, 5, 11, 4, 12, 3, 13, 2, 14, 1, 15
  );

  LITERAL_MATCH_ELEMENTS = END_OF_STREAM + 1;
  DISTANCE_ELEMENTS = 30;

type
  TRleResult = record
      size: integer;
      nonzero: integer;
      rl_pairs: array[0..LITERAL_MATCH_ELEMENTS-1] of record
          code_len, repeats: byte;
      end;
      code_lengths: array[0..LITERAL_MATCH_ELEMENTS-1] of byte;
  end;

{
  Build length-limited huffman code tree.
  Length is limited by reducing the code occurence's statistics.
  Less accuracy means that the differences between code lengths are reduced, too.
  This is somewhat suboptimal.

  Distance trees are special, because there are cases where they contain only one used symbol.
}
procedure build_limited_tree(var tree: huff_tree_t; limit, size: word; const for_distance: boolean = false);
var
  i: integer;
  tree_ok: boolean;
  freq_limit: integer;
begin
  freq_limit := 256*2;
  repeat
      if for_distance then
          huff_build_distance_tree(tree, freq_limit)
      else
          huff_build_tree(tree, freq_limit);
      tree_ok := true;
      for i := 0 to size - 1 do
          if tree.codes[i].code_len > limit then begin
              tree_ok := false;
              freq_limit := freq_limit shr 1;
              break;
          end;
  until tree_ok;
  //reverse bits for faster bitwriting
  for i := 0 to size - 1 do begin
      tree.codes[i].bits := SwapBits(tree.codes[i].bits, tree.codes[i].code_len);
  end;
end;

{
  WriteCodeLengths

  Write lengths of tree's codes using, encoded by header_codes
}
procedure WriteCodeLengths
  (const bs: TBitstreamWriter; const header_codes, tree_codes: vlc_code_p; const size: integer);
var
  i, k: integer;
  bits, length: longword;
begin
  for i := 0 to size - 1 do begin
      k := tree_codes[i].code_len;
      bits   := header_codes[k].bits;
      length := header_codes[k].code_len;
      bs.Write(bits, length);
  end;
end;


procedure WriteCodeLengthsRle
  (const bs: TBitstreamWriter; const header_codes: vlc_code_p; const rle_res: TRleResult);
var
  i, k: integer;
  bits, length: longword;
  size: integer;
begin
  size := rle_res.size;
  i := 0;
  while i < size do begin
      k := rle_res.rl_pairs[i].code_len;
      bits   := header_codes[k].bits;
      length := header_codes[k].code_len;
      bs.Write(bits, length);

      if k = 16 then begin //Copy the previous code length, the next 2 bits indicate repeat length
          bs.Write(rle_res.rl_pairs[i].repeats, 2);
      end
      else if k = 17 then begin  //Repeat a code length of 0, 3 bits of length
          bs.Write(rle_res.rl_pairs[i].repeats, 3);
      end
      else if k = 18 then begin  //Repeat a code length of 0, 7 bits of length
          bs.Write(rle_res.rl_pairs[i].repeats, 7);
      end;
      i += 1;
  end;
end;


{
  GetHclen

  Reduce the number of header tree code lengths that need to be stored.
  Saves a couple of bits per block.
}
function GetHclen(const min_length, max_length: integer): integer;
var
  i, length: integer;
begin
  result := 19;
  for i := 18 downto 5 do begin
      length := HeaderCodeLengthOrder[i];
      if (length >= min_length) and (length <= max_length) then
          break
      else
          result -= 1;
  end;
  result -= 4;
end;


function RleCodeBuffer(const src: array of byte; const size: integer): TRleResult;
var
  i, k, m: integer;
  value, next_value: byte;
  max_lookahead: integer;
  run_length: integer;
  nonzero: integer;

  procedure AddRl(const code_len, repeats: byte);
  begin
    result.rl_pairs[m].code_len := code_len;
    result.rl_pairs[m].repeats := repeats;
    m += 1;
  end;

begin
  nonzero := 0;
  i := 0;
  m := 0;
  while i < size do begin
      value := src[i];
      //zero runs
      if value = 0 then begin
          max_lookahead := 137;
          if size - i < max_lookahead then
              max_lookahead := size - i;

          run_length := 1;
          for k := i + 1 to i + max_lookahead - 1 do begin
              next_value := src[k];
              if value = next_value then
                  run_length += 1
              else
                  break;
          end;
          if run_length < 3 then begin
              AddRl(0, 0);
              i += 1;
          end else begin
              i += run_length;
              if run_length < 11 then
                  AddRl(17, run_length - 3) //Repeat a code length of 0 for 3 - 10 times.
              else
                  AddRl(18, run_length - 11); //Repeat a code length of 0 for 11 - 138 times.
          end;
      end
      //nonzero runs
      else begin
          max_lookahead := 6;
          if size - i <= max_lookahead then
              max_lookahead := size - i - 1;

          run_length := 0;
          for k := i + 1 to i + max_lookahead do begin
              next_value := src[k];
              if value = next_value then
                  run_length += 1
              else
                  break;
          end;
          if run_length >= 3 then begin
              AddRl(value, 0);
              AddRl(16, run_length - 3); //Copy the previous code length 3 - 6 times.
              i += run_length + 1;
              nonzero += run_length + 1;
          end else begin
              AddRl(value, 0);
              i += 1;
              nonzero += 1;
          end;
      end;
  end;

  result.size := m;
  result.nonzero := nonzero;

  for i := 0 to result.size - 1 do
      result.code_lengths[i] := result.rl_pairs[i].code_len;
end;


{ TBlockWriter }

{
  BeginBlock

  Write block header.
  header bytes:
    BFINAL - 1 bit
    BTYPE  - 2 bits
}
procedure TBlockWriter.BeginBlock;
begin
  bitWriter.Write(longword( _last ) and 1);
  bitWriter.Write(longword( _block_type ), 2);
end;

{ BuildFixedHuffCodes
  Create vlc trees for blocks compressed using fixed Huffman codes
}
procedure TBlockWriter.BuildFixedHuffCodes;
var
  i, bits: integer;

  function vlc(const b, len: integer): vlc_code_t;
  begin
    result.bits := SwapBits(b, len);
    result.code_len := len;
    bits += 1;
  end;

begin
  bits := 0;
  for i := 256 to 279 do literal_codes[i] := vlc(bits, 7);
  bits := bits << 1;
  for i :=   0 to 143 do literal_codes[i] := vlc(bits, 8);
  for i := 280 to 287 do literal_codes[i] := vlc(bits, 8);
  bits := bits << 1;
  for i := 144 to 255 do literal_codes[i] := vlc(bits, 9);
  for i := 0 to 29 do distance_codes[i] := vlc(i, 5);
end;

{ BuildHuffCodes
  Create vlc trees from accumulated statistics for literal/length and distance coding
}
procedure TBlockWriter.BuildHuffCodes;
var
  i: integer;
  tree: huff_tree_t;
begin
  //generate literal/match codes
  huff_init(tree);
  for i := 0 to LITERAL_MATCH_ELEMENTS - 1 do
      tree.counts[i] := literal_match_stats[i];
  tree.counts[END_OF_BLOCK_CODE] := 1;

  build_limited_tree(tree,  15, LITERAL_MATCH_ELEMENTS);
  Move(tree.codes ^, literal_codes ^, LITERAL_MATCH_ELEMENTS * sizeof(vlc_code_t));
  huff_free(tree);

  //generate distance codes
  huff_init(tree);
  for i := 0 to DISTANCE_ELEMENTS - 1 do
      tree.counts[i] := distance_stats[i];

  build_limited_tree(tree, 15, DISTANCE_ELEMENTS, true);
  Move(tree.codes ^, distance_codes ^, DISTANCE_ELEMENTS * sizeof(vlc_code_t));
  huff_free(tree);
end;

{
  WriteCodingTrees

  Store literal/length and distance trees.
  Canonical huff coding is used, so code lengths are enough to store the trees.
}
procedure TBlockWriter.WriteCodingTrees;
var
  max_used_codelength, min_used_codelength: integer;
  block_header_tree: huff_tree_t;

  procedure UpdateCodeLengthStats(const code_lengths: array of byte; const size: integer);
  var
    i: integer;
    length: integer;
  begin
    i := 0;
    while i < size do begin
        length := code_lengths[i];
        huff_raise_count(block_header_tree, length);
        i += 1;
        if (length < min_used_codelength) and (length > 0) then min_used_codelength := length;
        if length > max_used_codelength then max_used_codelength := length;
    end;
  end;

var
  header_codes: vlc_code_p;
  hclen, hlit, hdist: integer;
  i: integer;

  codelen_buffer: array[0..LITERAL_MATCH_ELEMENTS - 1] of byte;
  distance_rle: TRleResult;
  literal_rle: TRleResult;
begin
  //build header tree for coding the literal/length and distance tree code lengths
  max_used_codelength := 0;
  min_used_codelength := 15;
  huff_init(block_header_tree);

  for i := 0 to LITERAL_MATCH_ELEMENTS - 1 do
      codelen_buffer[i] := literal_codes[i].code_len;
  literal_rle := RleCodeBuffer(codelen_buffer, LITERAL_MATCH_ELEMENTS);
  UpdateCodeLengthStats(literal_rle.code_lengths, literal_rle.size);

  for i := 0 to DISTANCE_ELEMENTS - 1 do
      codelen_buffer[i] := distance_codes[i].code_len;
  distance_rle := RleCodeBuffer(codelen_buffer, DISTANCE_ELEMENTS);
  UpdateCodeLengthStats(distance_rle.code_lengths, distance_rle.size);

  build_limited_tree(block_header_tree, 7, 19);

  //store the header tree code lengths
  //when do we want to define a smaller hdist? if we used a smaller encoding window, thus limiting the distances?
  hlit := 286 - 257;
  hdist := 29;
  hclen := GetHclen(min_used_codelength, max_used_codelength);
  bitWriter.Write(hlit, 5);
  bitWriter.Write(hdist, 5);
  bitWriter.Write(hclen, 4);
  header_codes := block_header_tree.codes;
  for i := 0 to hclen + 4 - 1 do
      bitWriter.Write( header_codes[ HeaderCodeLengthOrder[i] ].code_len, 3 );

  //store codes of literal/length and distance trees
  WriteCodeLengthsRle(bitWriter, header_codes, literal_rle);
  WriteCodeLengthsRle(bitWriter, header_codes, distance_rle);

  huff_free(block_header_tree);
end;

{ WriteBlockEncoded
  Write a complete block into bitstream: literals and match length / distance pairs, END_OF_BLOCK symbol.
  Handles distinction between blocks compressed using fixed or dynamic huff codes
}
procedure TBlockWriter.WriteBlockEncoded(const search_results: PLiteralMatch; const size: integer);
var
  i: integer;
  lm: TLiteralMatch;
  vlc: TVlcWriter;
begin
  if _block_type = BTDynamic then begin
      BuildHuffCodes;
      WriteCodingTrees();
  end else
      BuildFixedHuffCodes();
  vlc.SetTrees(bitWriter, literal_codes, distance_codes);

  for i := 0 to size - 1 do begin
      lm := search_results[i];
      if lm.match_length > 0 then
          vlc.WriteMatch(lm.match_length, lm.offset)
      else
          vlc.WriteLiteral(lm.literal);
  end;

  vlc.WriteBlockEnd();
end;

{
  WriteBlockRaw

  Write a raw block into bitstream: copy input values.
  Raw block header:
    n bits  - byte alignment
    16 bits - data length
    16 bits - inverted data length
}
procedure TBlockWriter.WriteBlockRaw(const rawdata: pbyte; const rawsize: integer);
var
  i: integer;
begin
  bitWriter.ByteAlign;
  bitWriter.Write(rawsize, 16);
  bitWriter.Write(rawsize xor $ffff, 16);

  for i := 0 to rawsize - 1 do
      bitWriter.Write(rawdata[i], 8);  //todo: memcpy
end;

constructor TBlockWriter.Create(const output_buffer: pbyte);
begin
  bitWriter := TBitstreamWriter.Create(output_buffer);
  bs_cache := bitWriter.GetState;

  literal_match_stats := GetMem(LITERAL_MATCH_ELEMENTS * sizeof(integer));
  distance_stats := Getmem(DISTANCE_ELEMENTS * sizeof(integer));

  literal_codes := GetMem(LITERAL_MATCH_ELEMENTS * sizeof(vlc_code_t));
  distance_codes := GetMem(DISTANCE_ELEMENTS * sizeof(vlc_code_t));
end;

destructor TBlockWriter.Destroy;
begin
  inherited Destroy;
  bitWriter.Free;
  Freemem(literal_match_stats);
  Freemem(distance_stats);
  Freemem(literal_codes);
  Freemem(distance_codes);
end;

procedure TBlockWriter.InitNewBlock(const block_type: TBlockTypeEnum);
begin
  FillDWord(literal_match_stats^, LITERAL_MATCH_ELEMENTS, 0);
  FillDWord(distance_stats^, DISTANCE_ELEMENTS, 0);

  bitWriter.SetState(bs_cache);
  _block_type := block_type;
  _last := false;
end;

procedure TBlockWriter.SetLast;
begin
  _last := true;
end;

procedure TBlockWriter.UpdateStatsMatch(const len, dist: longword);
begin
  literal_match_stats[ Length2code(len) ] += 1;
  distance_stats[ Distance2code(dist) ] += 1;
end;

procedure TBlockWriter.UpdateStatsLiteral(const literal: byte);
begin
  literal_match_stats[ literal ] += 1;
end;

procedure TBlockWriter.WriteBlock(const rawdata: pbyte; const rawsize: integer;
  const search_results: PLiteralMatch; const size: integer; const keep_buffer: boolean);
begin
  if not keep_buffer then
      bitWriter.ResetBufferPosition;
  BeginBlock();
  if _block_type <> BTRaw then
      WriteBlockEncoded(search_results, size)
  else
      WriteBlockRaw(rawdata, rawsize);
end;

procedure TBlockWriter.Done;
begin
  //throw away bs cache
  bs_cache := bitWriter.GetState;
end;

{
  GetStreamSize

  Returns number of whole bytes that were written into bitstream for current block.
  The last written bit doesn't have to be at a byte aligned position,
  so we need to cache the write buffer and mask to put the bits in the next processed block.
  If the current block is the last one processed, the outstanding bits must be counted into the stream size
  (they would be lost otherwise).
}
function TBlockWriter.GetStreamSize: integer;
begin
  if not _last then begin
      result := bitWriter.GetUnbufferedByteSize;
  end else begin
      bitWriter.Close;
      result := bitWriter.GetByteSize;
  end;
end;


{ TBlockReader }

{ ReadHeaderCodes
  Read code lengths and generate tables for dynamic block decoding
}
procedure TBlockReader.ReadHeaderCodes(const bs: TBitstreamReader);
const
  MAX_CODE_LENGTHS = 286 + 32; //# of Literal/Length codes + # of Distance codes
var
  literal_dectable,                       //literal/length decoding table
  distance_dectable: TDecodeLookupTables; //distance decoding table
  hlit, hdist, hclen: word;
  len, last_len: integer;  //code length, previous code length
  i, k, extra_bits: longword;
  code_lengths: array[0..MAX_CODE_LENGTHS-1] of byte;
  dt: TDecodeLookupTables;
begin
  hlit  := bs.Read(5) + 257;
  hdist := bs.Read(5) + 1;
  hclen := bs.Read(4) + 4;

  //get code_len codes
  FillByte(code_lengths, 19, 0);
  for i := 0 to hclen - 1 do begin
      k := HeaderCodeLengthOrder[i];
      code_lengths[k] := bs.Read(3);
  end;
  dt := InitDecodeLut(code_lengths, 19);

  //decode symbols
  FillByte(code_lengths, MAX_CODE_LENGTHS, 0);
  i := 0;
  last_len := 16;
  while i < hlit + hdist do begin
      len := vlc_ReadCode(bs, dt);

      if len < 16 then begin
           code_lengths[i] := len;
           i += 1;
           last_len := len;
      end
      else
          case len of
              16: begin  //rep previous length
                  Assert(last_len <> 16, 'dynamic block header error');
                  extra_bits := bs.Read(2);
                  k := i;
                  i += extra_bits + 3;
                  for k := k to i - 1 do
                      code_lengths[k] := last_len;
                  end;
              17: begin  //rep zero length
                  extra_bits := bs.Read(3);
                  i += extra_bits + 3;
                  end;
              18: begin  //rep zero length
                  extra_bits := bs.Read(7);
                  i += extra_bits + 11;
                  end
          end;
  end;

  literal_dectable  := InitDecodeLut(pbyte(@code_lengths), hlit);
  huff_FillCanonDecodingTable(literal_dectable.canon_table, pbyte(@code_lengths), hlit);

  distance_dectable := InitDecodeLut(pbyte(@code_lengths) + hlit, hdist);
  huff_FillCanonDecodingTable(distance_dectable.canon_table, pbyte(@code_lengths) + hlit, hdist);

  _vlc.SetTables(bs, literal_dectable, distance_dectable);
end;

procedure TBlockReader.InitFixedCodes(const bs: TBitstreamReader);
var
  literal_dectable,                       //literal/length decoding table
  distance_dectable: TDecodeLookupTables; //distance decoding table
  code_lengths: array[0..287] of byte;
  i: integer;
begin
  for i := 256 to 279 do code_lengths[i] := 7;
  for i :=   0 to 143 do code_lengths[i] := 8;
  for i := 280 to 287 do code_lengths[i] := 8;
  for i := 144 to 255 do code_lengths[i] := 9;
  literal_dectable := InitDecodeLut(pbyte(@code_lengths), 288);
  huff_FillCanonDecodingTable(literal_dectable.canon_table, pbyte(@code_lengths), 288);

  for i := 0 to 31 do code_lengths[i] := 5;
  distance_dectable := InitDecodeLut(pbyte(@code_lengths), 32);
  huff_FillCanonDecodingTable(distance_dectable.canon_table, pbyte(@code_lengths), 32);

  _vlc.SetTables(bs, literal_dectable, distance_dectable);
end;

constructor TBlockReader.Create;
begin
  _vlc := TVlcReader.Create;
end;

destructor TBlockReader.Destroy;
begin
  _vlc.Free;
  inherited;
end;

{ ReadBlockHeader
  Reads block header including length trees for codes
}
function TBlockReader.ReadBlockHeader(const bs: TBitstreamReader): TBlockContext;
var
  block: TBlockContext;
  t: integer;
begin
  block.last  := bs.Read() = 1;
  block.btype := TBlockTypeEnum( bs.Read(2) );
  _block_type := block.btype;

  case block.btype of
      BTRaw: begin
          while not bs.IsByteAligned() do
              bs.Read();
          block.size := bs.Read(16);
          t := not integer(bs.Read(16));
          Assert(block.size = t, 'blk size mismatch');
      end;
      BTDynamic: begin
          ReadHeaderCodes(bs);
      end;
      BTFixed: begin
          InitFixedCodes(bs);
      end;
  end;

  Result := block;
end;


function TBlockReader.GetVlcReader(): TVlcReader;
begin
  result := _vlc;
end;

end.

