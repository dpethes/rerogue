(*******************************************************************************
dc2_encoder.pas
Copyright (c) 2014-2015 David Pethes

This file is part of Dc2.

Dc2 is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

Dc2 is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Dc2.  If not, see <http://www.gnu.org/licenses/>.

*******************************************************************************)
unit dc2_encoder;
{$mode objfpc}{$H+}

interface

uses
  SysUtils, dc2core;

const
  DefaultCompLevel = 5;

type
  TEncodedSlice = record
      data: pbyte;
      size: integer;
  end;

  { TDc2Encoder }

  TDc2Encoder = class
  private type
      TEncoderState = (stStartBlock, stProcessData, stWriteBlock);
  private
      dict: TMatchSearcher;              //string dictionary
      search_results: PLiteralMatch;     //dictionary search results buffer
      blockCoder: TBlockWriter;          //deflate block writing backend

      output_buffer: pbyte;              //buffer for encoded data, used by bitstream writer
      encoded_items: integer;            //encoded item count
      slices_stored: integer;            //number of input slices in current block

      block: record                      //current deflate block data
          btype: TBlockTypeEnum;         //type (fixed/dynamic/raw)
          size:  integer;                //size in bytes for raw blocks, coding elements otherwise
          last:  boolean;                //last block in deflate stream
      end;
      state: TEncoderState;
      clevel: byte;                      //compression level
      use_fixed_huff_only: boolean;      //static huffcodes only
      stats: stats_t;                    //block statistics

      procedure SearchMatches(const stream: pbyte; const size: integer);

  public
      constructor Create(const compression_level: byte; const fixed_code_only: boolean = false);
      destructor Destroy; override;
      //Encode given block of data
      procedure EncodeSlice(const data: pbyte; const input_size: longword; out slice: TEncodedSlice);
      //Signal the last input block
      procedure SetLastSlice;
      //encoded data statistics
      function GetStats: stats_t;
  end;

implementation

{ TDc2Encoder }

constructor TDc2Encoder.Create(const compression_level: byte; const fixed_code_only: boolean);
begin
  clevel := compression_level;
  if clevel > MAX_COMPRESSION_LEVEL then
      clevel := MAX_COMPRESSION_LEVEL;
  use_fixed_huff_only := fixed_code_only;

  dict := TMatchSearcher.Create;
  dict.SetCompressionLevel(clevel);

  output_buffer := getmem(MAX_BLOCK_SIZE * 2);  //padding for the case of data expansion
  blockCoder := TBlockWriter.Create(output_buffer);

  search_results := getmem(MAX_BLOCK_SIZE * 2 * sizeof(TLiteralMatch));

  state := stStartBlock;
  block.last := false;
  Fillbyte(stats, sizeof(stats_t), 0);
end;

destructor TDc2Encoder.Destroy;
begin
  dict.Free;
  blockCoder.Free;
  freemem(output_buffer);
  freemem(search_results);
  inherited;
end;

{
  EncodeSlice

  Encode given piece of data:
    - dictionary frontend: search for duplicate strings
    - block coder backend: store search results in Deflate format
}
procedure TDc2Encoder.EncodeSlice(const data: pbyte; const input_size: longword; out slice: TEncodedSlice);
var
  processed_size: integer;

  procedure WriteBlock;
  var
    append_bs_buffer: boolean;
  begin
    //write literals/matches to bitstream
    if block.last then blockCoder.SetLast;
    append_bs_buffer := slice.size > 0;
    blockCoder.WriteBlock(data, input_size, search_results, encoded_items, append_bs_buffer);

    //failed to compress the block - use raw block (if all data is available)
    if (blockCoder.GetStreamSize >= input_size) and (slices_stored = 1) and not block.last then begin
        block.btype := BTRaw;
        blockCoder.InitNewBlock(block.btype);
        blockCoder.WriteBlock(data, input_size, search_results, encoded_items);
        encoded_items := input_size;
    end;

    //commit block data
    blockCoder.Done;
    slice.data := output_buffer;
    slice.size += blockCoder.GetStreamSize();

    stats.elements_encoded += encoded_items;
    stats.blocks[block.btype] += 1;
  end;

begin
  Assert(input_size <= MAX_BLOCK_SIZE, 'unexpected input data size');

  //if no compression is specified, process the data as raw block
  if clevel = 0 then begin
      blockCoder.InitNewBlock(BTRaw);
      if block.last then blockCoder.SetLast;
      blockCoder.WriteBlock(data, input_size, search_results, 0);
      blockCoder.Done;
      slice.data := output_buffer;
      slice.size := blockCoder.GetStreamSize();
      stats.blocks[block.btype] += 1;
      exit;
  end;

  processed_size := 0;
  slice.data := nil;
  slice.size := 0;

  repeat
      case state of
      stStartBlock: begin
          //use fixed huffcode block if the input stream is too small, the overhead of dynamic block header would kill any compression
          block.btype := BTDynamic;
          if use_fixed_huff_only or (input_size < 200) then
              block.btype := BTFixed;

          blockCoder.InitNewBlock(block.btype);
          state := stProcessData;
          encoded_items := 0;
          slices_stored := 0;
      end;
      stProcessData: begin
          //search input buffer for matches
          SearchMatches(data, input_size);
          processed_size := input_size;
          slices_stored += 1;
          //save block if no more data should go to block or last data was processed
          if (encoded_items > MAX_BLOCK_SIZE div 8) or (slices_stored = 4) or block.last then
               state := stWriteBlock;
      end;
      stWriteBlock: begin
          WriteBlock;
          state := stStartBlock;
      end;
      end;
  until (state in [stProcessData, stStartBlock]) and (processed_size = input_size);
end;

procedure TDc2Encoder.SetLastSlice;
begin
  block.last := true;
end;

function TDc2Encoder.GetStats: stats_t;
begin
  result := stats;
end;

function InitMatch(const sr: TSearchResult): TLiteralMatch; inline;
begin
  result.match_length := sr.length;
  result.offset := sr.distance;
  result.literal := 0;
end;

function InitMatch(const l: byte): TLiteralMatch; inline;
begin
  result.match_length := 0;
  result.offset := 0;
  result.literal := l;
end;

{ SearchMatches
  Search stream for duplicate strings using a dictionary built from gathered data
}
procedure TDc2Encoder.SearchMatches(const stream: pbyte; const size: integer);
var
  lm: TLiteralMatch;
  lms: PLiteralMatch;
  match, match_lazy: TSearchResult;
  i: integer;
  literal: byte;
begin
  dict.NewData(stream, size);
  i := 0;
  lms := search_results + encoded_items;
  while i < size do begin
      match := dict.FindMatch(stream + i, i);
      literal := stream[i];

      if match.length >= MIN_MATCH_LENGTH then begin

          //lazy matching, biases are experimental and not tuned much
          if (clevel >= 5) and (i < size - 1) then begin
              if (match.length > 1) and (match.length < MAX_DEFLATE_MATCH_LENGTH-3) then begin

                  match_lazy := dict.FindMatch(stream + i + 1, i + 1);
                  if match_lazy.length > match.length + 2 then begin
                      i += 1;
                      lms^ := InitMatch(literal);
                      lms += 1;
                      blockCoder.UpdateStatsLiteral(literal);

                      i += match_lazy.length;
                      lms^ := InitMatch(match_lazy);
                      lms += 1;
                      blockCoder.UpdateStatsMatch(match_lazy.length, match_lazy.distance);

                      Continue;
                  end;

              end;
          end;
          //end lazy matching

          i += match.length;
          lm := InitMatch(match);
          blockCoder.UpdateStatsMatch(match.length, match.distance);
      end
      else begin
          i += 1;
          lm := InitMatch(literal);
          blockCoder.UpdateStatsLiteral(literal);
      end;

      lms^ := lm;
      lms += 1;
  end;
  encoded_items := lms - search_results;
end;

end.

