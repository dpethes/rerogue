unit prediction;
{$mode objfpc}

interface

type
  { TPngPredict }
  //why object? to cache row_data buffer in the future perhaps?
  TPngPredict = object
    public
      function PredictData(const pixels: pbyte; const width, height, samples_per_pixel: integer): pbyte;
  end;


//**************************************************************************************************
implementation

function filter_sub(dst, src: pbyte; const stride: integer): integer;
var
  i: integer;
  delta: integer;
begin
  result := 0;
  dst += 3;
  for i := 0 to stride - 4 do begin
      delta := dst^ - src^;
      dst^ := byte(delta);
      result += abs(delta);
      dst += 1;
      src += 1;
  end;
end;

function filter_up(dst, src_above: pbyte; const stride: integer): integer;
var
  i: integer;
  delta: integer;
begin
  result := 0;
  for i := 0 to stride - 1 do begin
      delta := dst^ - src_above^;
      dst^ := byte(delta);
      result += abs(delta);
      dst += 1;
      src_above += 1;
  end;
end;

function filter_average(dst, src, src_above: pbyte; const stride: integer): integer;
var
  i: integer;
  delta: integer;
begin
  result := 0;
  for i := 0 to 2 do begin
      delta := dst[i] - src_above[i] shr 1;
      dst[i] := byte(delta);
      result += abs(delta);
  end;
  dst += 3;
  src_above += 3;
  for i := 0 to stride - 4 do begin
      delta := integer(dst^) - integer(src_above^ + src^) shr 1;
      dst^ := byte(delta);
      result += abs(delta);
      dst += 1;
      src += 1;
      src_above += 1;
  end;
end;

//a,b,c = left, above, upper left
function paeth_predictor (a, b, c: integer): integer; inline;
var
  p, pa, pb, pc: integer;
begin
  p := a + b - c;
  pa := abs(p - a);
  pb := abs(p - b);
  pc := abs(p - c);
  if (pa <= pb) and (pa <= pc) then
      result := a
  else
      if (pb <= pc) then
          result := b
      else
          result := c;
end;

//for the first pixel, take the difference against top only. then do paeth
function filter_paeth(dst, src, src_above: pbyte; const stride: integer): integer;
var
  i: integer;
  delta: integer;
begin
  result := 0;
  for i := 0 to 2 do begin
      delta := dst[i] - src_above[i];
      dst[i] := byte(delta);
      result += abs(delta);
  end;
  dst += 3;
  src_above += 3;
  for i := 0 to stride - 4 do begin
      delta := dst^ - paeth_predictor(src^, src_above^, (src_above - 3)^);
      dst^ := byte(delta);
      result += abs(delta);
      dst += 1;
      src += 1;
      src_above += 1;
  end;
end;

function sum_line(p: pbyte; length: integer): integer;
var
  i: integer;
begin
  result := p[0];
  for i := 1 to length - 1 do begin
      if (p[i-1] <> p[i]) then
          result += p[i];
  end;
end;


function Predict(const pixels: pbyte; const width, height, samples_per_pixel: integer): pbyte;
var
  stride: integer;
  best_score: integer;
  best_prediction: integer;
  row_buffer: array[0..4] of pbyte;
  row_src: pbyte;

  procedure TestPrediction(const pred_mode: integer);
  var
    row_score: integer;
  begin
    case pred_mode of
         0: row_score := sum_line(row_buffer[0], stride);
         //1: row_score := filter_sub(row_buffer[1], row_src, stride);
         2: row_score := filter_up (row_buffer[2], row_src - stride, stride);
         //3: row_score := filter_average(row_buffer[3], row_src, row_src - stride, stride);
         //4: row_score := filter_paeth  (row_buffer[4], row_src, row_src - stride, stride);
    else
        row_score := MaxInt;
    end;
    if row_score < best_score then begin
        best_prediction := pred_mode;
        best_score := row_score;
    end;
  end;

var
  i, y, pred_mode: integer;
  row_dst: pbyte;
begin
  stride := width * samples_per_pixel;
  result := getmem(height * (stride + 1));
  //buffer for filter = none is set to original data, so no allocation is needed
  row_buffer[1] := getmem(stride * 4);
  for i := 1 to 3 do
      row_buffer[i + 1] := row_buffer[1] + stride * i;

  for y := 0 to height - 1 do begin
      //setup pointers and copy input data to processing buffers: TestPrediction is destructive
      row_src := pixels + y * stride;
      row_buffer[0] := row_src;
      for i := 1 to 4 do
          move(row_src^, row_buffer[i]^, stride);

      //only filter = none, filter = sub can be used on the first pixel row: there's no other row to predict from
      //Also use early termination if score gets below treshold to gain some speed.
      best_score := MaxInt;
      best_prediction := 0;
      TestPrediction(1);
      if y > 0 then begin
          for pred_mode := 2 to 4 do begin
              if best_score < stride then
                  break;
              TestPrediction(pred_mode);
          end;
      end;
      if best_prediction = 1 then
          TestPrediction(0);

      //save best prediction mode and predicted data to output
      row_dst := result + y * (1 + stride);
      row_dst^ := best_prediction;
      move(row_buffer[best_prediction]^, (row_dst + 1)^, stride);
  end;

  freemem(row_buffer[1]);
end;

{ TPngPredict }

function TPngPredict.PredictData(const pixels: pbyte; const width, height,
  samples_per_pixel: integer): pbyte;
begin
  result := Predict(pixels, width, height, samples_per_pixel);
end;

end.

