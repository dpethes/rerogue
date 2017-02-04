program parse_hmt;
{$mode objfpc}{$H+}

uses
  classes, sysutils, hmt_parser, rs_image;

procedure pnm_save(const fname: string; const p: pbyte; const w, h: integer);
var
  f: file;
  c: PChar;
Begin
  c := PChar(format('P6'#10'%d %d'#10'255'#10, [w, h]));
  AssignFile (f, fname);
  Rewrite (f, 1);
  BlockWrite (f, c^, strlen(c));
  BlockWrite (f, p^, w * h * 3);
  CloseFile (f);
end;

procedure pgm_save(fname: string; p: pbyte; w, h: integer) ;
var
  f: file;
  c: PChar;
Begin
  c := PChar(format('P5'#10'%d %d'#10'255'#10, [w, h]));
  AssignFile (f, fname);
  Rewrite (f, 1);
  BlockWrite (f, c^, strlen(c));
  BlockWrite (f, p^, w * h);
  CloseFile (f);
end;

procedure WriteTga32b(const filename: string; const data: pbyte; const width, height, data_length: integer);
const
  HeaderComment = 'NZA';
var
  f: file;
  stream: TMemoryStream;
begin
  stream := TMemoryStream.Create();
  stream.WriteByte(Length(HeaderComment)); //id field length
  stream.WriteByte (0);  //color map type
  stream.WriteByte (2);  //image type: 2 = uncompressed true-color image
  stream.WriteDWord(0);  //5B color map specification: 2B origin, 2B length
  stream.WriteByte (0);  //                            1B Color Map Entry Size.
  stream.WriteDWord(0);      //2B x origin, 2B y origin
  stream.WriteWord (width);  //width in pixels
  stream.WriteWord (height); //height in pixels
  stream.WriteByte (32);     //bits per pixel
  stream.WriteByte ($20);    //image descriptor
  stream.Write(HeaderComment, Length(HeaderComment));

  AssignFile(f, filename);
  Rewrite(f, 1);
  blockwrite(f, stream.Memory^, stream.Size);
  blockwrite(f, data^, data_length);
  CloseFile(f);
  stream.Free;
end;

procedure SaveImage(var image: TRSImage; const outname: string);
begin
  case image.type_ of
      0, 1: pnm_save(outname + '.pnm', image.pixels, image.width, image.height);
      3: WriteTga32b(outname + '.tga', image.pixels, image.width, image.height, image.width * image.height * 4);
      4, 5: pgm_save(outname + '.pgm', image.pixels, image.width, image.height);
  else
      writeln('image type was not saved: ', image.type_);
  end;
end;

var
  fname: string;
  hmt: THmtFile;
  i: integer;
  stream: TMemoryStream;

begin
  if ParamCount < 1 then begin
      writeln ('no input file specified');
      exit;
  end;

  fname := ParamStr(1);
  writeln('parsing file: ', fname);
  try
      stream := TMemoryStream.Create;
      stream.LoadFromFile(fname);

      hmt := ParseHmtFile(stream);
      writeln('saving textures');
      for i := 0 to hmt.texture_count - 1 do
          SaveImage(hmt.textures[i].image, '_' + fname + '_' + hmt.textures[i].name_string);

      hmt.materials := nil;
      if hmt.texture_count > 0 then hmt.textures := nil;
      stream.Free;
  except
      writeln('parsing failed!');
  end;
  writeln('done.');
end.

