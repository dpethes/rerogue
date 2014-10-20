program parse_hmt;
{$mode objfpc}{$H+}

uses
  sysutils, hmt_parser, rs_image;

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

procedure SaveImage(var image: TRSImage; const outname: string);
begin
  case image.type_ of
      0: pnm_save(outname + '.pnm', image.pixels, image.width, image.height);
      //3: WriteTga(outname + '.tga', image.pixels, image.width, image.height, image.width * image.height * 4);
      4: pgm_save(outname + '.pgm', image.pixels, image.width, image.height);
  else
      writeln('image type was not saved: ', image.type_);
  end;
end;

var
  fname: string;
  hmt: THmtFile;
  i: integer;

begin
  if ParamCount < 1 then begin
      writeln ('no input file specified');
      exit;
  end;

  fname := ParamStr(1);
  writeln('parsing file: ', fname);
  try
      hmt := ParseHmtFile(fname);
      writeln('saving textures');
      for i := 0 to hmt.texture_count - 1 do
          SaveImage(hmt.textures[i].image, fname + '_' + hmt.textures[i].name_string);
  except
      writeln('parsing failed!');
  end;
  writeln('done.');
end.

