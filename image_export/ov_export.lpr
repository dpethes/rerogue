program fpic_export;

uses
  sysutils, classes, rs_image;

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

procedure WriteTga(const filename: string; const data: pbyte; const width, height, data_length: integer);
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
      0: pnm_save(outname + '.pnm', image.pixels, image.width, image.height);
      1: pnm_save(outname + '.pnm', image.pixels, image.width, image.height);
      2: pgm_save(outname + '.pgm', image.pixels, image.width, image.height);
      3: WriteTga(outname + '.tga', image.pixels, image.width, image.height, image.width * image.height * 4);
      4: pgm_save(outname + '.pgm', image.pixels, image.width, image.height);
      5: pgm_save(outname + '.pgm', image.pixels, image.width, image.height);
  end;
end;


procedure ReadImagePack(var f: file; const fname: string);
var
  image_count: integer;
  file_ptr: int64;
  outname: string;
  image: TRSImage;
begin
  image_count := 0;
  file_ptr := 0;
  while file_ptr < FileSize(f) do begin
      writeln('reading at: ', file_ptr);
      outname := fname;
      if image_count > 0 then
          outname += '_' + IntToStr(image_count);

      image := LoadImageFromPack(f);
      if image.pixels <> nil then
          SaveImage(image, outname);
      //freemem(image.pixels);

      image_count += 1;
      file_ptr := FilePos(f);
      if file_ptr mod 4 <> 0 then begin
          file_ptr := (file_ptr div 4 + 1) * 4;
          if file_ptr < FileSize(f) then begin
              writeln('seeking to mod4 file position');
              Seek(f, file_ptr);
          end;
      end;
  end;
end;


procedure LoadImageFile(const fname: string);
var
  f: file;
begin
  AssignFile(f, fname);
  Reset(f, 1);

  ReadImagePack(f, fname);

  CloseFile(f);
end;


//main
var
  fname: string;

begin
  if Paramcount < 1 then begin
      writeln('no file specified');
      halt;
  end;

  fname := ParamStr(1);
  LoadImageFile(fname);
  writeln('done.');
end.

