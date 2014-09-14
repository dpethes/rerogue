program rs_repack;

uses
  sysutils, rsdat, rsdat_pack;

procedure UnpackData(const basedir: string);
var
  dat: TRSDatFile;
begin
  dat := TRSDatFile.Create;
  dat.ReadHeader(basedir + 'DATA.HDR');
  dat.ReadSections(basedir + 'DATA.DAT');
  dat.WriteFilesToDirectory(basedir);
  dat.Free;
end;

procedure PackData(const basedir: string);
var
  packer: TRSDatPacker;
begin
  packer := TRSDatPacker.Create;
  packer.PackDirectory(basedir);
  packer.Free;
end;

var
  mode: string;
  basedir: string;
begin
  if Paramcount < 2 then begin
      writeln ('usage: rs_repack [u|p] directory');
      halt;
  end;
  mode := ParamStr(1);
  basedir := ParamStr(2);
  if mode = 'u' then begin
      UnpackData(basedir);
  end;
  if mode = 'p' then begin
      PackData(basedir);
  end;
  writeln('done.')
end.

