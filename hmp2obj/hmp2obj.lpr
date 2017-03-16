program hmp2obj;

uses
  sysutils,
  rs_world;

function TestPath(s: string): string;
begin
  result := s;
  if not FileExists(s) then begin
      writeln('file doesn''t exist: ', s);
      Halt;
  end;
end;

var
  tex_fname, text_fname, hmp_fname: string;
  world: TWorld;

begin
  if Paramcount < 2 then begin
      writeln('not enough files specified');
      writeln('usage: hmp2obj hmp text tex');
      halt;
  end;

  hmp_fname  := TestPath(ParamStr(1));
  text_fname := TestPath(ParamStr(2));
  tex_fname  := TestPath(ParamStr(3));

  world := TWorld.Create;
  world.LoadFromFiles(hmp_fname, text_fname, tex_fname);

  writeln('world loaded');
  writeln('tile size: ', world.TileWidth, 'x', world.TileHeight);
  writeln('scale: ', world.MapScale:7:3);

  world.ExportToObj('heightmap.obj');
  world.ExportToRaw('heightmap.raw');
  writeln('world exported');

  world.Free;
end.

