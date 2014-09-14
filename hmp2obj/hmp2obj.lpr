program hmp2obj;

uses rs_world;

var
  tex_fname, text_fname, hmp_fname: string;
  world: TWorld;

begin
  if Paramcount < 2 then begin
      writeln('not enough files specified');
      writeln('usage: hmp2obj hmp text tex');
      halt;
  end;

  hmp_fname := ParamStr(1);
  text_fname := ParamStr(2);
  tex_fname := ParamStr(3);

  world := TWorld.Create;
  world.LoadFromFiles(hmp_fname, text_fname, tex_fname);

  writeln('world loaded');
  writeln('tile size: ', world.TileWidth, 'x', world.TileHeight);

  world.ExportToObj('heightmap.obj');
  writeln('world exported');

  world.Free;
end.

