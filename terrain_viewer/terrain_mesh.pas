unit terrain_mesh;
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  gl,
  rs_world;

type
 TRenderOpts = record
      wireframe: boolean;
      points: boolean;
      vcolors: boolean;
      textures: boolean;
      fg_to_draw: integer;
  end;

  { TTerrainMesh }
  TTerrainMesh = class
    private
      terrain: TWorld;
    public
      destructor Destroy; override;
      procedure Load(const hmp_filename: string);
      procedure InitGL;
      procedure DrawGL(opts: TRenderOpts);
  end;

implementation

destructor TTerrainMesh.Destroy;
begin
  inherited Destroy;
end;

procedure TTerrainMesh.Load(const hmp_filename: string);
begin
   terrain := TWorld.Create;
   terrain.LoadFromFiles(hmp_filename, '', '');
   terrain.ExportToObj('');  //generate vertices
end;

//generate textures / texture atlas?
procedure TTerrainMesh.InitGL;
begin

end;

//draw vertices / tiles around actual position
procedure TTerrainMesh.DrawGL(opts: TRenderOpts);
var
  i: integer;
  v: TVertex3f;
begin
  glBegin(GL_POINTS);
  glColor3f(0, 1, 0);
  for i := 0 to terrain.vertex_count - 1 do begin
      v := terrain.vertex_array[i];
      glVertex3fv(@v);
  end;
  glEnd;
end;

end.

