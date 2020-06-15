{ HMP terrain viewer

  Copyright (c) 2015 David Pethes

  Permission is hereby granted, free of charge, to any person obtaining a copy of this software
  and associated documentation files (the "Software"), to deal in the Software without
  restriction, including without limitation the rights to use, copy, modify, merge, publish,
  distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the
  Software is furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in all copies or
  substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING
  BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
  NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
  DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
}
program terrain_viewer;

uses
  sysutils, math,
  gl, glu, glext, gvector, sdl2, fpimgui, fpimgui_impl_sdlgl2,
  terrain_mesh, rs_dat, rs_world;

const
  SCR_W_INIT = 1280;
  SCR_H_INIT = 720;
  RotationAngleIncrement = 1;
  ZoomIncrement = 0.3;
  MouseZoomDistanceMultiply = 0.15;
  PitchIncrement = 0.5;
  MouseTranslateMultiply = 0.025;

var
  g_window: PSDL_Window;
  g_ogl_context: TSDL_GLContext;

  g_rs_files: TRsDatFileNodeList;
  g_levels: TLevelList;
  g_selected_level_idx: integer = 0;

  terrain: TTerrainMesh;

  view: record
      rotation_angle: single;
      distance: single;
      pitch: single;
      x, y: single;
      autorotate: boolean;
      render: TRenderOpts;
  end;

  mouse: record
      drag: boolean;
      translate: boolean;
      last_x, last_y: integer;
      resume_autorotate_on_release: boolean;
  end;


procedure AppError(s: string);
begin
  writeln(s);
  halt;
end;
  

// initial parameters
procedure InitGL;
const
  LightAmbient: array[0..3] of single = (0.5, 0.5, 0.5, 1);
  LightDiffuse: array[0..3] of single = (1, 1, 1, 1);
  LightPosition: array[0..3] of single = (1, 1, 0, 1);
var
  ogl_info: string;
begin
  ogl_info := format('vendor: %s renderer: %s', [glGetString(GL_VENDOR), glGetString(GL_RENDERER)]);
  writeln(ogl_info);
  ogl_info := 'version: ' + glGetString(GL_VERSION);
  writeln(ogl_info);

  glShadeModel( GL_SMOOTH );                  // Enable smooth shading
  glClearColor( 0.0, 0.3, 0.6, 0.0 );
  glClearDepth( 1.0 );                        // Depth buffer setup
  glEnable( GL_DEPTH_TEST );                  // Enables Depth Testing
  glDepthFunc( GL_LEQUAL );                   // The Type Of Depth Test To Do
  glHint( GL_PERSPECTIVE_CORRECTION_HINT, GL_NICEST );  // Really Nice Perspective Calculations

  //glEnable( GL_CULL_FACE );  //backface culling
  //glCullFace( GL_BACK );

  glEnable(GL_TEXTURE_2D);

  glEnable(GL_LIGHTING);
  glEnable(GL_LIGHT0);
  glLightfv(GL_LIGHT0, GL_AMBIENT, LightAmbient);
  glLightfv(GL_LIGHT0, GL_DIFFUSE, LightDiffuse);
  glLightfv(GL_LIGHT0, GL_POSITION,LightPosition);
end;


// function to reset our viewport after a window resize
procedure SetGLWindowSize( width, height : integer );
begin
  if ( height = 0 ) then
    height := 1;   // Protect against a divide by zero

  glViewport( 0, 0, width, height ); // Setup our viewport.
  glMatrixMode( GL_PROJECTION );     // change to the projection matrix and set our viewing volume.
  glLoadIdentity;
  gluPerspective( 45.0, width / height, 0.1, 1000.0 );  // Set our perspective

  glMatrixMode( GL_MODELVIEW );  // Make sure we're changing the model view and not the projection
  glLoadIdentity;                // Reset The View
end;

procedure DrawGui;
const
  LevelNames: array[0..19] of string = (
   'Ambush at Mos Eisley',
   'Rendezvous on Barkhesh',
   'The Search for the Nonnah',
   'Defection at Corellia',
   'Liberation of Gerrard V',
   'The Jade Moon',
   'Imperial Construction Yards',
   'Assault on Kile II',
   'Rescue on Kessel',
   'Prisons of Kessel',
   'Battle Above Taloraan',
   'Escape from Fest',
   'Blockade on Chandrila',
   'Raid on Sullust',
   'Moff Seerdon''s Revenge',
   'The Battle of Calamari',
   'Beggar''s Canyon',
   'The Death Star Trench Run',
   'The Battle of Hoth',
   'Intro'
  );
var
  item: TLevelListItem;
  i: Integer;
begin
  Imgui.Begin_('Rendering options');
  Imgui.Checkbox('points', @view.render.points);
  Imgui.Checkbox('wireframe', @view.render.wireframe);
  Imgui.End_;

  Imgui.Begin_('Level list');
  for i := 0 to g_levels.Size - 1 do begin
      item := g_levels[i];
      //some levels are broken atm
      if i in [7, 9, 15] then
          continue;
      if Imgui.Selectable(item.name + ' - ' + LevelNames[i], g_selected_level_idx = i) then begin
          g_selected_level_idx := i;
          terrain.Free;
          terrain := TTerrainMesh.Create;
          terrain.Load(g_levels[i]);
          terrain.InitGL;
      end;
  end;
  Imgui.End_;
end;

// The main drawing function.
procedure DrawTerrain;
begin
  glMatrixMode( GL_MODELVIEW );
  glLoadIdentity;

  if view.distance < ZoomIncrement then
      view.distance := ZoomIncrement;

  glTranslatef(view.x, view.y, -view.distance);
  glRotatef(view.rotation_angle, 0, 1, 0);
  glRotatef(view.pitch, 1, 0, 0);

  if view.autorotate then
      view.rotation_angle += RotationAngleIncrement;
  if view.rotation_angle > 360 then
      view.rotation_angle -= 360;

  terrain.DrawGL(view.render);
end;

procedure BeginScene;
var
  style: PImGuiStyle;
begin
  ImGui_ImplSdlGL2_NewFrame(g_window);
  style := Imgui.GetStyle();
  style^.WindowRounding := 0;
  glClear( GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT );
end;

procedure EndScene;
begin
  glDisable(GL_LIGHTING);
  igRender;
  glEnable(GL_LIGHTING);
  SDL_GL_SwapWindow(g_window);
end;


procedure WindowInit(w_width, w_height: integer);
var
  ver: TSDL_Version;
  x, y: integer;
  flags: longword;
  io: PImGuiIO;
begin
  SDL_GetVersion(@ver);
  writeln(format('SDL %d.%d.%d', [ver.major, ver.minor, ver.patch]));

  SDL_GL_SetAttribute(SDL_GL_DOUBLEBUFFER, 1);
  SDL_GL_SetAttribute(SDL_GL_DEPTH_SIZE,  24);
  SDL_GL_SetAttribute(SDL_GL_BUFFER_SIZE, 32);
  SDL_GL_SetAttribute(SDL_GL_RED_SIZE,     8);
  SDL_GL_SetAttribute(SDL_GL_GREEN_SIZE,   8);
  SDL_GL_SetAttribute(SDL_GL_BLUE_SIZE,    8);
  SDL_GL_SetAttribute(SDL_GL_ALPHA_SIZE,   8);

  WriteLn('init window: ', w_width, 'x', w_height);
  x := SDL_WINDOWPOS_CENTERED;
  y := SDL_WINDOWPOS_CENTERED;
  flags := SDL_WINDOW_SHOWN or SDL_WINDOW_OPENGL or SDL_WINDOW_RESIZABLE;
  g_window := SDL_CreateWindow('RS terrain viewer', x, y, w_width, w_height, flags);
  if g_window = nil then
      AppError ('SDL_CreateWindow failed. Reason: ' + SDL_GetError());

  g_ogl_context := SDL_GL_CreateContext(g_window);
  if g_ogl_context = nil then begin
      writeln ('SDL_GL_CreateContext failed. Reason: ' + SDL_GetError());
      halt;
  end;
  SDL_GL_SetSwapInterval(1); //enable VSync

  //setup imgui
  io := igGetIO();
  io^.DisplaySize.x := w_width;
  io^.DisplaySize.y := w_height;
  ImGui_ImplSdlGL2_Init();
end;

procedure WindowFree;
begin
  ImGui_ImplSdlGL2_Shutdown();
  SDL_GL_DeleteContext(g_ogl_context);
  SDL_DestroyWindow(g_window);
end;


procedure WindowScreenshot(const width, height : integer);
const
  head: array[0..8] of word = (0, 2, 0, 0, 0, 0, 0, 0, 24);
  counter: integer = 0;
var
  buf: pbyte;
  f: file;
  fname: string;
begin
  buf := getmem(width * height * 4);
  glReadBuffer(GL_FRONT);
  glReadPixels(0, 0, width, height, GL_BGR, GL_UNSIGNED_BYTE, buf);

  fname := format('screenshot_%.4d.tga', [counter]);
  AssignFile(f, fname);
  Rewrite(f, 1);
  head[6] := width;
  head[7] := height;
  BlockWrite(f, head, sizeof(head));
  BlockWrite(f, buf^, width * height * 3);
  CloseFile(f);
  counter += 1;

  Freemem(buf);
end;


procedure InitView;
begin
  view.rotation_angle := 0;
  view.distance := 160;
  view.pitch := 36;
  view.x := 0;
  view.y := 10;
  view.autorotate := false;
  view.render.wireframe := false;
  view.render.points := false;
  view.render.vcolors := true;
  view.render.textures := true;
end;


procedure HandleEvent(const event: TSDL_Event; var done: boolean);
var
  io: PImGuiIO;
begin
  ImGui_ImplSdlGL2_ProcessEvent(@event);
  io := igGetIO();
  if ((event.type_ = SDL_MOUSEBUTTONDOWN) or
     (event.type_ = SDL_MOUSEBUTTONUP) or
     (event.type_ = SDL_MOUSEWHEEL) or
     (event.type_ = SDL_MOUSEMOTION)) and io^.WantCaptureMouse then
      exit;
  if ((event.type_ = SDL_KEYDOWN) or (event.type_ = SDL_KEYUP)) and io^.WantCaptureKeyboard then
      exit;

  case event.type_ of
       SDL_QUITEV:
           Done := true;
       SDL_WINDOWEVENT: begin
           if event.window.event = SDL_WINDOWEVENT_RESIZED then
               SetGLWindowSize(event.window.data1, event.window.data2);
       end;

       SDL_KEYDOWN:
           case event.key.keysym.sym of
             SDLK_ESCAPE:
                 Done := true;
             SDLK_s:
                 WindowScreenshot(g_window^.w, g_window^.h);
             SDLK_PAGEUP:
                 view.distance += ZoomIncrement;
             SDLK_PAGEDOWN:
                 view.distance -= ZoomIncrement;
             SDLK_r:
                 view.autorotate := not view.autorotate;

             //terrain rendering opts
             SDLK_w:
                 view.render.wireframe := not view.render.wireframe;
             SDLK_v:
                 view.render.vcolors := not view.render.vcolors;
             SDLK_p:
                 view.render.points := not view.render.points;
             SDLK_t:
                 view.render.textures := not view.render.textures;
             SDLK_LEFT:
                 view.render.fg_to_draw := max(0, view.render.fg_to_draw - 1);
             SDLK_RIGHT:
                 view.render.fg_to_draw += 1;
           end;

       SDL_MOUSEBUTTONDOWN: begin
           mouse.resume_autorotate_on_release := view.autorotate;
           if event.button.button in [1..3] then begin
               mouse.drag := true;
               mouse.translate := event.button.button in [2];
               mouse.last_x := event.button.x;
               mouse.last_y := event.button.y;
               view.autorotate := false;
           end;
       end;
       SDL_MOUSEBUTTONUP: begin
           mouse.drag := false;
           view.autorotate := mouse.resume_autorotate_on_release;
       end;
       SDL_MOUSEWHEEL: begin
           if event.wheel.y < 0 then
               view.distance += view.distance * MouseZoomDistanceMultiply;
           if event.wheel.y > 0 then
               view.distance -= view.distance * MouseZoomDistanceMultiply;
       end;

       SDL_MOUSEMOTION: begin
           if mouse.drag then begin
               if not mouse.translate then begin
                   if event.motion.y <> mouse.last_y then begin
                       view.pitch += PitchIncrement * event.motion.yrel;
                       mouse.last_y := event.motion.y;
                   end;
                   if event.motion.x <> mouse.last_x then begin
                       view.rotation_angle += RotationAngleIncrement * event.motion.xrel;
                       mouse.last_x := event.motion.x;
                   end;
               end else begin
                   if event.motion.y <> mouse.last_y then begin
                       view.y -= MouseTranslateMultiply * event.motion.yrel;
                       mouse.last_y := event.motion.y;
                   end;
                   if event.motion.x <> mouse.last_x then begin
                       view.x += MouseTranslateMultiply * event.motion.xrel;
                       mouse.last_x := event.motion.x;
                   end;
               end;
           end;
       end;
   end; {case}
end;

//we only care about HOB and HMT files
procedure LoadLevelFilelist;
  procedure AddFile(const map, level: PRsDatFileNode);
  var
    item: TLevelListItem;
    fnode: PRsDatFileNode;
  begin
    item.name := map^.Name;  //lv_0
    for fnode in map^.nodes do
        if fnode^.name = 'hmp' then
            item.hmp := fnode;
    for fnode in level^.nodes do begin
        if fnode^.name = item.name + '_TEX' then  //lv_0_TEX
            item.texture_index := fnode;
        if fnode^.name = item.name + '_TEXT' then //lv_0_TEXT
            item.texture := fnode;
    end;
    g_levels.PushBack(item);
  end;
var
  fnode: TRsDatFileNode;
  data_dir, level_dir, map_dir: PRsDatFileNode;
begin
  //go to data/level/
  for fnode in g_rs_files do begin
      if fnode.is_directory and (fnode.Name = 'data') then begin
          data_dir := @fnode;
          break;
      end;
  end;
  Assert(data_dir <> nil);
  for level_dir in data_dir^.nodes do begin
      if level_dir^.is_directory and (level_dir^.Name = 'level') then
          break;
  end;
  Assert(level_dir <> nil);

  for map_dir in level_dir^.nodes do
      if map_dir^.is_directory then
          AddFile(map_dir, level_dir);
end;

//******************************************************************************
var
  rsdata: TRSDatFile;
  sec, frames: integer;
  event: TSDL_Event;
  done: boolean;

begin
  if not (FileExists(RS_DATA_HDR) and FileExists(RS_DATA_DAT)) then begin
      writeln('RS data files not found!');
      exit;
  end;

  writeln('loading data');
  rsdata := TRSDatFile.Create(RS_DATA_HDR, RS_DATA_DAT);
  rsdata.Parse();
  g_rs_files := rsdata.GetStructure();
  g_levels := TLevelList.Create;
  LoadLevelFilelist;

  terrain := TTerrainMesh.Create;
  terrain.Load(g_levels[g_selected_level_idx]);

  writeln('Init SDL...');
  SDL_Init(SDL_INIT_VIDEO or SDL_INIT_TIMER);
  WindowInit(SCR_W_INIT, SCR_H_INIT);
  writeln('Init OpenGL...');
  InitGL;
  SetGLWindowSize(g_window^.w, g_window^.h);

  InitView;
  terrain.InitGL;

  sec := SDL_GetTicks;
  frames := 0;
  Done := False;
  while not Done do begin
      BeginScene;
      DrawTerrain;
      DrawGui;
      EndScene;

      while SDL_PollEvent(@event) > 0 do
          HandleEvent(event, done);
      frames += 1;
      if (SDL_GetTicks - sec) >= 1000 then begin
          write(frames:3, ' dist: ', view.distance:5:1, ' rot: ', view.rotation_angle:5:1, #13);
          frames := 0;
          sec := SDL_GetTicks;
      end;
      SDL_Delay(10);
  end;

  terrain.Free;
  SDL_Quit;
  g_levels.Free;
  g_rs_files.Free;
  rsdata.Free;
end.

