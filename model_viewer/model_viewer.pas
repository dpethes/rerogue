(*
Copyright (c) 2017 David Pethes

This file is part of RS model viewer.

RS model viewer is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

RS model viewer is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with RS model viewer. If not, see <http://www.gnu.org/licenses/>.
*)
program model_viewer;

uses
  sysutils, classes, math, strutils, gvector,
  gl, glu, glext, sdl2, fpimgui, fpimgui_impl_sdlgl2,
  rs_dat, hob_mesh,
  png_writer, dc2core;

const
  SCR_W_INIT = 1280;
  SCR_H_INIT = 720;
  RotationAngleIncrement = 1;
  ZoomIncrement = 0.3;
  MouseZoomDistanceMultiply = 0.15;
  PitchIncrement = 0.5;
  MouseTranslateMultiply = 0.025;

type
  TFileListItem = record
      name: string;
      node_hob: PRsDatFileNode;
      node_hmt: PRsDatFileNode;
  end;
  TFileList = specialize TVector<TFileListItem>;

var
  g_window: PSDL_Window;
  g_ogl_context: TSDL_GLContext;

  g_rs_files: TRsDatFileNodeList;
  g_filelist: TFileList;
  g_selected_file_idx: integer;
  g_model: TModel;
  g_model_loading_failed: Boolean;

  view: record
      rotation_angle: single;
      distance: single;
      pitch: single;
      x, y: single;
      autorotate: boolean;
      render: TRenderOpts;
      mesh: TMeshOpts;
  end;

  key_pressed: record
      wireframe: boolean;
      vcolors: boolean;
      points: boolean;
      textures: boolean;
      fullscreen: boolean;
      autorotate: boolean;
      fg: boolean;
  end;

  mouse: record
      drag: boolean;
      translate: boolean;
      last_x, last_y: integer;
      resume_autorotate_on_release: boolean;
  end;

procedure AppError(s: string);
begin
  writeln(stderr, s);
  halt;
end;


// initial parameters
procedure InitGL;
var
  ogl_info: string;
begin
  ogl_info := format('vendor: %s renderer: %s', [glGetString(GL_VENDOR), glGetString(GL_RENDERER)]);
  writeln(ogl_info);
  ogl_info := 'version: ' + glGetString(GL_VERSION);
  writeln(ogl_info);

  //glShadeModel( GL_SMOOTH );                  // Enable smooth shading
  glClearColor( 0.0, 0.0, 0.0, 0.0 );
  glClearDepth( 1.0 );                        // Depth buffer setup
  glEnable( GL_DEPTH_TEST );                  // Enables Depth Testing
  glDepthFunc( GL_LEQUAL );                   // The Type Of Depth Test To Do
  glHint( GL_PERSPECTIVE_CORRECTION_HINT, GL_NICEST );  // Really Nice Perspective Calculations

  //glEnable( GL_CULL_FACE );  //backface culling
  //glCullFace( GL_BACK );

  glEnable(GL_TEXTURE_2D);
end;


// function to reset our viewport after a window resize
procedure SetGLWindowSize( width, height : integer );
begin
  if ( height = 0 ) then
    height := 1;   // Protect against a divide by zero

  glViewport( 0, 0, width, height ); // Setup our viewport.
  glMatrixMode( GL_PROJECTION );     // change to the projection matrix and set our viewing volume.
  glLoadIdentity;
  gluPerspective(45.0, width / height, 0.1, 100.0);  // Set our perspective
 //   glOrtho( 0, width, height, 0, - 1, 1);
  glMatrixMode( GL_MODELVIEW );  // Make sure we're changing the model view and not the projection
  glLoadIdentity;                // Reset The View
end;


// The main drawing function.
procedure DrawModel;
begin
  glLoadIdentity;

  if view.distance < ZoomIncrement then
      view.distance := ZoomIncrement;

  glTranslatef(view.x, view.y, -view.distance);
  glRotatef(view.rotation_angle, 0.0, 1.0, 0.0);
  glRotatef(view.pitch, 1, 0, 0);

  if view.autorotate then
      view.rotation_angle += RotationAngleIncrement;
  if view.rotation_angle > 360 then
      view.rotation_angle -= 360;

  if g_model <> nil then begin
      g_model.DrawGL(view.render);
  end;
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
  //WriteVideoInfo;

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
  g_window := SDL_CreateWindow('RS model viewer', x, y, w_width, w_height, flags);
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
  view.distance := 3;
  view.pitch := 0;
  view.x := 0;
  view.y := 0;
  view.autorotate := true;

  view.render.fg_all := true;
  view.render.fg_to_draw := 0;
  view.render.obj_to_draw := 0;
  view.render.wireframe := false;
  view.render.points := false;
  view.render.vcolors := true;
  view.render.textures := true;
end;


//Beware, not all models can be parsed correctly yet. This will leak then, but we don't care much.
procedure LoadMesh(item: TFileListItem);
var
  hob, hmt: TMemoryStream;
begin
  try
    if g_model <> nil then
        g_model.Free;
    hob := TMemoryStream.Create;
    hob.WriteBuffer(item.node_hob^.Data^, item.node_hob^.size);
    hob.Seek(0, soBeginning);

    hmt := TMemoryStream.Create;
    hmt.WriteBuffer(item.node_hmt^.Data^, item.node_hmt^.size);
    hmt.Seek(0, soBeginning);

    g_model := TModel.Create;
    g_model.Load(hob, hmt);
    g_model.InitGL;

    hob.Free;
    hmt.Free;
    g_model_loading_failed := false;
  except
    g_model_loading_failed := true;
  end;
  view.render.fg_to_draw := 0;
end;


procedure HandleEvent(const ev: TSDL_Event; var done: boolean);
var
  io: PImGuiIO;
begin
  ImGui_ImplSdlGL2_ProcessEvent(@ev);
  io := igGetIO();
  if ((ev.type_ = SDL_MOUSEBUTTONDOWN) or
     (ev.type_ = SDL_MOUSEBUTTONUP) or
     (ev.type_ = SDL_MOUSEWHEEL) or
     (ev.type_ = SDL_MOUSEMOTION)) and io^.WantCaptureMouse then
      exit;
  if ((ev.type_ = SDL_KEYDOWN) or (ev.type_ = SDL_KEYUP)) and io^.WantCaptureKeyboard then
      exit;

  case ev.type_ of
      SDL_QUITEV:
          Done := true;
      SDL_WINDOWEVENT: begin
          if ev.window.event = SDL_WINDOWEVENT_RESIZED then
              SetGLWindowSize(ev.window.data1, ev.window.data2);
      end;

      SDL_KEYDOWN: begin
          case ev.key.keysym.sym of
            SDLK_ESCAPE:
                Done := true;
            SDLK_s:
                WindowScreenshot(g_window^.w, g_window^.h);
            SDLK_PAGEUP:
                view.distance += ZoomIncrement;
            SDLK_PAGEDOWN:
                view.distance -= ZoomIncrement;
            SDLK_r:
                if not key_pressed.autorotate then begin
                    view.autorotate := not view.autorotate;
                    key_pressed.autorotate := true;
                    if not view.autorotate then
                        view.rotation_angle := 0;
                end;
                   //g_model rendering opts
            SDLK_w:
                if not key_pressed.wireframe then begin
                    view.render.wireframe := not view.render.wireframe;
                    key_pressed.wireframe := true;
                end;
            SDLK_v:
                if not key_pressed.vcolors then begin
                    view.render.vcolors := not view.render.vcolors;
                    key_pressed.vcolors := true;
                end;
            SDLK_p:
                if not key_pressed.points then begin
                    view.render.points := not view.render.points;
                    key_pressed.points := true;
                end;
            SDLK_t:
                if not key_pressed.textures then begin
                    view.render.textures := not view.render.textures;
                    key_pressed.textures := true;
                end;
            SDLK_f:
                view.render.fg_all := not view.render.fg_all;
            SDLK_LEFT:
                view.render.fg_to_draw := max(0, view.render.fg_to_draw - 1);
            SDLK_RIGHT:
                view.render.fg_to_draw += 1;
            SDLK_END:
                view.render.obj_to_draw += 1;
            SDLK_HOME:
                view.render.obj_to_draw := max(0, view.render.obj_to_draw - 1);

            SDLK_UP:
                if (g_selected_file_idx > 0) then begin
                    g_selected_file_idx -= 1;
                    view.render.fg_to_draw := 0;
                    view.render.obj_to_draw := 0;
                    LoadMesh(g_filelist[g_selected_file_idx]);
                end;
            SDLK_DOWN:
                if (g_selected_file_idx < g_filelist.Size - 1) then begin
                    g_selected_file_idx += 1;
                    view.render.fg_to_draw := 0;
                    view.render.obj_to_draw := 0;
                    LoadMesh(g_filelist[g_selected_file_idx]);
                end;
          end;
      end;
      SDL_KEYUP: begin
          case ev.key.keysym.sym of
            SDLK_F1:
                key_pressed.fullscreen := false;
            SDLK_w:
                key_pressed.wireframe := false;
            SDLK_v:
                key_pressed.vcolors := false;
            SDLK_p:
                key_pressed.points := false;
            SDLK_t:
                key_pressed.textures := false;
            SDLK_r:
                key_pressed.autorotate := false;
          end;
      end;

      SDL_MOUSEBUTTONDOWN: begin
          mouse.resume_autorotate_on_release := view.autorotate;
          if ev.button.button in [1..3] then begin
              mouse.drag := true;
              mouse.translate := ev.button.button = 2;
              mouse.last_x := ev.button.x;
              mouse.last_y := ev.button.y;
              view.autorotate := false;
          end;
      end;
      SDL_MOUSEBUTTONUP: begin
          mouse.drag := false;
          view.autorotate := mouse.resume_autorotate_on_release;
      end;
      SDL_MOUSEWHEEL: begin
          if ev.wheel.y < 0 then
              view.distance += view.distance * MouseZoomDistanceMultiply;
          if ev.wheel.y > 0 then
              view.distance -= view.distance * MouseZoomDistanceMultiply;
      end;
      SDL_MOUSEMOTION: begin
          if mouse.drag then begin
              if not mouse.translate then begin
                  if ev.motion.y <> mouse.last_y then begin
                      view.pitch += PitchIncrement * ev.motion.yrel;
                      mouse.last_y := ev.motion.y;
                  end;
                  if ev.motion.x <> mouse.last_x then begin
                      view.rotation_angle += RotationAngleIncrement * ev.motion.xrel;
                      mouse.last_x := ev.motion.x;
                  end;
              end else begin
                  if ev.motion.y <> mouse.last_y then begin
                      view.y -= MouseTranslateMultiply * ev.motion.yrel;
                      mouse.last_y := ev.motion.y;
                  end;
                  if ev.motion.x <> mouse.last_x then begin
                      view.x += MouseTranslateMultiply * ev.motion.xrel;
                      mouse.last_x := ev.motion.x;
                  end;
              end;
          end;
      end;
  end; {case}
end;


procedure DrawGui;
var
  file_item: TFileListItem;
  selected_item_idx: integer;
  selected_item: TFileListItem;
  i: Integer;
  do_export: boolean;
begin
  if g_selected_file_idx >= 0 then begin
      Imgui.Begin_('Mesh');  //window used in hob_mesh as well
      if not g_model_loading_failed then begin
          Imgui.Text(g_filelist[g_selected_file_idx].name);
      end else
          Imgui.Text('mesh loading failed :(');
      if g_model <> nil then begin
          do_export := Imgui.Button('Export to obj');
          Imgui.SameLine(0, 15);
          Imgui.Checkbox('with png textures', @view.mesh.export_png_textures);
          if do_export then
              g_model.ExportObj('rs_exported.obj', view.mesh.export_png_textures);
      end;
      Imgui.End_;
  end;

  Imgui.Begin_('Rendering options');
    Imgui.Checkbox('points', @view.render.points);
    Imgui.Checkbox('wireframe', @view.render.wireframe);
    Imgui.Checkbox('textures', @view.render.textures);
    Imgui.Checkbox('vertex colors', @view.render.vcolors);
  Imgui.End_;

  if g_filelist.Size = 0 then
      exit;

  selected_item_idx := -1;
  //todo scrolling, filter?
  Imgui.Begin_('File list');
    for i := 0 to g_filelist.Size - 1 do begin
        file_item := g_filelist[i];
        if Imgui.Selectable(file_item.name, g_selected_file_idx = i) then begin
            selected_item_idx := i;
            selected_item := file_item;
        end;
    end;
  Imgui.End_;

  if (selected_item_idx >= 0) and (selected_item_idx <> g_selected_file_idx) then begin
      g_selected_file_idx := selected_item_idx;
      LoadMesh(selected_item);
  end;
end;

//we only care about HOB and HMT files
procedure LoadMeshFilelist;
  procedure AddFile(const path: string; const fnode, fnode_next: PRsDatFileNode);
  var
    item: TFileListItem;
    name: String;
    i: integer;
  begin
    if fnode^.is_directory then begin
        for i := 0 to Length(fnode^.nodes)-2 do  //hob/hmt always go in pairs
            AddFile(path + fnode^.Name + '/', fnode^.nodes[i], fnode^.nodes[i+1]);
    end
    else begin
        name := fnode^.name;
        if AnsiEndsText('hob', name) then begin
            Assert(AnsiEndsText('hmt', fnode_next^.name), 'no HMT file!');
            item.name := path + name;
            item.node_hob := fnode;
            item.node_hmt := fnode_next;
            g_filelist.PushBack(item);
        end;
    end;
  end;
var
  file_: TRsDatFileNode;
begin
  for file_ in g_rs_files do begin
      AddFile('', @file_, nil);
  end;
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
  igRender;
  SDL_GL_SwapWindow(g_window);
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
  g_filelist := TFileList.Create;
  LoadMeshFilelist();

  writeln('Init SDL...');
  SDL_Init(SDL_INIT_VIDEO or SDL_INIT_TIMER);
  WindowInit(SCR_W_INIT, SCR_H_INIT);
  writeln('Init OpenGL...');
  InitGL;
  SetGLWindowSize(g_window^.w, g_window^.h);

  InitView;
  g_model := nil;
  g_selected_file_idx := 0;
  LoadMesh(g_filelist[0]);

  sec := SDL_GetTicks;
  frames := 0;
  Done := False;
  key_pressed.wireframe := false;
  key_pressed.fullscreen := false;
  while not Done do begin
      BeginScene;
      DrawModel;
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
      SDL_Delay(5);
      //WindowScreenshot( surface^.w, surface^.h );
  end;

  WindowFree;
  SDL_Quit;

  if g_model <> nil then
      g_model.Free;
  g_filelist.Free;
  g_rs_files.Free;
  rsdata.Free;
end.

