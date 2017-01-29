(*
hob_viewer.pas
Copyright (c) 2014 David Pethes

This file is part of HOB viewer.

HOB viewer is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

HOB viewer is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with HOB viewer.  If not, see <http://www.gnu.org/licenses/>.
*)
program hob_viewer;

uses
  sysutils,
  gl, glu, glext, sdl,
  hob_mesh, GenericStructList, hob_parser, hmt_parser, rs_image;

const
  SCR_W_fscrn = 1024;
  SCR_H_fscrn = 768;
  SCR_W_INIT = 1280;
  SCR_H_INIT = 720;
  SCREEN_BPP = 0;
  RotationAngleIncrement = 1;
  ZoomIncrement = 0.3;
  MouseZoomDistanceMultiply = 0.15;
  PitchIncrement = 0.5;
  MouseTranslateMultiply = 0.025;

var
  surface: PSDL_Surface;
  done,
  fullscreen: boolean;
  model: TModel;

  view: record
      rotation_angle: single;
      distance: single;
      pitch: single;
      x, y: single;
      autorotate: boolean;
      opts: TRenderOpts;
  end;

  key_pressed: record
      wireframe: boolean;
      vcolors: boolean;
      points: boolean;
      textures: boolean;
      fullscreen: boolean;
      autorotate: boolean;
  end;

  mouse: record
      drag: boolean;
      translate: boolean;
      last_x, last_y: integer;
      resume_autorotate_on_release: boolean;
  end;


procedure ReportError(s: string);
begin
  writeln(s);
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
procedure ResizeWindow( width, height : integer );
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


// The main drawing function.
procedure DrawGLScene;
begin
  glClear( GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT );
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

  model.DrawGL(view.opts);
  
  SDL_GL_SwapBuffers;
end;


procedure SetMode(w, h: word; fullscreen: boolean = false);
var
  flags: UInt32;
begin
  if fullscreen then
      flags := SDL_OPENGL or SDL_FULLSCREEN
  else
      flags := SDL_OPENGL or SDL_RESIZABLE;
  surface := SDL_SetVideoMode( w, h, SCREEN_BPP, flags);
  if surface = nil then
      ReportError('SDL_SetVideoMode failed');
  SDL_WM_SetCaption('HOB viewer', nil);
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
  view.distance := 6;
  view.pitch := 0;
  view.x := 0;
  view.y := 0;
  view.autorotate := true;
  view.opts.wireframe := false;
  view.opts.points := false;
  view.opts.vcolors := true;
  view.opts.textures := true;
end;


procedure HandleEvent;
var
 event: TSDL_Event;
begin
  SDL_PollEvent( @event );
  case event.type_ of
       SDL_QUITEV:
           Done := true;

       SDL_VIDEORESIZE:
           begin
             SetMode (event.resize.w, event.resize.h);
             ResizeWindow( surface^.w, surface^.h );
           end;

       SDL_KEYDOWN:
           case event.key.keysym.sym of
             SDLK_ESCAPE:
                 Done := true;
             SDLK_F1:
                 if not key_pressed.fullscreen then begin
                     if not fullscreen then begin
                         SetMode(SCR_W_fscrn, SCR_H_fscrn, true);
                         fullscreen := true;
                     end else begin
                         SetMode(SCR_W_INIT, SCR_H_INIT, false);
                         fullscreen := false;
                     end;
                     InitGL;
                     ResizeWindow( surface^.w, surface^.h );
                     key_pressed.fullscreen := true;
                 end;
             SDLK_s:
                 WindowScreenshot( surface^.w, surface^.h );
             SDLK_PAGEUP:
                 view.distance += ZoomIncrement;
             SDLK_PAGEDOWN:
                 view.distance -= ZoomIncrement;
             SDLK_r:
                 if not key_pressed.autorotate then begin
                     view.autorotate := not view.autorotate;
                     key_pressed.autorotate := true;
                 end;

             //model rendering opts
             SDLK_w:
                 if not key_pressed.wireframe then begin
                     view.opts.wireframe := not view.opts.wireframe;
                     key_pressed.wireframe := true;
                 end;
             SDLK_v:
                 if not key_pressed.vcolors then begin
                     view.opts.vcolors := not view.opts.vcolors;
                     key_pressed.vcolors := true;
                 end;
             SDLK_p:
                 if not key_pressed.points then begin
                     view.opts.points := not view.opts.points;
                     key_pressed.points := true;
                 end;
             SDLK_t:
                 if not key_pressed.textures then begin
                     view.opts.textures := not view.opts.textures;
                     key_pressed.textures := true;
                 end;
           end;

       SDL_KEYUP:
           case event.key.keysym.sym of
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

       SDL_MOUSEBUTTONDOWN: begin
           mouse.resume_autorotate_on_release := view.autorotate;
           if event.button.button in [1..3] then begin
               mouse.drag := true;
               mouse.translate := event.button.button in [2];
               mouse.last_x := event.button.x;
               mouse.last_y := event.button.y;
               view.autorotate := false;
           end;
           if event.button.button = 5 then
               view.distance += view.distance * MouseZoomDistanceMultiply;
           if event.button.button = 4 then
               view.distance -= view.distance * MouseZoomDistanceMultiply;
       end;
       SDL_MOUSEBUTTONUP: begin
           mouse.drag := false;
           view.autorotate := mouse.resume_autorotate_on_release;
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

//******************************************************************************
var
  sec, frames: integer;
  hob_file, hmt_file, obj_file: string;

begin
  if Paramcount < 1 then begin
      writeln('specify HOB file');
      exit;
  end;
  hob_file := ParamStr(1);
  hmt_file := StringReplace(hob_file, '.hob', '.hmt', [rfIgnoreCase]);
  model := TModel.Create;
  model.Load(hob_file, hmt_file);

  writeln('Init SDL...');
  SDL_Init( SDL_INIT_VIDEO );
  SDL_GL_SetAttribute( SDL_GL_DOUBLEBUFFER, 1 );
  
  SetMode(SCR_W_INIT, SCR_H_INIT);
  writeln('Init OpenGL...');
  InitGL;
  ResizeWindow( surface^.w, surface^.h );

  InitView;
  model.InitGL;

  //export
  obj_file := StringReplace(hob_file, '.hob', '.obj', [rfIgnoreCase]);
  model.ExportObj(obj_file);

  sec := SDL_GetTicks;
  frames := 0;
  Done := False;
  key_pressed.wireframe := false;
  key_pressed.fullscreen := false;
  while not Done do begin
      HandleEvent;
      DrawGLScene;
      frames += 1;
      if (SDL_GetTicks - sec) >= 1000 then begin
          write(frames:3, ' dist: ', view.distance:5:1, ' rot: ', view.rotation_angle:5:1, #13);
          frames := 0;
          sec := SDL_GetTicks;
      end;
      SDL_Delay(10);
      //WindowScreenshot( surface^.w, surface^.h );
  end;

  model.Free;
  SDL_Quit;
end.

