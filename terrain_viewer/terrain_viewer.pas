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
  gl, glu, glext, sdl,
  terrain_mesh;

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
  terrain: TTerrainMesh;

  view: record
      rotation_angle: single;
      distance: single;
      pitch: single;
      x, y: single;
      autorotate: boolean;
      opts: TRenderOpts;
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

  terrain.DrawGL(view.opts);
  
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
  view.autorotate := false;
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
             SDLK_F1: begin
                     if not fullscreen then begin
                         SetMode(SCR_W_fscrn, SCR_H_fscrn, true);
                         fullscreen := true;
                     end else begin
                         SetMode(SCR_W_INIT, SCR_H_INIT, false);
                         fullscreen := false;
                     end;
                     InitGL;
                     terrain.InitGL;
                     ResizeWindow( surface^.w, surface^.h );
                 end;
             SDLK_s:
                 WindowScreenshot( surface^.w, surface^.h );
             SDLK_PAGEUP:
                 view.distance += ZoomIncrement;
             SDLK_PAGEDOWN:
                 view.distance -= ZoomIncrement;
             SDLK_r:
                 view.autorotate := not view.autorotate;

             //terrain rendering opts
             SDLK_w:
                 view.opts.wireframe := not view.opts.wireframe;
             SDLK_v:
                 view.opts.vcolors := not view.opts.vcolors;
             SDLK_p:
                 view.opts.points := not view.opts.points;
             SDLK_t:
                 view.opts.textures := not view.opts.textures;
             SDLK_LEFT:
                 view.opts.fg_to_draw := max(0, view.opts.fg_to_draw - 1);
             SDLK_RIGHT:
                 view.opts.fg_to_draw += 1;
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
  in_file: integer;

begin
  if Paramcount < 1 then begin
      writeln('specify HMP file index');
      exit;
  end;
  in_file := StrToInt( ParamStr(1) );
  terrain := TTerrainMesh.Create;
  terrain.Load(14);

  writeln('Init SDL...');
  SDL_Init( SDL_INIT_VIDEO );
  SDL_GL_SetAttribute( SDL_GL_DOUBLEBUFFER, 1 );
  
  SetMode(SCR_W_INIT, SCR_H_INIT);
  writeln('Init OpenGL...');
  InitGL;
  ResizeWindow( surface^.w, surface^.h );

  InitView;
  terrain.InitGL;

  sec := SDL_GetTicks;
  frames := 0;
  Done := False;
  while not Done do begin
      HandleEvent;
      DrawGLScene;
      frames += 1;
      if (SDL_GetTicks - sec) >= 1000 then begin
          write(frames:3, ' dist: ', view.distance:5:1, ' rot: ', view.rotation_angle:5:1, #13);
          frames := 0;
          sec := SDL_GetTicks;

          //WindowScreenshot( surface^.w, surface^.h );
      end;
      SDL_Delay(10);
  end;

  terrain.Free;
  SDL_Quit;
end.

