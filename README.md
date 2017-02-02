rerogue
=======

Tools to extract and view data from Star Wars: Rogue Squadron 3D.
Unpackers:
* DAT repacker - unpacks DAT archive or packs files from folder structure to DAT archive.
* Bundle pack - unpacks BUNDLE.00x files

Parsers, exporters:
* Hmp2obj - creates wavefront OBJ files from .hmp and corresponding .tex + .text files. 
* Image exporter - exports some images to pnm/pgm/tga files (according to their internal format).
* HOB parser - parses mesh data files.
* HMT parser - parses material data files and exports stored textures.
* HMT compiler - builds custom material data files.

Viewers:
* Model viewer - utilizes HOB & HMT parsers to view 3d objects used in game. 
* Terrain viewer - displays any .hmp heightmap level as points. Uses OpenGL and SDL for display & input handling.

Compilation
-----------

Use recent Lazarus (1.6 and higher) with Freepascal (3.0 and higher) to compile. 
Older versions may work, but are not recommended.
Terrain viewer needs SDL 1.2 and OpenGL 1.x support to work. I tested 32bit versions only, 64bit will most probably work as well.
Model viewer needs OpenGL2, SDL2 and ImGui (through cimgui). Only 64bit is tested.

Bundle pack uses Rust, so (try to) compile with Cargo.

TODO
-----------

* hmt parser: decode all image subtypes
* hmt compiler: needs some usable interface
* hob parser: parse more header fields
* bundle repack: pack bundle.00x archives

