rerogue
=======

Tools to extract data from Star Wars: Rogue Squadron 3D.
Unpackers:
* DAT repacker - unpacks DAT archive or packs files from folder structure to DAT archive.

Parsers, exporters:
* Hmp2obj - creates wavefront OBJ files from .hmp and corresponding .tex + .text files. 
* Image exporter - exports some images to pnm/pgm/tga files (according to their internal format).
* HOB parser - parses mesh data files.
* HMT parser - parses material data files and exports stored textures.
* HMT compiler - builds custom material data files.

Viewers:
* HOB viewer - utilizes HOB & HMT parsers to view 3d objects used in game. Uses OpenGL and SDL for display & input handling.
* Terrain viewer - displays any .hmp heightmap level as points. Uses OpenGL and SDL for display & input handling.

Compilation
-----------

Use recent Lazarus (1.2.x) with Freepascal (2.6.x) to compile.
Viewers need SDL 1.2 and OpenGL 1.x support to work. I tested 32bit versions only, 64bit will most probably work as well.

TODO
-----------

* hmt parser: decode all image subtypes
* hmt compiler: needs some usable interface
* hob parser: parse more header fields
* bundle repack: extract & compile bundle.00x archives
* terrain viewer: use tiling and texturing
