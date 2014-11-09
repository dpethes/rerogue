rerogue
=======

Tools to extract data from Star Wars: Rogue Squadron 3D.

* DAT repacker - unpacks DAT archive or packs files from folder structure to DAT archive.
* Hmp2obj - creates wavefront OBJ files from .hmp and corresponding .tex + .text files. 
* Image exporter - exports some images to pnm/pgm/tga files (according to their internal format).
* HOB parser - parses mesh data files.
* HMT parser - parses material data files and exports stored textures.
* HMT compiler - builds custom material data files.
* HOB display - utilizes HOB & HMT parsers to view 3d objects used in game. Uses OpenGL and SDL for display & input handling.

Compilation
-----------

Use recent Lazarus (1.2.x) with Freepascal (2.6.x) to compile.
HOB display needs SDL 1.2 and OpenGL 1.x support to work.

TODO
-----------

* hmt parser: decode all image subtypes
* hmt compiler: needs some usable interface
* hob parser: parse more header fields
* mesh viewer: reuse hmt & hob parsers to display data
* bundle repack: extract & compile bundle.00x archives
* terrain viewer
