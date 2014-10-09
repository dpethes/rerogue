program parse_hob;

uses
  sysutils, hob_parser;

var
  fname: string;
  hob: THobFile;

begin
  if ParamCount < 1 then begin
      writeln ('no input file specified');
      exit;
  end;

  fname := ParamStr(1);
  writeln('parsing file: ', fname);
  hob := ParseHobFile(fname);
  writeln('done.');
end.

