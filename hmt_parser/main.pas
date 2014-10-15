program parse_hmt;
{$mode objfpc}{$H+}

uses
  hmt_parser;

var
  fname: string;

begin
  if ParamCount < 1 then begin
      writeln ('no input file specified');
      exit;
  end;

  fname := ParamStr(1);
  writeln('parsing file: ', fname);
  try
      ParseHmtFile(fname);
  except
      writeln('parsing failed!');
  end;
  writeln('done.');
end.

