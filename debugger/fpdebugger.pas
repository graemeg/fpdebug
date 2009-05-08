program fpdebugger;

{$mode objfpc}{$H+}

uses
  dbgTypes
  {$ifdef windows}
  ,winDbgTypes
  {$endif}, nixDbgTypes, nixPtrace;

begin
  writeln('hello world');
end.

