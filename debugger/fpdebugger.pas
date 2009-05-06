program fpdebugger;

{$mode objfpc}{$H+}

uses
  dbgTypes
  {$ifdef windows}
  ,winDbgTypes
  {$endif};

begin
  writeln('hello world');
end.

