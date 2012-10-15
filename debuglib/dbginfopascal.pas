{
    fpDebug  -  A debugger for the Free Pascal Compiler.

    Copyright (c) 2012 by Graeme Geldenhuys.

    See the file LICENSE.txt, included in this distribution,
    for details about redistributing fpDebug.

    Description:
      .
}
unit dbgInfoPascal;

interface

uses
  dbgInfoTypes;

// RTL pascal functions... if any available?!
procedure InitRTLSymbols(info: TDbgInfo);

// Strings, Files, dynamic arrays, objects and classes
procedure InitStdTypes(info: TDbgInfo);

implementation

procedure InitRTLSymbols(info: TDbgInfo);
begin
  //todo:
end;

procedure InitStdTypes(info: TDbgInfo);
begin
  //todo:
end;

end.
