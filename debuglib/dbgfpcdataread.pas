{
    fpDebug  -  A debugger for the Free Pascal Compiler.

    Copyright (c) 2012 by Graeme Geldenhuys.

    See the file LICENSE.txt, included in this distribution,
    for details about redistributing fpDebug.

    Description:
      .
}
unit dbgFPCDataRead;

interface

uses
  SysUtils,
  dbgTypes, dbgInfoTypes, dbgDataRead, dbgInfoFPCTypes;

type

  { TFPCDynArrayReader }

  TFPCDynArrayReader = class(TDbgTypeRead)
    function Dump(ASymType: TDbgSymType; const Data; DataSize: Integer): AnsiString; override;
  end;

implementation

{ TFPCDynArrayReader }


function TFPCDynArrayReader.Dump(ASymType:TDbgSymType;const Data;DataSize: Integer):AnsiString;
var
  arr : TFPCDynArray;
begin
  if DataSize<sizeof(TFPCDynArray) then begin
    writeln('not enough size!');
    Exit;
  end;
  Move(Data, arr, sizeof(arr));
  Result:=Format('ref count = %d, length = %d', [arr.RefCount, arr.High+1]);
end;

initialization
  RegisterReader( TDbgSymFPCDynArray, TFPCDynArrayReader.Create );

end.
