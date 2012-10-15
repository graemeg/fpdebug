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
