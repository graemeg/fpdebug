unit dbgInfoFPCTypes;

interface

uses
  dbgTypes, dbgInfoTypes;

type
  { TDbgSymFPCDynArray }

  TDbgSymFPCDynArray = class(TDbgSymType)
    function GetVarSize: LongWord; override;
    function isRefType: Boolean; override;
    function DerefOfs: PtrInt; override;
  end;

type
  TFPCDynArray = record  // see dynarray declaration
    RefCount  : PtrInt;
    High      : SizeInt;
  end;

implementation

function TDbgSymFPCDynArray.GetVarSize:LongWord;
begin
  Result:=sizeof(TFPCDynArray);
end;

{ TDbgSymFPCDynArray }

function TDbgSymFPCDynArray.isRefType:Boolean;
begin
  Result:=True;
end;

function TDbgSymFPCDynArray.DerefOfs:PtrInt;
begin
  Result:=-sizeof(TFPCDynArray);
end;

end.
