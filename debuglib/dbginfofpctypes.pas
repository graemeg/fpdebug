{
    fpDebug  -  A debugger for the Free Pascal Compiler.

    Copyright (c) 2012 by Graeme Geldenhuys.

    See the file LICENSE.txt, included in this distribution,
    for details about redistributing fpDebug.

    Description:
      .
}
unit dbgInfoFPCTypes;

interface

{$mode objfpc}{$h+}

uses
  dbgTypes, dbgInfoTypes;

type
  { TDbgSymFPCDynArray }

  TDbgSymFPCDynArray = class(TDbgSymType)
    function GetVarSize: LongWord; override;
    function isRefType: Boolean; override;
    function DerefOfs: PtrInt; override;
    function isIndexAccess: Boolean; override;
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

function TDbgSymFPCDynArray.isIndexAccess:Boolean;
begin
  Result:=True;
end;

end.
