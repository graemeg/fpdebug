{
    fpDebug  -  A debugger for the Free Pascal Compiler.

    Copyright (c) 2012 by Graeme Geldenhuys.

    See the file LICENSE.txt, included in this distribution,
    for details about redistributing fpDebug.

    Description:
      .
}
unit dbgInfoUtils;

interface

uses
  dbgTypes, dbgCPU, dbgUtils, dbgInfoTypes, dbgMain;

function GetVarAddr(AVar: TDbgSymVar; Thread: TDbgThread; ACPU: TCPUCode=nil): TDbgPtr;

implementation

function GetVarAddr(AVar: TDbgSymVar; Thread: TDbgThread; ACPU: TCPUCode): TDbgPtr;
var
  list: TDbgDataBytesList;
begin
  Result:=0;
  if not Assigned(ACPU) then ACPU:=CPUCode;
  if AVar.DataPos.Location=ddlAbsolute then
    Result:=AVar.DataPos.Addr
  else begin
    list:=TDbgDataBytesList.Create;
    Thread.GetThreadRegs(list);
    if AVar.DataPos.Location=ddlFrameRel then
      Result:=list[CPUCode.FrameRegName].DbgPtr+AVar.DataPos.Addr;
    list.Free;
  end;
end;

end.
