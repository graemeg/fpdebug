unit dbgCPU; 

{$mode objfpc}{$H+}

interface

uses
  dbgTypes; 
  
type 
  { TCPUCode }
  TCPUCode = class(TObject)
    function BreakPointSize: Integer; virtual; 
    procedure SetBreakPoint(var Buffer: array of byte; Offset: Integer); virtual; 
    function IsBreakPoint(const Buffer: array of byte; Offset: Integer): Boolean; virtual;
    procedure BreakPointAddress(const ExeRegValue : TDbgPtr; out BreakAddr: TDbgPtr); virtual;
    
    function ExecuteRegisterName: AnsiString; virtual;
  end;  

function CPUCode: TCPUCode; inline;
procedure InstallCPU(ACPUCode: TCPUCode);
  
implementation

var 
  fCPUCode : TCPUCode = nil;

function CPUCode: TCPUCode;  
begin
  Result := fCPUCode;
end;

procedure InstallCPU(ACPUCode: TCPUCode);
begin
  if Assigned(fCPUCode) then fCPUCode.Free;
  fCPUCode := ACPUCode;
end;

{ TCPUCode }

function TCPUCode.BreakPointSize: Integer; 
begin
  Result := 0;
end;

procedure TCPUCode.SetBreakPoint(var Buffer: array of byte; Offset: Integer); 
begin

end;

function TCPUCode.IsBreakPoint(const Buffer: array of byte; Offset: Integer): Boolean; 
begin
  Result := false;
end;

procedure TCPUCode.BreakPointAddress(const ExeRegValue: TDbgPtr; out BreakAddr: TDbgPtr); 
begin
  BreakAddr := 0;
end;

function TCPUCode.ExecuteRegisterName: AnsiString; 
begin
  Result := '';
end;

initialization
  fCPUCode := TCPUCode.Create;

finalization
  fCPUCode.Free;

end.

