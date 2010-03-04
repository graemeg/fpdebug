unit dbgi386; 

{$mode objfpc}{$H+}
//todo: cleanup ifdefs

interface

uses
  dbgTypes, dbgCPU, dbgConsts; 
  
type
  
  { TCPUi386 }

  TCPUi386 = class(TCPUCode)
    function BreakPointSize: Integer; override;
    procedure WriteBreakPoint(var Buffer: array of byte; Offset: Integer); override;
    function IsBreakPoint(const Buffer: array of byte; Offset: Integer): Boolean; override;
    
    function ExecuteRegisterName: AnsiString; override;
  end;

  { TCPUx64 }

  TCPUx64 = class(TCPUi386)
    function ExecuteRegisterName: AnsiString; override;
  end;

implementation

const
  Int3 = $CC;

function TCPUi386.BreakPointSize: Integer; 
begin
  Result := 1;
end;

procedure TCPUi386.WriteBreakPoint(var Buffer: array of byte; Offset: Integer); 
begin
  Buffer[Offset] := Int3;
end;

function TCPUi386.IsBreakPoint(const Buffer: array of byte; Offset: Integer): Boolean;  
begin
  Result := Buffer[Offset] = Int3;
end;

function TCPUi386.ExecuteRegisterName: AnsiString;  
begin
  Result:=_Eip;  
end;


{ TCPUx64 }

function TCPUx64.ExecuteRegisterName: AnsiString;
begin
  Result:=_rip;
end;

initialization
  {$ifdef CPUx86_64}
  InstallCPU(TCPUx64.Create);
  {$else}
  InstallCPU(TCPUi386.Create);
  {$endif}

end.

