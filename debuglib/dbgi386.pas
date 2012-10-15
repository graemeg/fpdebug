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
    
    function ExecRegName: AnsiString; override;
    function FrameRegName: AnsiString; override;
    function StackRegName: AnsiString; override;
  end;

  { TCPUx64 }

  TCPUx64 = class(TCPUi386)
    function ExecRegName: AnsiString; override;
    function FrameRegName: AnsiString; override;
    function StackRegName: AnsiString; override;
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

function TCPUi386.ExecRegName:AnsiString;
begin
  Result:=_Eip;
end;

function TCPUi386.FrameRegName:AnsiString;
begin
  Result:=_Ebp;
end;

function TCPUi386.StackRegName:AnsiString;
begin
  Result:=_Esp;
end;

{ TCPUx64 }

function TCPUx64.ExecRegName:AnsiString;
begin
  Result:=_rip;
end;

function TCPUx64.FrameRegName:AnsiString;
begin
  Result:=_rbp;
end;

function TCPUx64.StackRegName:AnsiString;
begin
 Result:=_rsp;
end;

initialization
  {$ifdef CPUx86_64}
  InstallCPU(TCPUx64.Create);
  {$else}
  InstallCPU(TCPUi386.Create);
  {$endif}

end.

