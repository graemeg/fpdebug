unit dbgi386; 

{$mode objfpc}{$H+}

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


initialization
  InstallCPU(TCPUi386.Create);

end.

