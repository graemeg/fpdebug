unit cmddbg; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, 
  dbgTypes, dbgUtils, dbgInfoTypes, cmdlineutils,
  commands; 

procedure LoadDebugInfo(const FileName: string);
procedure LoadExeDebugInfo(const cmdLine: string);

implementation

var
  DbgSources : TList;
  
type
  TWhereCommand = class(TCommand)
  public
    procedure Execute(CmdParams: TStrings; Process: TDbgProcess); override;
  end;  
  
procedure TWhereCommand.Execute(CmdParams: TStrings; Process: TDbgProcess);
var
  regs : TDbgDataList;
begin
  regs := GetProcessRegisters(Process);
  if not Assigned(regs) then begin
    writeln('Cannot read process state. ');
    Exit;
  end;
  WriteLn('EIP = ', regs['EIP'].SInt32);
end;

procedure InitDebugCommands;
begin
  DbgSources := TList.Create;
  RegisterCommand(['where'], TWhereCommand.Create);
end;

procedure ReleaseDebugCommands;
begin
  DbgSources.Free;
end;
  
procedure LoadDebugInfo(const FileName: string);
var
  source  : TDbgDataSource;
begin
  source := GetDataSource(FileName);  
  if not Assigned(source) then 
    writeln('cannot find reader for the file: ', FileName);
  DbgSources.Add(source);
end;  

procedure LoadExeDebugInfo(const cmdLine: string);
var
  binname : String;
begin
  binName := GetBinaryName(cmdLine);
  LoadDebugInfo(binName);
end;

initialization
  InitDebugCommands;

finalization
  ReleaseDebugCommands;

end.

