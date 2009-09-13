unit execview; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, commands, 
  dbgTypes, dbgInfoTypes; 
  
  
type
  { TInfoExec }
  TInfoExec = class(TCommand)
    procedure Execute(CmdParams: TStrings; Process: TDbgProcess); override;  
  end;
  
procedure SetCommandLine(const CmdLine: String);

var
  mainexec  : String;
  ExecData  : TDbgDataSource = nil;

implementation

procedure SetCommandLine(const CmdLine: String);
begin
  mainexec := CmdLine;    
  writeln('Commnad line set = ', CmdLine);
  ExecData := GetDataSource(TFileStream.Create(mainexec, fmOpenRead or fmShareDenyNone), true);
  writeln('ExecData = ', PtrInt(ExecData));
end;

{ TInfoExec }

procedure TInfoExec.Execute(CmdParams: TStrings; Process: TDbgProcess);  
begin
  writeln(' Main executable: ', mainexec);
  if Assigned(ExecData) then begin
    writeln('   Debug info source found');
    writeln('   Data sections = ', ExecData.SectionsCount);
  end else
    writeln(' executable format is unknown ');
end;

initialization
  RegisterCommand(['infoexec', 'ie'], TInfoExec.Create);
  
finalization
  ExecData.Free;
  
end.

