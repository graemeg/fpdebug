{
    fpDebug  -  A debugger for the Free Pascal Compiler.

    Copyright (c) 2012 by Graeme Geldenhuys.

    See the file LICENSE.txt, included in this distribution,
    for details about redistributing fpDebug.

    Description:
      .
}
unit execview;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, commands, 
  dbgTypes, dbgInfoTypes; 
  
  
type
  { TInfoExec }
  TInfoExec = class(TCommand)
    procedure Execute(CmdParams: TStrings; Process: TDbgTarget); override;  
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

procedure TInfoExec.Execute(CmdParams: TStrings; Process: TDbgTarget);  
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

