{
    fpDebug  -  A debugger for the Free Pascal Compiler.

    Copyright (c) 2012 by Graeme Geldenhuys.

    See the file LICENSE.txt, included in this distribution,
    for details about redistributing fpDebug.

    Description:
      .
}
unit cmdDbgEx;
// the unit contains additional duby-line commands
// to debug loaded debug-info 

interface

{$ifdef fpc}{$mode delphi}{$H+}{$endif}
uses
  Classes, SysUtils,
  commands, cmdDbg,
  dbgInfoTypes;

implementation

type
  { TdbgInfoFilesCommand }
  TdbgInfoFilesCommand = class(TCommand)
  public
    procedure Execute(CmdParams: TStrings; Env: TCommandEnvironment); override;
    function ShortHelp: String; override;
  end;

  { TdbgSourceFilesCommand}

  TdbgSourceFilesCommand = class(TCommand)
  public
    procedure Execute(CmdParams: TStrings; Env: TCommandEnvironment); override;
    function ShortHelp: String; override;
  end;

  { TdbgEnumFileSymCommand }

  TdbgEnumFileSymCommand = class(TCommand)
  public
    procedure Execute(CmdParams: TStrings; Env: TCommandEnvironment); override;
    function ShortHelp: String; override;
  end;

{ TTdbgInfoFilesCommand }

procedure TdbgInfoFilesCommand.Execute(CmdParams:TStrings;Env:TCommandEnvironment);
var
  i : Integer;
begin
  writeln('debug info files used: ');
  for i:=0 to DbgInfoFiles.Count-1 do
    writeln('  ', DbgInfoFiles[i]);
  writeln;
end;

function TdbgInfoFilesCommand.ShortHelp:String;
begin
  Result:='Returns the list of debug info files';
end;


{ TdbgSourceFilesCommand }

procedure TdbgSourceFilesCommand.Execute(CmdParams:TStrings;Env:TCommandEnvironment);
var
  i : Integer;
  l : TStringList;
begin
  l := TStringList.Create;
  try
    CommonInfo.EnumSourceFiles(l);
    writeln('source files:');
    for i:=0 to l.Count-1 do
      writeln('  ', l[i]);
  finally
    l.Free;
  end;
end;

function TdbgSourceFilesCommand.ShortHelp:String;
begin
  Result:='Returns the list source files with debug info available';
end;

function isFileNameMatch(const Name, Matching: string): Boolean;
var
  l1, l2: string;
begin
  l1:=AnsiLowerCase(Name);
  l2:=AnsiLowerCase(Matching);
  Result:=(l1=l2) or
          (CompareText(ExtractFileName(l1), ExtractFileName(l2))=0);
end;

procedure DumpInfoSym(info: TDbgSymbol; const prefix: string='');
var
  i : Integer;
begin
  writeln(prefix+info.Name);
  for i:=0 to info.Count-1 do
    DumpInfoSym(info.Child[i], prefix+'  ');
end;

{ TdbgEnumFileSymCommand }

procedure TdbgEnumFileSymCommand.Execute(CmdParams:TStrings;Env: TCommandEnvironment);
var
  i     : Integer;
  fn    : string;
  t     : TStringList;
  info  : TDbgSymFile;
begin
  if CmdParams.Count<=1 then begin
    writeln('please specify file name');
    Exit;
  end;
  fn:=CmdParams[1];
  t:=TStringList.Create;
  CommonInfo.EnumSourceFiles(t);
  for i:=0 to t.Count-1 do begin
    if not isFileNameMatch(t[i], fn) then Continue;
    info:=CommonInfo.FindFile(t[i]);
    if not Assigned(info) then
      writeln('no information about:', t[i])
    else
      DumpInfoSym(info);
  end;
  t.Free;
end;

function TdbgEnumFileSymCommand.ShortHelp:String;
begin
  Result:='Enumerates debug symbols in the give file';
end;


procedure InitDebugInfoExCommands;
begin
  RegisterCommand(['dbginfos'], TdbgInfoFilesCommand.Create);
  RegisterCommand(['enumfiles'], TdbgSourceFilesCommand.Create);
  RegisterCommand(['enumfilesym'], TdbgEnumFileSymCommand.Create);
end;

initialization
  InitDebugInfoExCommands;
end.

