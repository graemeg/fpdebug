unit cmdDbgEx;
// the unit contains additional duby-line commands
// to debug loaded debug-info 

interface

{$ifdef fpc}{$mode delphi}{$H+}{$endif}
uses
  Classes, SysUtils,
  commands,
  dbgInfoTypes;

implementation

type

  { TdbgInfoFilesCommand }

  TdbgInfoFilesCommand = class(TCommand)
  public
    procedure Execute(CmdParams: TStrings; Env: TCommandEnvironment); override;
    function ShortHelp: String; override;
  end;

{ TTdbgInfoFilesCommand }

procedure TdbgInfoFilesCommand.Execute(CmdParams:TStrings;Env:TCommandEnvironment);
begin

end;

function TdbgInfoFilesCommand.ShortHelp:String;
begin
  Result:='Returns the list of debug info files';
end;


procedure InitDebugInfoExCommands;
begin
  RegisterCommand(['dbgfiles'], TdbgInfoFilesCommand.Create);
end;
initialization
  InitDebugInfoExCommands;
end.

