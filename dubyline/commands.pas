unit commands; 

{$ifdef fpc}{$mode delphi}{$H+}{$endif}

interface

uses
  Classes, SysUtils, 
  dbgTypes, dbgMain; 

type
  TCommandEnvironment = class
    function Main: TDbgMain; virtual; abstract;
    function Process: TDbgProcess; virtual; abstract; // process where stopped
    function Thread: TDbgThread; virtual; abstract; // thread there stopped
  end;
  
  { TCommand }

  TCommand = class(TObject)
  public
    ProcessID : TDbgProcessID;
    ThreadID  : TDbgThreadID;
    procedure Execute(CmdParams: TStrings; Env: TCommandEnvironment); virtual; abstract;
    procedure PrintHelp; virtual; 
    function ShortHelp: String; virtual;
    function ResetParamsCache: Boolean; virtual;
  end;
  
function RegisterCommand(const Keys: array of String; ACommand: TCommand): Boolean;
function FindCommand(const Key: String): TCommand;
function ExecuteCommand(Params: TStrings; var Env: TCommandEnvironment; var ExecutedCommand: TCommand): Boolean;

implementation

var 
  keyslist : TStringList;
  cmdlist  : TList;
  
type 
  { THelpCommand }
  THelpCommand = class(TCommand)
  public
    procedure Execute(CmdParams: TStrings; Env: TCommandEnvironment); override;
    function ShortHelp: String; override;
  end;
  
  { TExitCommand }

  TExitCommand = class(TCommand)
    procedure Execute(CmdParams: TStrings; Env: TCommandEnvironment); override;
    procedure PrintHelp; override;
    function ShortHelp: String; override;
  end;

{ TExitCommand }

procedure TExitCommand.Execute(CmdParams: TStrings; Env: TCommandEnvironment);
begin
  Halt;
end;

procedure TExitCommand.PrintHelp;  
begin
  writeln(ShortHelp);
end;

function TExitCommand.ShortHelp: String;  
begin
  Result := 'quits debugger';
end;
  
function GetCommandKeys(cmd: TCommand): String;
var
  i : Integer;
begin
  Result := '';
  for i := 0 to keyslist.Count - 1 do
    if keyslist.Objects[i] = cmd then begin
      if Result = '' then Result := keyslist[i]
      else Result := Result + ', ' + keyslist[i];
    end;
end;          

{ THelpCommand }

procedure THelpCommand.Execute(CmdParams: TStrings; Env: TCommandEnvironment);
var
  i   : Integer;
  nm  : String;
  cmd : TCommand;
begin
  if CmdParams.Count > 1 then begin
    nm := CmdParams[0];
    cmd := FindCommand(nm);
    if Assigned(cmd) then cmd.PrintHelp;
  end else 
    for i := 0 to cmdlist.Count - 1 do begin
      cmd := TCommand(cmdlist[i]);
      writeln(GetCommandKeys(cmd):15,' - ' + cmd.ShortHelp);
    end
end;

function THelpCommand.ShortHelp: String;  
begin
  Result:='Prints help';          
end;  

{ TCommand }

procedure TCommand.PrintHelp; 
begin

end;

function TCommand.ShortHelp: String; 
begin
  Result := '';
end;

function TCommand.ResetParamsCache: Boolean; 
begin
  Result := false;
end;
  
procedure InitCommands;
begin
  keyslist := TStringList.Create;
  keyslist.CaseSensitive := false;
  cmdlist := TList.Create;
end;  

procedure ReleaseCommands;
begin
  keyslist.Free;
  cmdlist.Free;
end;

function RegisterCommand(const Keys: array of String; ACommand: TCommand): Boolean;
var
  i : Integer;
begin
  Result := false;
  for i := 0 to length(Keys) - 1 do 
    if keyslist .IndexOf(Keys[i]) < 0 then begin
      keyslist.AddObject(Keys[i], ACommand);
      if not Result then begin
        Result := true;
        cmdlist.Add(ACommand);
      end;
    end;
end;

procedure RegisterDefault;
begin
  RegisterCommand(['help','h','?'], THelpCommand.Create);
  RegisterCommand(['exit','quit','q'], TExitCommand.Create);
end;
  
function FindCommand(const Key: String): TCommand;
var
  i : integer;
begin
  i := keyslist.IndexOf(Key);
  if i < 0 then Result := nil
  else Result := TCommand(keyslist.Objects[i]);
end;  

function ExecuteCommand(Params: TStrings; var Env: TCommandEnvironment; var ExecutedCommand: TCommand): Boolean;
var
  cmd : TCommand;
begin
  Result := false;
  ExecutedCommand := nil;
  if not Assigned(Params) then Exit;
  cmd := FindCommand(Params[0]);
  if not Assigned(cmd) then Exit;
  cmd.Execute(Params, Env);
  ExecutedCommand := cmd;
  Result := true;
end;

initialization
  InitCommands;
  RegisterDefault;
  
finalization
  ReleaseCommands;

end.

