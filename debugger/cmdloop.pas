unit cmdloop; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, dbgTypes, commands; 

procedure RunLoop(Process: TDbgProcess);
  
implementation

var
  LastCommand : String;
  DbgEvent    : TDbgEvent;

  Running       : Boolean = false;
  callwaitnext  : Boolean = false;

const
  CmdPrefix = 'duby> ';

type
  { TRunComand }

  TRunComand = class(TCommand)
    procedure Execute(CmdParams: TStrings; Process: TDbgProcess); override;
  end;

  { TContinueCommand }

  TContinueCommand = class(TCommand)
    procedure Execute(CmdParams: TStrings; Process: TDbgProcess); override;
  end;

{ TContinueCommand }

procedure TContinueCommand.Execute(CmdParams: TStrings; Process: TDbgProcess);
begin
  if not Running then
    writeln('not running')
  else
    callwaitnext := true;
end;

{ TRunComand }

procedure TRunComand.Execute(CmdParams: TStrings; Process: TDbgProcess);
begin
  if not Running then begin
    running := true;
    writeln('starting process');
    callwaitnext := true;
  end else
    writelN('process already started');
end;
  
procedure ExecuteNextCommand;
var
  s : string;
  p : TStringList;
begin
  write(CmdPrefix);
  readln(s);
  if s = '' then begin
    s := LastCommand;
    writeln(s);
  end;

  if s <> '' then begin
    p := TStringList.Create;
    p.Add(s);
    if not ExecuteCommand(p, nil) then
      writeln('unknown command ', p[0]);
    LastCommand := s;
    p.Free;
  end;
end;

procedure DoRunLoop(Process: TDbgProcess);
var
  ProcTerm  : Boolean;
begin
  if not Assigned(Process) then begin
    writeln('no process to debug (internal error?)');
    Exit;
  end;

  ProcTerm := false;
  while true do begin
    callwaitnext := false;
    ExecuteNextCommand;

    if CallWaitNext and not ProcTerm then begin
      if not Process.WaitNextEvent(DbgEvent) then
        writeln('process terminated?')
      else begin
        writeln('event: ', DbgEvent.Debug);
      end;
      if DbgEvent.Kind = dek_ProcessTerminated then
        writeln('process has been terminated');
    end;
  end;
end;

procedure RunLoop(Process: TDbgProcess);
begin
  try
    DoRunLoop(Process);
  except
  end;
end;

procedure RegisterLoopCommands;
begin
  RegisterCommand(['run','r'], TRunComand.Create);
  RegisterCommand(['c'], TContinueCommand.Create);
end;

initialization
  RegisterLoopCommands;

end.

