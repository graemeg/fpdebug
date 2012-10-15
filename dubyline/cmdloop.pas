unit cmdloop;

{$ifdef fpc}{$mode delphi}{$H+}{$endif}

interface

uses
  Classes, SysUtils, 
  dbgTypes, dbgUtils, dbgConsts, dbgMain,
  commands; 

procedure RunLoop(ATarget: TDbgMain);

{type
  TEventHandler = procedure (Process: TDbgTarget; Event : TDbgEvent) of object;

procedure InstallHandler(AHandler: TEventHandler);
procedure RemoveHandler(AHandler: TEventHandler);
procedure HandleEvent(Process: TDbgTarget; Event : TDbgEvent);}

function LastEvent: TDbgEvent;
  
implementation

var
  LastCommand   : String;
  DbgEvent      : TDbgEvent;
  Target        : TDbgMain;

  Running       : Boolean = False;
  WaitForNext   : Boolean = False;
  StopOnSysCall : Boolean = False;
  
 // EventHandlers : TFPList;
  
type
  TDebugEnvironment=class(TCommandEnvironment)
  public
    function Main: TDbgMain; override;
    function Process: TDbgProcess; override;
    function Thread: TDbgThread; override;
  end;

function TDebugEnvironment.Main: TDbgMain; 
begin
  Result:=Target;
end;

function TDebugEnvironment.Process: TDbgProcess; 
begin
  Result:=Main.FindProcess( DbgEvent.Process );
end;

function TDebugEnvironment.Thread: TDbgThread; 
begin
  Result:=Main.FindThread( DbgEvent.Process, DbgEvent.Thread );
end;
  
const
  CmdPrefix = 'duby> ';
  HexSize   = sizeof(TDbgPtr) * 2;

type
  { TRunCommand }

  TRunCommand = class(TCommand)
    procedure Execute(CmdParams: TStrings; Env: TCommandEnvironment); override;
    function ShortHelp: String; override;
  end;
  
  { TRunToCommand }

  TRunToCommand = class(TCommand)
    procedure Execute(CmdParams: TStrings; Env: TCommandEnvironment); override;
    function ShortHelp: String; override;
  end;
  
  { TStepCommand }

  TStepCommand = class(TCommand)
    procedure Execute(CmdParams: TStrings; Env: TCommandEnvironment); override;
    function ShortHelp: String; override;
  end;

  { TContinueCommand }

  TContinueCommand = class(TCommand)
    procedure Execute(CmdParams: TStrings; Env: TCommandEnvironment); override;
    function ShortHelp: String; override;
  end;

{ TRunToCommand }

procedure TRunToCommand.Execute(CmdParams: TStrings; Env: TCommandEnvironment);
var
  reg : TDbgDataBytesList;
  addr : TDbgPtr;
  err  : integer;
begin 
  writeln('sorry not implemented');
  Exit;
  
  if CmdParams.Count > 1 then Val(CmdParams[1], addr, err);
  //todo: replace with break-point usage!
  if (CmdParams.Count <= 1) or (err <> 0) then begin
    writeln('please specify valid address');
    Exit;
  end;
  
  writeln('run-to = ', IntToHex(Addr, Sizeof(TDbgPtr)*2 ));
  
  reg := TDbgDataBytesList.Create;
  try
    if not Env.Thread.GetThreadRegs(reg) then begin
      writeln('unable to read thread regs');
      Exit;
    end;
    if reg.Reg[_Eip].DbgPtr = addr then begin
      writeln('addr achieved: ', addr);
      Exit;
    end;
    
  finally
    reg.Free;
  end;
end;

function TRunToCommand.ShortHelp: String;  
begin
  Result:='IT''S SLOW!!! runs to a specific address';
end;

{ TStepCommand }

procedure TStepCommand.Execute(CmdParams: TStrings; Env: TCommandEnvironment);
var
  Success: Boolean;
begin
  if not Running then WriteLn('not running');
    
  Success:=Assigned(Env.Thread) and Env.Thread.NextSingleStep;
  if not Success then writeln('unable to make a step');
  
  WaitForNext := true;
end;

function TStepCommand.ShortHelp: String;  
begin
  Result:='makes a single step in the (main thread) process';
end;

{ TContinueCommand }

procedure TContinueCommand.Execute(CmdParams: TStrings; Env: TCommandEnvironment);
begin
  if not Running then
    writeln('not running')
  else
    WaitForNext := true;
end;

function TContinueCommand.ShortHelp: String;  
begin
  Result:='continues process execution';
end;

{ TRunComand }

procedure TRunCommand.Execute(CmdParams: TStrings; Env: TCommandEnvironment);
begin
  if not Running then begin
    running := true;
    writeln('starting process');
    WaitForNext := true;
  end else
    writelN('process already started');
end;

function TRunCommand.ShortHelp: String;  
begin
  Result:='runs the process';
end;

function GetNextWord(const s: AnsiString; var index: Integer): AnsiString;
const
  WhiteSpace = [' ',#8,#10];
  Literals   = ['"',''''];
var
  wStart,wEnd : Integer;
  InLiteral   : Boolean;
  LastLiteral : AnsiChar;

begin
  wStart:=index;
  while (wStart<=Length(S)) and (S[wStart] in WhiteSpace) do
    Inc(wStart);

  wEnd:=wStart;
  InLiteral:=False;
  LastLiteral:=#0;
  while (wEnd<=Length(S)) and (not (S[wEnd] in WhiteSpace) or InLiteral) do begin
    if S[wEnd] in Literals then
      If InLiteral then
        InLiteral:=not (S[wEnd]=LastLiteral)
      else begin
        InLiteral:=True;
        LastLiteral:=S[wEnd];
      end;
    inc(wEnd);
  end;

  Result:=Copy(S,wStart,wEnd-wStart);

  if (Length(Result) > 0)
     and (Result[1] = Result[Length(Result)]) // if 1st char = last char and..
     and (Result[1] in Literals) then // it's one of the literals, then
    Result:=Copy(Result, 2, Length(Result) - 2); //delete the 2 (but not others in it)

  while (wEnd<=Length(S)) and (S[wEnd] in WhiteSpace) do
    inc(wEnd);
  index := wEnd;
end;

procedure ParseCommand(const Cmd: String; items: Tstrings);
var
  i : integer;
  w : String;
begin
  if not Assigned(items)then Exit;
  i:=1;
  while i <= length(Cmd) do begin
    w := GetNextWord(cmd, i);        
    items.Add(w);
  end;  
end;

  
procedure ExecuteNextCommand(Env: TCommandEnvironment);
var
  s   : string;
  p   : TStringList;
  cmd : TCommand;
begin
  write(CmdPrefix);
  readln(s);
  if s = '' then begin
    s := LastCommand;
    writeln(CmdPrefix,s);
  end;

  if s <> '' then begin
    p := TStringList.Create;
    try
      ParseCommand(s, p);
      if not ExecuteCommand(p, Env, cmd) then
        writeln('unknown command ', p[0])
      else begin
        LastCommand := s;
        if cmd.ResetParamsCache then
          LastCommand := p[0];
      end;
    finally
      p.Free;
    end;
  end;
end;

procedure DoRunLoop(Target: TDbgMain);
var
  ProcTerm     : Boolean;
  StopForUser  : Boolean;
  Env : TDebugEnvironment;
  
const
  dekStr: array [TDbgEventKind] of string = (
    'Other', 'SysExc', 'Single step', 'Breakpoint', 
    'Process Start', 'Process Terminated', 'Thread Start', 'Thread Terminate', 'SysCall');
begin
  if not Assigned(Target) then begin
    writeln('no process to debug (internal error?)');
    Exit;
  end;
  Env:=TDebugEnvironment.Create;
  try
    ProcTerm := false;
    StopForUser := true;

    while not QuitCommand do begin
      if StopForUser then begin
        WaitForNext := false;
        ExecuteNextCommand(Env);
      end else
        WaitForNext := true;
      StopForUser  := true;

      if WaitForNext and not ProcTerm then begin
        if not Target.WaitNextEvent(DbgEvent) then begin
          writeln('the process terminated? (type "quit" to quit)');
        end else begin
          //HandleEvent( Process, DbgEvent);

          case DbgEvent.Kind of
            dek_SysExc:;
            dek_SingleStep:;
              //writeln('single step');
            dek_BreakPoint:;
              //writeln('breakpoint');
            dek_SysCall:
            begin
              //writeln('system call: ', DbgEvent.Debug);
              StopForUser := StopOnSysCall;
            end;
          end;
          writeln('even:    ',  dekStr[DbgEvent.Kind]);
          writeln('process: ',  DbgEvent.Process);
          writeln('thread:  ',  PtrUInt(DbgEvent.Thread));
          writeln('addr:    $', HexAddr(DbgEvent.Addr), ' / ', DbgEvent.Addr);
        end;
        if DbgEvent.Kind = dek_ProcessTerminated then
          writeln('process has been terminated');
      end;
    end;
  finally
    writeln('freeing environment.');
    Env.Free;
  end;
end;

procedure RunLoop(ATarget: TDbgMain);
begin
  try
    Target:=ATarget;
    DoRunLoop(ATarget);
  except
  end;
end;

function LastEvent:TDbgEvent;
begin
  Result:=DbgEvent
end;

function DebugTarget: TDbgMain;
begin
  Result:=Target;
end;

function EventProc: TDbgProcess;
begin
  Result:=Target.FindProcess(DbgEvent.Process);
end;

function EventThread: TDbgThread;
begin
  Result:=Target.FindThread(DbgEvent.Process, DbgEvent.Thread);
end;

// Registering commands
procedure RegisterLoopCommands;
begin
  RegisterCommand(['run','r'], TRunCommand.Create);
  //RegisterCommand(['runto','rt'], TRunToCommand.Create);
  RegisterCommand(['c'], TContinueCommand.Create);
  RegisterCommand(['step','s'], TStepCommand.Create);
end;

initialization
  RegisterLoopCommands;
 
finalization
    
  

end.

