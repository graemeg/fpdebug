unit cmdloop;

{$ifdef fpc}{$mode delphi}{$H+}{$endif}

interface

uses
  Classes, SysUtils, dbgTypes, dbgUtils, dbgConsts, commands; 

procedure RunLoop(Process: TDbgProcess);

type
  TEventHandler = procedure (Process: TDbgProcess; Event : TDbgEvent) of object;

procedure InstallHandler(AHandler: TEventHandler);
procedure RemoveHandler(AHandler: TEventHandler);
procedure HandleEvent(Process: TDbgProcess; Event : TDbgEvent);
  
implementation

var
  LastCommand   : String;
  DbgEvent      : TDbgEvent;

  Running       : Boolean = False;
  WaitForNext   : Boolean = False;
  StopOnSysCall : Boolean = False;
  
  EventHandlers : TFPList;  

const
  CmdPrefix = 'duby> ';
  HexSize   = sizeof(TDbgPtr) * 2;

type
  { TRunCommand }

  TRunCommand = class(TCommand)
    procedure Execute(CmdParams: TStrings; Process: TDbgProcess); override;
    function ShortHelp: String; override;
  end;
  
  { TRunToCommand }

  TRunToCommand = class(TCommand)
    procedure Execute(CmdParams: TStrings; Process: TDbgProcess); override;
    function ShortHelp: String; override;
  end;
  
  { TStepCommand }

  TStepCommand = class(TCommand)
    procedure Execute(CmdParams: TStrings; Process: TDbgProcess); override;
    function ShortHelp: String; override;
  end;

  { TContinueCommand }

  TContinueCommand = class(TCommand)
    procedure Execute(CmdParams: TStrings; Process: TDbgProcess); override;
    function ShortHelp: String; override;
  end;

{ TRunToCommand }

procedure TRunToCommand.Execute(CmdParams: TStrings; Process: TDbgProcess);  
var
  reg : TDbgDataBytesList;
  addr : TDbgPtr;
  err  : integer;
begin 
  if CmdParams.Count > 1 then Val(CmdParams[1], addr, err);
  //todo: replace with break-point usage!
  if (CmdParams.Count <= 1) or (err <> 0) then begin
    writeln('please specify valid address');
    Exit;
  end;
  
  writeln('run-to = ', IntToHex(Addr, Sizeof(TDbgPtr)*2 ));
  
  reg := TDbgDataBytesList.Create;
  try
    if not Process.GetThreadRegs(Process.MainThreadID, reg) then begin
      writeln('unable to read thread regs');
      Exit;
    end;
    if reg.Reg[_Eip].DbgPtr = addr then begin
      writeln('addr achieved: ', addr);
      Exit;
    end;
    
    Process.SetSingleStep(Process.MainThreadID);
    while Process.WaitNextEvent(DbgEvent) do begin
      if not Process.GetThreadRegs(Process.MainThreadID, reg) then begin
        writeln('unable to read thread regs');
      end;
      
      //writeln('eip = ', IntToHex(reg.Reg[_Eip].DbgPtr, Sizeof(TDbgPtr)*2 ));
      //writeln('eax = ', IntToHex(reg.Reg[_Eax].UInt32, Sizeof(LongWord)*2 ));
      if reg.Reg[_Eip].DbgPtr = addr then begin
        writeln('addr achieved: ', addr);
        Break;        
      end;
      Process.SetSingleStep(Process.MainThreadID);
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

procedure TStepCommand.Execute(CmdParams: TStrings; Process: TDbgProcess);  
begin
  if not Running then WriteLn('not running');
    
  Process.SetSingleStep( Process.MainThreadID );
  
  WaitForNext := true;
end;

function TStepCommand.ShortHelp: String;  
begin
  Result:='makes a single step in (the mainthead) process';
end;

{ TContinueCommand }

procedure TContinueCommand.Execute(CmdParams: TStrings; Process: TDbgProcess);
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

procedure TRunCommand.Execute(CmdParams: TStrings; Process: TDbgProcess);
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

function GetNextWord(const s: AnsiString; var index: Integer): String;
const
  WhiteSpace = [' ',#8,#10];
  Literals   = ['"',''''];
var
  Wstart,wend : Integer;
  InLiteral   : Boolean;
  LastLiteral : AnsiChar;

begin
  WStart:=index;
  while (WStart<=Length(S)) and (S[WStart] in WhiteSpace) do
    Inc(WStart);

  WEnd:=WStart;
  InLiteral:=False;
  LastLiteral:=#0;
  while (Wend<=Length(S)) and (not (S[Wend] in WhiteSpace) or InLiteral) do begin
    if S[Wend] in Literals then
      If InLiteral then
        InLiteral:=not (S[Wend]=LastLiteral)
      else begin
        InLiteral:=True;
        LastLiteral:=S[Wend];
      end;
    inc(wend);
  end;

  Result:=Copy(S,WStart,WEnd-WStart);

  if (Length(Result) > 0)
     and (Result[1] = Result[Length(Result)]) // if 1st char = last char and..
     and (Result[1] in Literals) then // it's one of the literals, then
    Result:=Copy(Result, 2, Length(Result) - 2); //delete the 2 (but not others in it)

  while (WEnd<=Length(S)) and (S[Wend] in WhiteSpace) do
    inc(Wend);
  index := Wend;
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

  
procedure ExecuteNextCommand(AProcess: TDbgProcess);
var
  s : string;
  p : TStringList;
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
    ParseCommand(s, p);
    if not ExecuteCommand(p, AProcess, cmd) then 
      writeln('unknown command ', p[0])
    else begin
      LastCommand := s;
      if cmd.ResetParamsCache then 
        LastCommand := p[0];
    end;
    p.Free;
  end;
end;

procedure DoRunLoop(Process: TDbgProcess);
var
  ProcTerm     : Boolean;
  StopForUser  : Boolean;
begin
  if not Assigned(Process) then begin
    writeln('no process to debug (internal error?)');
    Exit;
  end;

  ProcTerm := false;
  StopForUser := true;

  while true do begin
    if StopForUser then begin
      WaitForNext := false;
      ExecuteNextCommand(Process);
    end else
      WaitForNext := true;
    StopForUser  := true;

    if WaitForNext and not ProcTerm then begin
      if not Process.WaitNextEvent(DbgEvent) then
        writeln('process terminated?')
      else begin
        
        HandleEvent( Process, DbgEvent);
        
        writeln('event: ', DbgEvent.Debug);
        case DbgEvent.Kind of
          dek_SysExc:
            writeln('system exception at ', IntToHex(DbgEvent.Addr, HexSize));
          dek_SysCall:
          begin
            writeln('system call: ', DbgEvent.Debug);
            StopForUser := StopOnSysCall;
          end;
        end;
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

type
  TProcHandler = class(TObject)
    Handler: TEventHandler;
  end;

// Hanlder routines

procedure InstallHandler(AHandler: TEventHandler);
var
  p : TProcHandler;
begin
  p := TProcHandler.Create;
  p.Handler := AHandler;
  EventHandlers.Add( p );
end;

procedure RemoveHandler(AHandler: TEventHandler);
var
  i : Integer;
begin
  for i := 0 to EventHandlers.Count - 1 do
    if @TProcHandler(EventHandlers[i]).Handler = @AHandler then begin
      TProcHandler(EventHandlers[i]).Free;
      EventHandlers.Delete(i);
      Exit;
    end;
end;

procedure HandleEvent(Process: TDbgProcess; Event: TDbgEvent);
var
  i : Integer;
begin
  for i := 0 to EventHandlers.Count - 1 do
    try
      writelN('handling !!!');
      TProcHandler(EventHandlers[i]).Handler(Process, Event);
    except
    end;
end;

procedure InitHandlers;
begin
  EventHandlers := TFPList.Create;  
end;

procedure ReleaseHandlers;
var
  i : Integer;
begin
  for i := 0 to EventHandlers.Count - 1 do TProcHandler(EventHandlers[i]).free;
  EventHandlers.Free;
end;

// Registering commands
procedure RegisterLoopCommands;
begin
  RegisterCommand(['run','r'], TRunCommand.Create);
  RegisterCommand(['runto','rt'], TRunToCommand.Create);
  RegisterCommand(['c'], TContinueCommand.Create);
  RegisterCommand(['step','s'], TStepCommand.Create);
end;

initialization
  RegisterLoopCommands;
  InitHandlers;
 
finalization
  ReleaseHandlers;
    
  

end.

