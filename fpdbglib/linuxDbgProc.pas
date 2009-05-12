unit linuxDbgProc;

{$mode objfpc}{$H+}

interface

uses
  BaseUnix, Unix, nixPtrace, dbgTypes;

function ForkAndDebugProcess(const ACmdLine: String; var childid: TPid): Boolean;
function WaitStatusToDbgEvent(ChildID: TPid; Status: Integer; var event: TDbgEvent): Boolean;

function isTerminated(Status: Integer; var TermSignal: Integer): Boolean;
function isStopped(Status: Integer; var StopSignal: Integer): Boolean;
function isExited(Status: Integer; var ExitStatus: Integer): Boolean;

implementation

function ForkAndDebugProcess(const ACmdLine: String; var childid: TPid): Boolean;
var
  res     : TPtraceWord;
begin
  childid := FpFork;
  if childid < 0 then begin
    Result := false;
  end else if childid = 0 then begin
    res := _ptrace_traceme;
    writeln('ptrace_traceme res = ', res);
    if res < 0 then Exit; // process cannot be traced
    writeln('ACmdLine = ', ACmdLine);
    FpExecVE(ACmdLine, nil, nil);
  end else
    Result := true;
end;

function isTerminated(Status: Integer; var TermSignal: Integer): Boolean;
begin
  Result := wifsignaled(Status);
  if Result then TermSignal := wstopsig(Status);
end;

function isStopped(Status: Integer; var StopSignal: Integer): Boolean;
begin
  Result := WIFSTOPPED(Status);
  if Result then StopSignal := wstopsig(Status);
end;

function isExited(Status: Integer; var ExitStatus: Integer): Boolean;
begin
  Result := wifexited(Status);
  if Result then ExitStatus := wexitStatus(Status);
end;

function WaitStatusToDbgEvent(ChildID: TPid; Status: Integer; var event: TDbgEvent): Boolean;
var
  termSig : Integer;
  exitSig : Integer;
  stopSig : Integer;
  isTerm  : Boolean;
  isExit  : Boolean;
begin
  writeln('Status = ', Status);
  Result := true;
  isTerm := isTerminated(Status, termSig);
  if not isTerm then
    isExit := isExited(Status, exitSig);

  if isStopped(Status, stopSig) then writeln('* stopped = ', StopSig);
  if isTerm then writeln('* signaled/terminated ', termSig);
  if isExit then writeln('* exited ', exitSig);

  if isTerm or isExit then begin
    event.Kind := dek_ProcessTerminated;
  end else
    event.Kind := dek_Other;
end;

end.

