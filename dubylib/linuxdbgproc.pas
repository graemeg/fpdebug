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

function ReadProcMem(pid: Integer; Offset: TDbgPtr; Size: Integer; var Data: array of Byte): Integer;
function ReadProcMemUser(pid: Integer; Offset: TDbgPtr; Size: Integer; var Data: array of Byte): Integer;

implementation

function ReadProcMem(pid: Integer; Offset: TDbgPtr; Size: Integer; var Data: array of Byte): Integer;
var
  i   : TDbgPtr;
  cnt : integer;
  ofs : Integer;
begin
  cnt := Size div sizeof(TPtraceWord);
  i := 0;
  while i < Size do begin
    ptracePeekData(pid, Offset+i, PPtraceWord(@Data[i])^);
    inc(i, sizeof(TPtraceWord));
  end;
  Result := i;
end;

function ReadProcMemUser(pid: Integer; Offset: TDbgPtr; Size: Integer; var Data: array of Byte): Integer;
var
  i   : integer;
  cnt : integer;
  ofs : Integer;
begin
  cnt := Size div sizeof(TPtraceWord);
  i := 0;
  while i < Size do begin
    ptracePeekUser(pid, Offset+i, PPtraceWord(@Data[i])^);
    inc(i, sizeof(TPtraceWord));
  end;
  Result := i;
end;

function IntToStr(i: int64): String;
begin
  Str(i, result);
end;

function ForkAndDebugProcess(const ACmdLine: String; var childid: TPid): Boolean;
var
  res     : TPtraceWord;
begin
  childid := FpFork;
  if childid < 0 then begin
    Result := false;
  end else if childid = 0 then begin
    res := ptraceMe;
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
  if Result then TermSignal := wtermsig(Status);
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

function StopSigToEventKind(ASig: Integer): TDbgEventKind;
begin
  case ASig of
    SIGTRAP: Result := dek_BreakPoint;
  else
    Result := dek_Other;
  end;
end;

function StopSigToStr(stopsig: Integer): String;
begin
  case stopsig of
    SIGHUP: Result := 'SIGHUP';
    SIGINT: Result := 'SIGINT';
    SIGQUIT: Result := 'SIGQUIT';
    SIGILL: Result := 'SIGILL';
    SIGTRAP: Result := 'SIGTRAP';
    SIGABRT: Result := 'SIGABRT';
    //SIGIOT: Result := 'SIGIOT';
    SIGBUS: Result := 'SIGBUS';
    SIGFPE: Result := 'SIGFPE';
    SIGKILL: Result := 'SIGKILL';
    SIGUSR1: Result := 'SIGUSR1';
    SIGSEGV: Result := 'SIGSEGV';
    SIGUSR2: Result := 'SIGUSR2';
    SIGPIPE: Result := 'SIGPIPE';
    SIGALRM: Result := 'SIGALRM';
    SIGTerm: Result := 'SIGTERM';
    SIGSTKFLT: Result := 'SIGSTKFLT';
    SIGCHLD: Result := 'SIGCHLD';
    SIGCONT: Result := 'SIGCONT';
    SIGSTOP: Result := 'SIGSTOP';
    SIGTSTP: Result := 'SIGTSTP';
    SIGTTIN: Result := 'SIGTTIN';
    SIGTTOU: Result := 'SIGTTOU';
    SIGURG:  Result := 'SIGURG';
    SIGXCPU: Result := 'SIGXCPU';
    SIGXFSZ: Result := 'SIGFXSZ';
    SIGVTALRM: Result := 'SIGVTALRM';
    SIGPROF: Result := 'SIGPROF';
    SIGWINCH: Result := 'SIGWINCH';
    SIGIO: Result := 'SIGIO';
    SIGPWR: Result := 'SIGPWR';
    SIGUNUSED: Result := 'SIGUNUSED';
  else
    Result := 'SIG#'+IntToStr(stopsig);
  end;
end;


procedure SigInfoToEvent(const siginfo: TSigInfo; var event: TDbgEvent);
begin
  event.debug := event.debug + ' si_code='+IntToStr(siginfo.si_code)+
                               ' si_errno='+IntToStr(siginfo.si_errno);
  case siginfo.si_signo of
    SIGILL, SIGFPE, SIGSEGV, SIGBUS: begin
      event.Kind := dek_SysExc;
      event.Addr := TDbgPtr(siginfo._sifields._sigfault._addr);
    end;
  end;
end;

function WaitStatusToDbgEvent(ChildID: TPid; Status: Integer; var event: TDbgEvent): Boolean;
var
  termSig : Integer;
  exitSig : Integer;
  stopSig : Integer;
  isTerm  : Boolean;
  isExit  : Boolean;
  siginfo : tsiginfo;
  res     : TPtraceWord;
begin
  //writeln('Status = ', Status);
  Result := true;
  if isStopped(Status, stopSig) then begin
    event.Debug := 'stop ' + StopSigToStr(stopsig);
    event.Kind := StopSigToEventKind(stopSig);

    res := ptraceGetSigInfo(ChildID, siginfo);
    if res = 0 then begin
      SigInfoToEvent(siginfo, event);
    end else
      event.Debug:=event.Debug + ' can''t get siginfo';

  end else begin
    isTerm := isTerminated(Status, termSig);
    if not isTerm then begin
      isExit := isExited(Status, exitSig);
      event.Debug := 'term, sig = ' + IntToStr(exitSig);
      event.Kind := dek_ProcessTerminated;
    end else begin
      event.Debug := 'else? ';
      event.Kind := dek_Other;
    end;
  end;
end;

end.

