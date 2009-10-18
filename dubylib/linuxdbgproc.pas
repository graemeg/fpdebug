unit linuxDbgProc;

{ Linux base debugging utility function(s) }

{$mode objfpc}{$H+}

interface

uses
  BaseUnix, Unix, nixPtrace,
  dbgTypes, dbgConsts;

function ForkAndDebugProcess(const ACmdLine: String; var childid: TPid): Boolean;
function WaitStatusToDbgEvent(ChildID: TPid; Status: Integer; var event: TDbgEvent): Boolean;

function isTerminated(Status: Integer; var TermSignal: Integer): Boolean;
function isStopped(Status: Integer; var StopSignal: Integer): Boolean;
function isExited(Status: Integer; var ExitStatus: Integer): Boolean;

function ReadProcMem(pid: Integer; Offset: TDbgPtr; Size: Integer; var Data: array of Byte): Integer;
function WriteProcMem(pid: Integer; Offset: TDbgPtr; Size: Integer; const Data: array of Byte): Integer;
function ReadProcMemUser(pid: Integer; Offset: TDbgPtr; Size: Integer; var Data: array of Byte): Integer;
function WriteProcMemUser(pid: Integer; Offset: TDbgPtr; Size: Integer; var Data: array of Byte): Integer;

function ReadRegsi386(pid: Integer; regs: TDbgDataList): Boolean;
function WriteRegsi386(pid: Integer; regs: TDbgDataList): Boolean;

implementation

type
  TByteArray = array [word] of byte;
  PByteArray = ^TByteArray;

function ReadRegsi386(pid: Integer; regs: TDbgDataList): Boolean;
var
  user    : user_32;
begin
  Result := ReadProcMemUser(pid, 0, sizeof(user), PByteArray(@user)^) = sizeof(user);
  if Result then begin
    with user.regs do begin
      regs[_Edi].UInt32 := edi;
      regs[_Esi].UInt32 := esi;
      regs[_Ebx].UInt32 := ebx;
      regs[_Edx].UInt32 := edx;
      regs[_Ecx].UInt32 := ecx;
      regs[_Eax].UInt32 := eax;
      regs[_Ebp].UInt32 := ebp;
      regs[_Eip].UInt32 := eip;
      regs[_EFlags].UInt32 := eflags;
      regs[_Esp].UInt32 := esp;
    end;

    regs[_Dr0].UInt32 := user.u_debugreg[0];
    regs[_Dr1].UInt32 := user.u_debugreg[1];
    regs[_Dr2].UInt32 := user.u_debugreg[2];
    regs[_Dr3].UInt32 := user.u_debugreg[3];
    regs[_Dr4].UInt32 := user.u_debugreg[4];
    regs[_Dr4].UInt32 := user.u_debugreg[5];
    regs[_Dr6].UInt32 := user.u_debugreg[6];
    regs[_Dr7].UInt32 := user.u_debugreg[7];
  end;
end;

function WriteRegsi386(pid: Integer; regs: TDbgDataList): Boolean;
var
  regs32  : user_regs_struct_32;
  res     : Integer;
begin
  Result := ReadProcMemUser(pid, 0, sizeof(regs32), PByteArray(@regs32)^) = sizeof(regs32);
  if not Result then Exit;

  with regs32 do begin
    edi := regs[_Edi].UInt32;
    esi := regs[_Esi].UInt32;
    ebx := regs[_Ebx].UInt32;
    edx := regs[_Edx].UInt32;
    ecx := regs[_Ecx].UInt32;
    eax := regs[_Eax].UInt32;
    ebp := regs[_Ebp].UInt32;
    eip := regs[_Eip].UInt32;
    eflags := regs[_EFlags].UInt32;
    esp := regs[_Esp].UInt32;
  end;

  res := WriteProcMemUser(pid, 0, sizeof(regs32), PByteArray(@regs32)^);
  Result := res = sizeof(regs32);
end;

function ReadProcMem(pid: Integer; Offset: TDbgPtr; Size: Integer; var Data: array of Byte): Integer;
var
  i   : LongWord;
  j   : Integer;
  cnt : integer;
  d   : TPtraceWord;
begin
  cnt := Size div sizeof(TPtraceWord);
  i := 0;
  for j := 0 to cnt - 1 do begin
    ptracePeekData(pid, Offset+i, PPtraceWord(@Data[i])^);
    inc(i, sizeof(TPtraceWord));
  end;
  Result := cnt;

  cnt := Size mod sizeof(TPtraceWord);
  if cnt > 0 then begin
    ptracePeekData(pid, Offset+i, d);
    Move(d, Data[i], cnt);
    inc(Result, cnt);
  end;
end;

function WriteProcMem(pid: Integer; Offset: TDbgPtr; Size: Integer; const Data: array of Byte): Integer;
var
  i   : Integer;
  j   : LongWord;
  wd  : TPtraceWord;
begin
  j := 0;
  for i := 0 to Size div sizeof(TPtraceWord) - 1 do begin
    ptracePokeData(pid, Offset, PPTraceWord(@Data[j])^);
    inc(j, sizeof(TPtraceWord));
    inc(Offset, sizeof(TPtraceWord));
  end;

  if Size mod sizeof(TPtraceWord) > 0 then begin
    ptracePeekData(pid, Offset, wd);
    System.move(Data[j], wd, Size mod sizeof(TPtraceWord));
    ptracePokeData(pid, Offset, wd);
  end;
  Result := Size;
end;

function ReadProcMemUser(pid: Integer; Offset: TDbgPtr; Size: Integer; var Data: array of Byte): Integer;
var
  i   : Integer;
begin
  i := 0;
  while i < Size do begin
    ptracePeekUser(pid, Offset, PPtraceWord(@Data[i])^);
    inc(i, sizeof(TPtraceWord));
    inc(Offset, sizeof(TPtraceWord));
  end;
  Result := i;
end;

function WriteProcMemUser(pid: Integer; Offset: TDbgPtr; Size: Integer; var Data: array of Byte): Integer;
var
  i   : Integer;
begin
  i := 0;
  while i < Size do begin
    ptracePokeUser(pid, Offset, PPtraceWord(@Data[i])^);
    inc(i, sizeof(TPtraceWord));
    inc(Offset, sizeof(TPtraceWord));
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
    if res < 0 then Exit; // process cannot be traced
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

    SIGTRAP: begin
      //writeln('siginfo.si_code = ', siginfo.si_code);

      if (siginfo.si_code = TRAP_TRACE)
        then event.Kind := dek_SingleStep
        else event.Kind := dek_BreakPoint;
    end;
  end;
end;


function GetBreakAddri386(pid: TPid; var CurrentAddr, PrevDelta: TDbgPtr): Boolean; inline;
var
  user  : user_32;
begin
  Result := ReadProcMemUser(pid, 0, sizeof(user), PByteArray(@user)^) = sizeof(user);
  if Result then CurrentAddr:=user.regs.eip;
  PrevDelta:=1;
end;

function GetBreakAddr(pid: TPid; var CurrentAddr, PrevDelta: TDbgPtr): Boolean;
begin
  {$ifdef cpui386}
  Result := GetBreakAddri386(pid, CurrentAddr, PrevDelta);
  {$else}
  Result := false;
  {$endif}
end;

function WaitStatusToDbgEvent(ChildID: TPid; Status: Integer; var event: TDbgEvent): Boolean;
var
  termSig : Integer;
  exitSig : Integer;
  stopSig : Integer;
  isTerm  : Boolean;
  siginfo : tsiginfo;
  res     : TPtraceWord;

  trapaddr  : TDbgPtr;
  trapdelta : TDbgPtr;
begin
  //writeln('Status = ', Status);
  Result := true;
  if isStopped(Status, stopSig) then begin
    event.Debug := 'stop ' + StopSigToStr(stopsig);
    event.Kind := StopSigToEventKind(stopSig);

    res := ptraceGetSigInfo(ChildID, siginfo);
    if res = 0 then begin
      SigInfoToEvent(siginfo, event);
      if stopSIG = SIGTRAP then begin
        if GetBreakAddr(ChildID, trapaddr, trapdelta) then begin
          event.Addr := trapaddr - trapdelta;
        end else
          event.Addr := 0; //address is unknown. Internal error?!
      end;

    end else
      event.Debug:=event.Debug + ' can''t get siginfo';

  end else begin
    isTerm := isTerminated(Status, termSig);
    if not isTerm then begin
      {isExit := }isExited(Status, exitSig);
      event.Debug := 'term, sig = ' + IntToStr(exitSig);
      event.Kind := dek_ProcessTerminated;
    end else begin
      event.Debug := 'else? ';
      event.Kind := dek_Other;
    end;
  end;
end;

end.

