unit macDbgType;

{$mode objfpc}{$H+}

interface

uses
  SysUtils,
  BaseUnix, Unix, machapi, mach_port, 
  dbgTypes, macPtrace, macDbgProc;

type
  { TMachDbgProcess }

  TMachDbgProcess = class(TDbgProcess)
  private
    fchildpid   : TPid;
    fchildtask  : mach_port_t;

    catchport   : mach_port_t;
  protected
    waited      : Boolean;
    waitedsig   : Integer;

    procedure SetupChildTask(child: task_t);

  public
    procedure Terminate; override;
    function WaitNextEvent(var Event: TDbgEvent): Boolean; override;
    function GetProcessState: TDbgState; override;

    function GetThreadsCount: Integer; override;
    function GetThreadID(AIndex: Integer): TDbgThreadID; override;
    function GetThreadRegs(ThreadID: TDbgThreadID; Regs: TDbgDataList): Boolean; override;
    function SetSingleStep(ThreadID: TDbgThreadID): Boolean; override;

    function ReadMem(Offset: TDbgPtr; Count: Integer; var Data: array of byte): Integer; override;
    function WriteMem(Offset: TDbgPtr; Count: Integer; const Data: array of byte): Integer; override;

    function StartProcess(const ACmdLine: String): Boolean;
  end;

implementation

function ForkAndRun(const CommandLine: String; var ChildId: TPid; var ChildTask: mach_port_t): Boolean;
var
  len   : Integer;
  res   : Integer;
  pname : mach_port_name_t;
  kret  : kern_return_t;
begin
  Result := false;
  childid := FpFork;
  if childid < 0 then Exit;

  if childid = 0 then begin
    ptraceme;
    //todo: ptrace_sig_as_exc, what needs to be initialized after going from signals to exceptions?
    ptrace_sig_as_exc;

    res := FpExecV(CommandLine, nil);
    if res < 0 then begin
      writeln('failed to run: ', CommandLine);
      Halt;
    end;

  end else begin
    writeln('self  task = ', mach_task_self);
    writeln('child pid  = ', ChildId);
    pname := 0;

    kret := task_for_pid(mach_task_self, ChildId, pname);
    writeln('task_for_pid, returned = ', kret);

    ChildTask := mach_port_t(pname);
    writeln('child task = ', ChildTask);

    Result := true;
  end;
end;

function TMachDbgProcess.GetProcessState: TDbgState;
begin
  Result := ds_Nonstarted;
end;

function TMachDbgProcess.GetThreadsCount: Integer;
begin
  Result := 0;
end;

function TMachDbgProcess.GetThreadID(AIndex: Integer): TDbgThreadID;
begin
  Result:=nil;
end;

function TMachDbgProcess.GetThreadRegs(ThreadID: TDbgThreadID; Regs: TDbgDataList): Boolean;
begin
  Result:=false;
end;

function TMachDbgProcess.SetSingleStep(ThreadID: TDbgThreadID): Boolean;
begin
  Result := false;
end;

function TMachDbgProcess.ReadMem(Offset: TDbgPtr; Count: Integer; var Data: array of byte): Integer;
var
  r             : QWord;
  portarray     : mach_port_array_t;
  portcount     : integer;
  res           : kern_return_t;

  threads       : Pointer;
  threadscount  : mach_msg_type_number_t;

  pname         : mach_port_name_t;

begin
  writeln('child pid = ', fchildpid);
  debugout_kret(task_for_pid(mach_task_self, fchildpid, pname), 'task_for_pid');
  writeln('task = ', pname);

  res := debugout_kret(task_threads(pname, threads, threadscount), 'task_threads');
  if res = KERN_SUCCESS then writeln('threads count');

  res := debugout_kret( mach_ports_lookup(fchildtask, portarray, portcount), 'mach_ports_lookup');
  if res = KERN_SUCCESS then writeln('ports = ', portcount);

  if ReadTaskMem(fchildtask, Offset, Count, Data, r) <> 0 then
    Result := -1
  else
    Result := r;
end;

function TMachDbgProcess.WriteMem(Offset: TDbgPtr; Count: Integer; const Data: array of byte): Integer;
begin
  Result := -1;
end;

procedure TMachDbgProcess.SetupChildTask(child: task_t);
var
  res : Integer;
begin
  writeln('setting up child task');
  
  writeln('setting procedd exceptions handlers');

  catchport := MachAllocPortForSelf;
  writelN('catch port = ', catchport);
  
  debugout_kret(
    mach_port_insert_right(task_t(mach_task_self), catchport, catchport, MACH_MSG_TYPE_MAKE_SEND), 
    'mach_port_insert_right'
    );
  
  writeln('setting exception port');
  res := debugout_kret(
//    task_set_exception_ports( child, EXC_MASK_ALL, catchport, EXCEPTION_DEFAULT, 0),
    task_set_exception_ports( child, EXC_MASK_ALL, catchport, EXCEPTION_DEFAULT, 0),
      'task_set_exception_ports' );

end;

procedure TMachDbgProcess.Terminate;
begin
end;

function GetSigStr(sig: integer): String;
begin
  case sig of
    SIGHUP:  Result := 'SIGHUP';  { hangup  }
    SIGINT:  Result := 'SIGINT'; { interrupt  }
    SIGQUIT:  Result := 'SIGQUIT'; { quit  }
    SIGILL :  Result := 'SIGILL'; { illegal instruction (not reset when caught)  }
    SIGTRAP:  Result := 'SIGTRAP';  { trace trap (not reset when caught)  }
    SIGABRT:  Result := 'SIGABRT'; { abort()  }
    //SIGIOT:   Result := 'SIGIOT'; { compatibility  }
    SIGEMT:   Result := 'SIGEMT'; { EMT instruction  }
    SIGFPE:   Result := 'SIGFPE'; { floating point exception  }
    SIGKILL:  Result := 'SIGKILL'; { kill (cannot be caught or ignored)  }
    SIGBUS :  Result := 'SIGBUS'; { bus error  }
    SIGSEGV:  Result := 'SIGSEGV';  { segmentation violation  }
    SIGSYS :  Result := 'SIGSYS';  { bad argument to system call  }
    SIGPIPE:  Result := 'SIGPIPE';  { write on a pipe with no one to read it  }
    SIGALRM:  Result := 'SIGALRM';  { alarm clock  }
    SIGTERM:  Result := 'SIGTERM'; { software termination signal from kill  }
    SIGURG:   Result := 'SIGURG'; { urgent condition on IO channel  }
    SIGSTOP:  Result := 'SIGSTOP';  { sendable stop signal not from tty  }
    SIGTSTP:  Result := 'SIGTSTP';  { stop signal from tty  }
    SIGCONT:  Result := 'SIGCONT'; { continue a stopped process  }
    SIGCHLD:  Result := 'SIGCHLD'; { to parent on child stop or exit  }
    SIGTTIN:  Result := 'SIGTTIN';  { to readers pgrp upon background tty read  }
    SIGTTOU:  Result := 'SIGTTOU'; { like TTIN for output if (tp->t_local&LTOSTOP)  }
    SIGIO:    Result := 'SIGIO';  { input/output possible signal  }
    SIGXCPU:  Result := 'SIGXCPU';  { exceeded CPU time limit  }
    SIGXFSZ:  Result := 'SIGXFSZ';  { exceeded file size limit  }
    SIGVTALRM:  Result := 'SIGVTALRM';  { virtual time alarm  }
    SIGPROF:    Result := 'SIGPROF'  ;  { profiling time alarm  }
    SIGWINCH:   Result := 'SIGWINCH' ;  { window size changes  }
    SIGINFO:  Result := 'SIGINFO';  { information request  }
    SIGUSR1:  Result := 'SIGUSR1';   { user defined signal 1  }
    SIGUSR2:  Result := 'SIGUSR2';   { user defined signal 2  }
  end;
end;

(* exc_msg {
    mach_msg_header_t          Head;
    mach_msg_body_t            msgh_body; // start of kernel-processed data
    mach_msg_port_descriptor_t thread;    // victim thread
    mach_msg_port_descriptor_t task;      // end of kernel-processed data
    NDR_record_t               NDR;       // see osfmk/mach/ndr.h
    exception_type_t           exception;
    mach_msg_type_number_t     codeCnt;   // number of elements in code[]
    exception_data_t           code;      // an array of integer_t
    char                       pad[512];  // for avoiding MACH_MSG_RCV_TOO_LARGE
  }
*)

  
function TMachDbgProcess.WaitNextEvent(var Event: TDbgEvent): Boolean;
var
  buf    : array [0..1024] of byte;
  header : pmach_msg_header_t;
  ret    : mach_msg_return_t;
  msg    : pexception_raise_state_identity_t;
  st      : Integer;
  pidres  : pid_t;
begin
  Result := false;
//mach code

{  waited := false;
  writeln('waiting for the next event?');
  pidres := FpWaitpid(fchildpid, st, 0);
  writeln('pidres = ', pidres);
  Result := pidres >= 0;
  if not Result then begin
    writeln('waitpid = ', pidres);
    Exit;
  end;
  waited := true;}
  
  //debugout_kret(task_resume(fchildtask),'task_resume');
  FillChar(buf, sizeof(buf), 0);
  header := @buf[0];
  msg := pexception_raise_state_identity_t(header);
  writeln('mach_msg...');
  ret := mach_msg(
    header, 
    MACH_RCV_MSG or MACH_RCV_LARGE,
    0, 
    sizeof(buf),
    catchport,
    MACH_MSG_TIMEOUT_NONE,
    MACH_PORT_NULL
   );    
    
  writelN('msg size = ', header^.msgh_size );
  writelN('msg bits = ', header^.msgh_bits, ' ', IntToHex(header^.msgh_bits, 8) );
  
  writeln('exc task   = ', Integer(msg^.task.name));
  writeln('exc thread = ', Integer(msg^.thread.name));
  writeln('exc type   = ', msg^.exception);
  writeln('exc codeCnt= ', msg^.codeCnt);
  writeln('exc flavor = ', msg^.flavor);
  
    
  debugout_kret(ret, 'mach_msg');
  writelN('message recieved!');
end;

(*

// unix code
var
  st  : Integer;
  sig : integer;
  res : Integer;
begin
  Event.Debug := '';
  if fchildpid = 0 then begin
    Result := false;
    Exit;
  end;

  FillChar(Event, SizeOf(Event), 0);
  if waited then begin
    if waitedsig = 5 then
      sig := 0
    else
      sig := waitedsig;
    ptrace_cont(fchildpid, CONT_STOP_ADDR, sig)
  end;

  waited := false;
  res := FpWaitpid(fchildpid, st, 0);
  Result := res >= 0;
  if not Result then begin
    writeln('waitpid = ', res);
    Exit;
  end;
  waited := true;

  if res = 0 then
    Event.Kind := dek_ProcessTerminated
  else begin
    with Event do begin
      Debug := 'wait st = ' + IntToStr(st) + '; ';
      if WIFSTOPPED(st) then begin
        waitedsig := wstopsig(st);

        if waitedsig = SIGTRAP
          then Event.Kind := dek_BreakPoint
          else Event.Kind := dek_Other;

        Debug := Debug + ' stop sig = ' + IntToStr(waitedsig) + ' ' + GetSigStr(waitedsig) ;

      end else if wifsignaled(st) then Debug := Debug + ' term sig = ' + IntToStr(wtermsig(st))
      else if wifexited(st) then begin
        Event.Kind := dek_ProcessTerminated;
        Debug := Debug + ' exitcode = ' + IntToStr(wexitStatus(st))
      end else
        Debug := Debug + ' unknown state?';
      Debug := Debug + '; ';
    end;

    RecvMessage(catchport);
  end;
end;
*)

function TMachDbgProcess.StartProcess(const ACmdLine: String): Boolean;
begin
  Result := ForkAndRun(ACmdLine, fchildpid, fchildtask);
  SetupChildTask(fchildtask);
end;

function MachDebugProcessStart(const ACmdLine: String): TDbgProcess;
var
  machdbg : TMachDbgProcess;
begin
  machdbg := TMachDbgProcess.Create;
  if machdbg.StartProcess(ACmdLine) then
    Result := machdbg
  else begin
    machdbg.Free;
    Result := nil;
  end;
end;

procedure InitMachDebug;
begin
  DebugProcessStart := @MachDebugProcessStart;
end;

initialization
  InitMachDebug;

end.

