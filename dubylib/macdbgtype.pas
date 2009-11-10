unit macDbgType;

{$mode objfpc}{$H+}

interface

uses
  SysUtils,
  BaseUnix, Unix, machapi, mach_port, machexc,
  dbgTypes, macPtrace, macDbgProc;

type
  { TMachDbgProcess }

  TMachDbgProcess = class(TDbgTarget)
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
var
  r             : QWord;
  portarray     : mach_port_array_t;
  portcount     : integer;
  res           : kern_return_t;

  threads       : Pointer;
  threadscount  : mach_msg_type_number_t;

  pname         : mach_port_name_t;
begin

  task_for_pid(mach_task_self, fchildpid, pname);
  res := debugout_kret(task_threads(pname, threads, threadscount), 'task_threads');
  if res = KERN_SUCCESS then Result := threadScount
  else Result := 0;
{
  res := debugout_kret( mach_ports_lookup(fchildtask, portarray, portcount), 'mach_ports_lookup');
  if res = KERN_SUCCESS then writeln('ports = ', portcount);
}
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
  r : QWord;
begin
  writeln('child pid = ', fchildpid);

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
const
  HANDLER_COUNT = 64; // Could make this dynamic by looking for a result of MIG_ARRAY_TOO_LARGE
type
  TExceptionPorts = record
    maskCount : mach_msg_type_number_t;
    masks     : array [0..63] of exception_mask_t;
    handlers  : array [0..63] of exception_handler_t;
    behaviors : array [0..63] of exception_behavior_t;
    flavors   : array [0..63] of thread_state_flavor_t;
  end;
var
  ports : TExceptionPorts;
begin
  FillChar(ports, sizeof(ports), 0);
  ports.maskCount := 64;
  res := task_get_exception_ports(task_t(mach_task_self), EXC_MASK_ALL,
    @ports.masks,
    ports.maskCount, @ports.handlers, @ports.behaviors, @ports.flavors);
  writeln('ports.maskCount = ', ports.maskCount);

  writeln('[setting process exceptions handlers]');

  catchport := MachAllocPortForSelf;
  writelN('catch port = ', catchport);
  
  debugout_kret(
    mach_port_insert_right(task_t(mach_task_self), catchport, catchport,
       MACH_MSG_TYPE_MAKE_SEND),
    'mach_port_insert_right'
    );
  
  writeln('setting exception port');
  res := debugout_kret(
//  task_set_exception_ports( child, EXC_MASK_ALL, catchport, EXCEPTION_DEFAULT, 0),
    task_set_exception_ports( child, EXC_MASK_ALL, catchport, EXCEPTION_DEFAULT, 0),
      'task_set_exception_ports' );

  writeln('[setting process exceptions handlers. done]');
end;

procedure TMachDbgProcess.Terminate;
begin
end;

procedure DebugExptMsg(const msg: __Request__exception_raise_t);
var
  i : Integer;
begin
  writeln('msg.head.id:    ', msg.head.msgh_id);
  writeln('msg.body:       ', msg.msgh_body.msgh_descriptor_count);
  writelN('exception type: ', msg.exception, ' ', ExceptionType(msg.exception));
  writelN('exception at: ');
  writeln('  process: ', msg.task.name);
  writeln('  thread:  ', msg.thread.name);
  writeln('Code Count = ', msg.codeCnt);
  for i := 0 to msg.codeCnt - 1 do begin
    write(msg.code[i],' ');
    if msg.code[i] >= $10000 then write('uxcode = ', msg.code[i] -$10000);
    writeln(';');
  end;
  writeln;
end;


var
  hackthread : Integer;
  hacksig    : Integer;

function catch_exception_raise (exception_port, thread, task : mach_port_t;
	exception  : exception_type_t;
	code       : exception_data_t;
	codeCnt    : mach_msg_type_number_t
): kern_return_t; cdecl; [public];
var
  i  : Integer;
begin
  writeln('--- catch_exception_raise called! ---');
  writeln('exc_port  = ', exception_port);
  writeln('thread    = ', thread);
  writeln('exception = ', exception, ' ', ExceptionType(exception));
  writeln('codeCnt   = ', codeCnt);
  // for i := 0 to codeCnt - 1 do  writeln('code[',i,'] = ', code[i]);
  if (exception = EXC_SOFTWARE) and (CodeCnt = 2) and (code[0] = EXC_SOFT_SIGNAL) then begin
    writeln('unix signal! ', GetSigStr(code[1]));
    hackthread := thread;
    hacksig := code[1];
  end;
  Result := KERN_SUCCESS;
end;

function catch_exception_raise_state(
	exception_port    : mach_port_t;
	exception         : exception_type_t;
	const code        : exception_data_t;
	codeCnt           : mach_msg_type_number_t;
	flavor            : pinteger;
	const old_state   : thread_state_t;
	old_stateCnt      : mach_msg_type_number_t;
	new_state         : thread_state_t;
	var new_stateCnt  : mach_msg_type_number_t
): kern_return_t; cdecl; [public];
begin
  writeln('catch state! ');
  Result := KERN_SUCCESS;
end;

function catch_exception_raise_state_identity (
	exception_port    : mach_port_t;
	thread            : mach_port_t;
	task              : mach_port_t;
	exception         : exception_type_t;
	code              : exception_data_t;
	codeCnt           : mach_msg_type_number_t;
	flavor            : pinteger;
	old_state         : thread_state_t;
	old_stateCnt      : mach_msg_type_number_t;
	new_state         : thread_state_t;
	var new_stateCnt  : mach_msg_type_number_t
): kern_return_t; cdecl; [public];
begin
  writeln('catch identity! ');
  Result := KERN_SUCCESS;
end;


function TMachDbgProcess.WaitNextEvent(var Event: TDbgEvent): Boolean;
var
  buf     : array [0..4095] of byte;
  reqbuf  : array [0..4095] of byte;
  ret     : mach_msg_return_t;
  mreq    : PRequestUnion__exc_subsystem;
  mrep    : PReplyUnion__exc_subsystem;
  st      : Integer;
  pidres  : pid_t;
  b       : boolean_t;
  i       : Integer;
begin
  Result := True;
  //if waited then  ptrace_cont(fchildpid, 1, 0);

 { waited := false;
  writeln('**** waiting for the next event ***');
  pidres := FpWaitpid(fchildpid, st, 0);
  writeln('pidres   = ', pidres);
  writeln('childres = ', fchildpid);
  if pidres < 0 then begin
    writeln('error ', fpgeterrno);
    Exit;
  end;
  waited := true;}

  //writeln('st = ', st, '; if Stopped = ',WIFSTOPPED(st), '; WSTOPSIG = ', wstopsig(st)) ;
  //writeln('is Exited   = ', wifexited(st) ) ;
  //writeln('is Signaled = ', wifsignaled(st), ' WTERMSIG = ', wtermsig(st), ' ', GetSigStr(wtermsig(st)));

  //debugout_kret(task_resume(fchildtask),'task_resume');

  FillChar(reqbuf, sizeof(reqbuf), 0);
  mreq := @reqbuf;
  mreq^.req_raise.head.msgh_local_port := catchport;
  mreq^.req_raise.head.msgh_size := sizeof(reqbuf);

  writeln('recieving message from catchport...');
  // replacment for FPWaitPid
  ret := mach_msg(
    @reqbuf,
    MACH_RCV_MSG or MACH_RCV_LARGE,
    sizeof(reqbuf),
    sizeof(reqbuf),
    catchport,
    MACH_MSG_TIMEOUT_NONE,
    MACH_PORT_NULL
   );

  //debugout_kret(ret, 'mach_msg');

  writeln('id = ', mreq^.req_raise.Head.msgh_id);
  if mreq^.req_raise.Head.msgh_id = msgid_exception_raise then begin
    writeln('exception raise');
    DebugExptMsg(mreq^.req_raise);
  end;

  FillChar(buf, sizeof(buf), 0);
  try
    mrep := @buf;
    b := exc_server(@mreq^.req_raise.Head, @buf);
  except
    writeln('failure in exc_server');
    halt;
  end;
  writelN('exc_server = ', b);
  if not b then Exit;


  i := ptrace(PT_THUPDATE, fchildpid, hackthread, 0);
  writeln('ptrace = ', i);

  writeln('sending msg...');
  writeln('mrep = ', PtrUInt(mrep));
  writeln('mrep^.rep_raise.Head.msgh_size = ', mrep^.rep_raise.Head.msgh_size);
  writeln('mrep^.rep_raise.Head.msgh_id   = ', mrep^.rep_raise.Head.msgh_id);
  writeln('mrep^.rep_raise.RetCode        = ', mrep^.rep_raise.RetCode);
  ret := mach_msg(@buf,        // message
                  MACH_SEND_MSG,           // options
                  mrep^.rep_raise.Head.msgh_size, // send size
                  0,                       // receive limit (irrelevant here)
                  MACH_PORT_NULL,          // port for receiving (none)
                  MACH_MSG_TIMEOUT_NONE,   // no timeout
                  MACH_PORT_NULL);         // notify port (we don't want one)
  debugout_kret(ret, 'mach_msg');
  if ret = KERN_SUCCESS then writeln('message sent');



  Event.Addr := 0;
  Event.Debug := '';
  Event.Kind := dek_Other;
  Event.Thread := nil;
  Result := true;
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

function MachDebugProcessStart(const ACmdLine: String): TDbgTarget;
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

