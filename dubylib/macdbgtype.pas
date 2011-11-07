unit macDbgType;

{$mode objfpc}{$H+}

interface

uses
  SysUtils,
  BaseUnix, Unix, machapi, mach_port, machexc,
  dbgTypes, macPtrace, macDbgProc, macDbgUtils;

type
  TMacDebugState = (mdsStartProc, mdsStartThread, mdsNormal, mdsTerminated);

  { TMacDbgTarget }

  TMacDbgTarget = class(TDbgTarget)
  private
    fchildpid   : TPid;
    fchildtask  : mach_port_t;

    catchport   : mach_port_t;
  protected
    fState      : TMacDebugState;
    waitedsig   : Integer;

    recvMsg     : TRecvDebugMessage;
    eventTask   : task_t;
    eventThread : thread_t;

    procedure SetupChildTask(child: task_t);

    procedure HandleMachMsg;
  public
    procedure Terminate; override;
    function WaitNextEvent(var Event: TDbgEvent): Boolean; override;

    function GetThreadsCount(AProcess: TDbgProcessID): Integer; override;
    function GetThreadID(AProcess: TDbgProcessID; AIndex: Integer): TDbgThreadID; override;
    function GetThreadRegs(AProcess: TDbgProcessID; ThreadID: TDbgThreadID; Regs: TDbgDataList): Boolean; override;
    function SetThreadRegs(AProcess: TDbgProcessID; ThreadID: TDbgThreadID; Registers: TDbgDataList): Boolean; override;

    function SetSingleStep(AProcess: TDbgProcessID; ThreadID: TDbgThreadID): Boolean; override;

    function ReadMem(AProcess: TDbgProcessID; Offset: TDbgPtr; Count: Integer; var Data: array of byte): Integer; override;
    function WriteMem(AProcess: TDbgProcessID; Offset: TDbgPtr; Count: Integer; const Data: array of byte): Integer; override;

    // MacOSX specific function
    function StartProcess(const ACmdLine: String): Boolean;
  end;

var
  CanDebug : Boolean = False;

implementation

var
  catchedthread : Integer;

function TMacDbgTarget.GetThreadsCount(AProcess: TDbgProcessID): Integer;
var
  //r             : QWord;
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

function TMacDbgTarget.GetThreadID(AProcess: TDbgProcessID; AIndex: Integer): TDbgThreadID;
begin
  Result:=nil;
end;

function TMacDbgTarget.GetThreadRegs(AProcess: TDbgProcessID; ThreadID: TDbgThreadID; Regs: TDbgDataList): Boolean;
begin
  Result:=false;
end;

function TMacDbgTarget.SetThreadRegs(AProcess: TDbgProcessID;
  ThreadID: TDbgThreadID; Registers: TDbgDataList): Boolean;
begin
  Result:=False;
end;

function TMacDbgTarget.SetSingleStep(AProcess: TDbgProcessID; ThreadID: TDbgThreadID): Boolean;
begin
  Result := false;
end;

function TMacDbgTarget.ReadMem(AProcess: TDbgProcessID; Offset: TDbgPtr; Count: Integer; var Data: array of byte): Integer;
var
  r   : QWord;
  res : kern_return_t;
begin
  res:=ReadTaskMem(fchildtask, Offset, Count, Data, r);
  if res <> 0 then begin
    Result := -1
  end else
    Result := r;
end;

function TMacDbgTarget.WriteMem(AProcess: TDbgProcessID; Offset: TDbgPtr; Count: Integer; const Data: array of byte): Integer;
var
  res : kern_return_t;
  r   : QWord;
begin
  writeln('proc = ', PtrUInt(AProcess));
  res:=WriteTaskMem(AProcess, Offset, Count, Data, r);
  writeln('WriteTaskMem = ', res,' ',hexStr(res, sizeof(res)*2) );
  writeln('r =', r);
  if res<>0 then
    Result := -1
  else
    Result := r;
end;

procedure TMacDbgTarget.SetupChildTask(child: task_t);
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

  writeln('[mach: setting process exceptions handlers]');

  catchport := MachAllocPortForSelf;
  writelN('[mach: catch port = ', catchport,']');

  debugout_kret(
    mach_port_insert_right(task_t(mach_task_self), catchport, catchport,
       MACH_MSG_TYPE_MAKE_SEND),
    'mach_port_insert_right'
    );
  
  writeln('[mach: setting exception ports]');
  res := debugout_kret(
    task_set_exception_ports( child, EXC_MASK_ALL, catchport, EXCEPTION_DEFAULT, 0),
      'task_set_exception_ports' );

  writeln('[mach: setting process exceptions handlers. done]');
end;

procedure TMacDbgTarget.HandleMachMsg;
var
  buf     : array [0..4095] of byte;
  reqbuf  : array [0..4095] of byte;
  ret     : mach_msg_return_t;
  mrep    : PReplyUnion__exc_subsystem;
  b       : boolean_t;
  i       : Integer;
  res : Boolean;
begin
  FillChar(buf, sizeof(buf), 0);
  try
    mrep := @buf;
    b := exc_server(@recvmsg.exc_raise, @buf);
  except
    writeln('failure in exc_server');
    halt;
  end;
  writelN('exc_server = ', b);
  if not b then Exit;


  i := ptrace(PT_THUPDATE, fchildpid, catchedthread, 0);
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
end;

procedure TMacDbgTarget.Terminate;
begin
end;

function catch_exception_raise (exception_port, thread, task : mach_port_t;
	exception  : exception_type_t;
	code       : exception_data_t;
	codeCnt    : mach_msg_type_number_t
): kern_return_t; cdecl; [public]; //alias: '_catch_exception_raise';
begin
  writeln('--- catch_exception_raise called! ---');
  writeln('  exc_port  = ', exception_port);
  writeln('  thread    = ', thread);
  writeln('  exception = ', exception, ' ', debugExceptionType(exception));
  writeln('  code      = ', code^, ' ptr = ', PtrUInt(code), ' ', hexStr(code^, 8));
  writeln('  codeCnt   = ', codeCnt);
  // for i := 0 to codeCnt - 1 do  writeln('code[',i,'] = ', code[i]);
  if (exception = EXC_SOFTWARE) and (CodeCnt = 2) and (code[0] = EXC_SOFT_SIGNAL) then begin
    writeln('unix signal! ', GetSigStr(code[1]));
    catchedthread := thread;
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


//todo: WaitNextEvent is quite ugly in its implementation, please change!

function TMacDbgTarget.WaitNextEvent(var Event: TDbgEvent): Boolean;
begin
  //todo!!!!
  eventTask:=fchildtask;
  eventThread:=GetAnyTaskThread(eventTask);

  if fState=mdsTerminated then begin
    Result:=False;
    Exit;
  end;
  writeln('[mach: recieving message from catchport...]');

  case fState of
    mdsStartThread: begin
      Event.Kind:=dek_ThreadStart;
      Event.Addr:=0;
      Result:=True;
      fState:=mdsNormal;
      Exit;
    end;
    mdsNormal: HandleMachMsg;
  end;


  Result:=WaitForDebugMsg(catchport, recvMsg, -1);
  if not Result then begin
    // terminated?
    fState:=mdsTerminated;
    Exit;
  end;

  Event.Process:=TDbgProcessID(eventTask);
  Event.Thread:=TDbgThreadID(eventThread);

  case fState of
    mdsStartProc: begin
      Event.Kind:=dek_ProcessStart;
      Event.Addr:=0;
      fState:=mdsStartThread;
    end;
  else
    Event.Kind:=dek_Other;
  end;
  Exit;

  //debugout_kret(ret, 'mach_msg');

  writeln('id = ', recvmsg.head.msgh_id);
  if recvmsg.head.msgh_id = msgid_exception_raise then begin
    writeln('exception raise');
    debugExcReqRaise(recvmsg.exc_raise);
  end;

end;

function TMacDbgTarget.StartProcess(const ACmdLine: String): Boolean;
var
  susp: Boolean;
begin
  Result := ForkAndRun(ACmdLine, fchildpid, fchildtask, True);
  //setting up the child's exception port
  writeln('[mach: installing exception ports]');
  SetupChildTask(fchildtask);

  // waiting for the child to suspend itself
  writeln('[mach: waiting the child to suspend]');
  if not WaitForChildSuspend(fchildtask, -1) then begin
    writeln('cannot start the process');
    Result:=False;
    Exit;
  end;

  writeln('[mach: the next message should "initial" break]');
  Result:=task_resume(fchildtask)=KERN_SUCCESS;



  fState:=mdsStartProc;
end;

function MachDebugProcessStart(const ACmdLine: UnicodeString): TDbgTarget;
var
  machdbg : TMacDbgTarget;
begin
  machdbg := TMacDbgTarget.Create;
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
  CanDebug:=(@catch_exception_raise<>nil) and
            (@catch_exception_raise_state<>nil) and
            (@catch_exception_raise_state_identity<>nil);
  // Smartlinking protection. (functions must not be cut off)
end;

initialization
  InitMachDebug;

end.


