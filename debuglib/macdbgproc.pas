{
    fpDebug  -  A debugger for the Free Pascal Compiler.

    Copyright (c) 2012 by Graeme Geldenhuys.

    See the file LICENSE.txt, included in this distribution,
    for details about redistributing fpDebug.

    Description:
      .
}
unit macDbgProc;

//todo: better ForkAndRun code

{$mode objfpc}{$H+}

interface

uses
  BaseUnix, Unix,
  Classes, SysUtils,
  machapi, machexc, mach_port, macPtrace, macDbgUtils;

// == utility functions ==

// running the process
function ForkAndRun(const CommandLine: String; var ChildId: TPid; var ChildTask: mach_port_t; SuspendChild: Boolean): Boolean;

// the function sleeps for TimeOut millisecconds for the child to come
// into suspended state. negative TimeOut value means - inifinite
// WARNING: if child is not to suspend itself the function might fail!
function WaitForChildSuspend(atask: mach_port_t; TimeOut: Integer): Boolean;

// returns true if task task is correct and the state can be read
// returns false overwise.
function isTaskSuspended(atask: mach_port_t; var Suspended: Boolean): Boolean;

// mach-port allocation routines
function MachAllocPortForSelf: mach_port_name_t;
function MachAllocPortForTask(atask: task_t): mach_port_name_t;

// reads tasks memory

function ReadTaskMem(task: task_t; const Offset, Size: Qword; var data: array of byte; var BytesRead: Qword): kern_return_t;
function WriteTaskMem(task: task_t; const Offset, Size: Qword; const data: array of byte; var BytesWritten: Qword): kern_return_t;

// == debugging functions ==

// if err <> 0, writes to stdout comment and error. function returns err value
function debugout_kret(err: kern_return_t; const comment: AnsiString): kern_return_t;

function GetSigStr(sig: Integer): string;
function kern_err_str(err: Integer): string;
function ExcMaskStr(excMask: Integer): string;

procedure PrintTaskBasicInfo32(task: task_t);
procedure PrintTaskThreadInfo32(task: task_t);
procedure PrintTaskAllInfo(task: task_t);

procedure PrintThreadBasicInfo(thread: thread_act_t);
procedure PrintTaskThreads(task: task_t);

function PolicyStr(p: policy_t): string;
function MachTimeStr(t: time_value): string;
function ThRunStateStr(run_state: Integer): string;
function ThFlagStr(flags: Integer): string;


type
  // the structure conatains structure of all possible messages
  // that debugger should handle
  TRecvDebugMessage = record
  // following messages are expected by the debugger
  case byte of
    0:   (head                : mach_msg_header_t); // each message starts with the header
    1:   (exc_raise           : __Request__exception_raise_t);
    2:   (exc_raise_state     : __Request__exception_raise_state_t);
  	3:   (exc_raise_identity  : __Request__exception_raise_state_identity_t);
    255: (raw: array [0..1024*2-1] of byte); // for the bigger size; remove?
  end;

// waits for the messages for TimeOut mls (infinite if negative)
// returns True if success, False overwise
function WaitForDebugMsg(port: mach_port_t; var Msg: TRecvDebugMessage; TimeOut: Integer): Boolean;
procedure HandleException(const Msg: TRecvDebugMessage);


// returns the first thread from task's thread list or zero, if threads cannot be read
function GetAnyTaskThread(task: task_t): thread_t;

implementation

function GetAnyTaskThread(task: task_t): thread_t;
var
  cnt   : Integer;
  list  : thread_act_array_t;
begin
  list:=nil;
  cnt:=0;
  if (task_threads(task, list, cnt)=KERN_SUCCESS) and (cnt>0) then
    Result:=list^[0]
  else
    Result:=0;
end;

function WaitForDebugMsg(port: mach_port_t; var Msg: TRecvDebugMessage; TimeOut: Integer): Boolean;
var
  flags : Integer;
  tm    : mach_msg_timeout_t;
begin
  FillChar(Msg.head, sizeof(Msg.head), 0);
  Msg.head.msgh_local_port:=port;
  Msg.head.msgh_size:=sizeof(Msg);

  flags:=MACH_RCV_MSG or MACH_RCV_LARGE;
  if TimeOut>=0 then begin
    flags:=flags or MACH_SEND_TIMEOUT;
    tm:=TimeOut;
  end else
    tm:=MACH_MSG_TIMEOUT_NONE;
  Result:=mach_msg(@msg.head, flags, 0, sizeof(Msg), port, tm, MACH_PORT_NULL)=KERN_SUCCESS;
end;

procedure HandleException(const Msg: TRecvDebugMessage);
begin
  case Msg.head.msgh_id of
    msgid_exception_raise,
    msgid_exception_raise_state,
    msgid_exception_raise_state_identity: {don't exit};
  else
    Exit;
  end;

end;

function MachTimeStr(t: time_value): string;
begin
  Result:=IntToStr(t.seconds) + ' : ' + IntToStr(t.microseconds)
end;

function PolicyStr(p: policy_t): string;
begin
  if p = POLICY_NULL then
    Result:='POLICY_NULL'
  else begin
    Result:='';
    if p and POLICYCLASS_FIXEDPRI > 0 then
      Result:=Result+ 'POLICYCLASS_FIXEDPRI '
    else if p and POLICY_RR > 0 then
      Result:=Result+ 'POLICY_RR '
    else if p and POLICY_FIFO > 0 then
      Result:=Result+ 'POLICY_FIFO ';
    if p and POLICY_TIMESHARE > 0 then
      Result:=Result + 'POLICY_TIMESHARE ';
  end;
end;

procedure PrintTaskBasicInfo32(task: task_t);
var
  ret   : kern_return_t;
  info  : task_basic_info;
  cnt   : Integer;
begin
  FillChar(info, sizeof(info), 0);
  cnt := TASK_BASIC_INFO_32_COUNT;

  ret:=task_info(task, TASK_BASIC_INFO_32, @info, cnt);
  if ret <> KERN_SUCCESS then begin
    writeln('  failed: ', ret,' ', HexStr(ret,8));
    exit;
  end;

  writeln('  suspend count:   ', info.suspend_count);
  writeln('  virtual pages:   ', info.virtual_size);
  writeln('  resident pages:  ', info.resident_size);
  writeln('  user time (sec): ', MachTimeStr(info.user_time));
  writeln('  sys time (sec):  ', MachTimeStr(info.system_time));
  writeln('  policy: ', PolicyStr(info.policy));
end;

procedure PrintTaskThreadInfo32(task: task_t);
var
  ret   : kern_return_t;
  info  : task_thread_times_info_data_t;
  cnt   : Integer;
begin
  FillChar(info, sizeof(info), 0);
  cnt := TASK_THREAD_TIMES_INFO_COUNT;

  ret:=task_info(task, TASK_THREAD_TIMES_INFO, @info, cnt);
  if ret <> KERN_SUCCESS then begin
    writeln('  failed: ', HexStr(ret,8));
    Exit;
  end;

  writeln('  user time (sec): ', MachTimeStr(info.user_time));
  writeln('  sys time (sec):  ', MachTimeStr(info.system_time));
end;

procedure PrintTaskAllInfo(task: task_t);
begin
  writeln('task info: ', PtrUInt(task));
  writeln('basic info:');
  PrintTaskBasicInfo32(task);
  writeln('live thread time info:');
  PrintTaskThreadInfo32(task);
  writeln;
end;

function ThRunStateStr(run_state: Integer): string;
const
  ThStateStr : array [TH_STATE_RUNNING..TH_STATE_HALTED] of String = (
    'TH_STATE_RUNNING', 'TH_STATE_STOPPED', 'TH_STATE_WAITING',
    'TH_STATE_UNINTERRUPTIBLE', 'TH_STATE_HALTED'
  );
begin
  case run_state of
    TH_STATE_RUNNING..TH_STATE_HALTED:
      Result:=ThStateStr[run_state];
  else
    Result:='TH_OTHER! (error?) ' + IntToStr(run_state);
  end;
end;

function ThFlagStr(flags: Integer): string;
begin
  Result:='';
  if flags and TH_FLAGS_SWAPPED > 0 then Result:=Result + 'TH_FLAGS_SWAPPED ';
  if flags and TH_FLAGS_IDLE > 0 then Result:=Result+ 'TH_FLAGS_IDLE ';
end;

procedure PrintThreadBasicInfo(thread: thread_act_t);
var
  info  : thread_basic_info_data_t;
  cnt   : Integer;
  ret   : kern_return_t;
begin
  FillChar(info, sizeof(info), 0);
  cnt:=THREAD_BASIC_INFO_COUNT;
  ret := thread_info(thread, THREAD_BASIC_INFO, @info, cnt);
  if ret<>KERN_SUCCESS then begin
    writeln('  failed ', HexStr(ret,8));
    Exit;
  end;

  writeln('  user time (sec): ', MachTimeStr(info.user_time));
  writeln('  sys time (sec):  ', MachTimeStr(info.system_time)); { system run time }
  writeln('  cpu usage:       ', info.cpu_usage);
	writeln('  policy:          ', PolicyStr(info.policy));
  writeln('  run state:       ', ThRunStateStr(info.run_state));
  writeln('  flags:           ', ThFlagStr(info.flags));
  writeln('  suspend count:   ', info.suspend_count);
  writeln('  sleep time: '     , info.sleep_time);

end;

procedure PrintTaskThreads(task: task_t);
var
  list  : thread_act_array_t;
  cnt   : Integer;
  ret   : kern_return_t;
  i     : Integer;
begin
  list:=nil;
  ret := task_threads(task, list, cnt);

  if ret<>KERN_SUCCESS then begin
    writeln('  failed ', HexStr(ret, 8));
    Exit;
  end;

  writeln('task ', PtrUInt(task),', threads count = ', cnt);
  for i:=0 to cnt-1 do begin
    writeln('thread: ', list^[i]);
    PrintThreadBasicInfo(list^[i]);
  end;
end;

function ForkAndRun(const CommandLine: String; var ChildId: TPid; var ChildTask: mach_port_t; SuspendChild: Boolean): Boolean;
var
  res   : Integer;
  pname : mach_port_name_t;
  kret  : kern_return_t;
begin
  Result := false;
  childid := FpFork;
  if childid < 0 then Exit;

  if childid = 0 then begin
    // todo: better ForkAndRun code
    // suspending self. the debugger must unsuspend, after installed exception ports.
    if SuspendChild then task_suspend(mach_task_self);

    ptraceme;
    ptrace_sig_as_exc;

    res := FpExecV(CommandLine, nil);
    if res < 0 then begin
      writeln('failed to run: ', CommandLine);
      Halt;
    end;

  end else begin
    pname := 0;
    kret := task_for_pid(mach_task_self, ChildId, pname);
    if kret=KERN_SUCCESS then
      ChildTask:=mach_port_t(pname)
    else
      ChildTask:=0;
    Result := true;
  end;
end;


function MachAllocPortForSelf: mach_port_name_t;
begin
  Result := MachAllocPortForTask( task_t(mach_task_self) );
end;

function MachAllocPortForTask(atask: task_t): mach_port_name_t;
var
  portname: mach_port_name_t;
begin
  if debugout_kret(
    mach_port_allocate(ipc_space_t(atask), MACH_PORT_RIGHT_RECEIVE, portname),'mach_port_allocate') = 0 then
    Result := portname
  else
    Result := 0;
end;

function ReadTaskMem(task: task_t; const Offset, Size: Qword; var data: array of byte; var BytesRead: Qword): kern_return_t;
begin
  Result:=mach_vm_read_overwrite(task, Offset, Size, mach_vm_address_t(@data[0]), BytesRead);
end;

function WriteTaskMem(task: task_t; const Offset, Size: Qword; const data: array of byte; var BytesWritten: Qword): kern_return_t;
var
  d   : LongWord;
  sz  : mach_vm_size_t;
begin
  if Size>0 then begin
    BytesWritten:=Size;
    mach_vm_read_overwrite(task, Offset, sizeof(d), mach_vm_address_t(@d), sz);
    writelN('d = ', HexStr(d, 8));
    Move(data, d, 1);
    //Result:=mach_vm_write(task, Offset, mach_vm_address_t(@data[0]), BytesWritten);
    BytesWritten:=sizeoF(d);
    writelN('d = ', HexStr(d, 8));
    writeln('bw = ', BytesWritten);
    Result:=mach_vm_write(task, Offset, mach_vm_address_t(@d), BytesWritten);
    writelN('mach_vm_write = ', Result);
  end else begin
    Result:=0;
    BytesWritten:=0;
  end;
end;

function kern_err_str(err: INteger): string;
begin
  case err of
    KERN_SUCCESS                       :Result:='KERN_SUCCESS';
    KERN_INVALID_ADDRESS               :Result:='KERN_INVALID_ADDRESS';
    KERN_PROTECTION_FAILURE            :Result:='KERN_PROTECTION_FAILURE';
    KERN_NO_SPACE                      :Result:='KERN_NO_SPACE';
    KERN_INVALID_ARGUMENT              :Result:='KERN_INVALID_ARGUMENT';
    KERN_FAILURE                       :Result:='KERN_FAILURE';
    KERN_RESOURCE_SHORTAGE             :Result:='KERN_RESOURCE_SHORTAGE';
    KERN_NOT_RECEIVER                  :Result:='KERN_NOT_RECEIVER';
    KERN_NO_ACCESS                     :Result:='KERN_NO_ACCESS';
    KERN_MEMORY_FAILURE                :Result:='KERN_MEMORY_FAILURE';
    KERN_MEMORY_ERROR                  :Result:='KERN_MEMORY_ERROR';
   	KERN_ALREADY_IN_SET                :Result:='KERN_ALREADY_IN_SET';
    KERN_NOT_IN_SET                    :Result:='KERN_NOT_IN_SET';
    KERN_NAME_EXISTS                   :Result:='KERN_NAME_EXISTS';
    KERN_ABORTED                       :Result:='KERN_ABORTED';
    KERN_INVALID_NAME                  :Result:='KERN_INVALID_NAME';
   	KERN_INVALID_TASK                  :Result:='KERN_INVALID_TASK';
    KERN_INVALID_RIGHT                 :Result:='KERN_INVALID_RIGHT';
    KERN_INVALID_VALUE                 :Result:='KERN_INVALID_VALUE';
   	KERN_UREFS_OVERFLOW                :Result:='KERN_UREFS_OVERFLOW';
   	KERN_INVALID_CAPABILITY            :Result:='KERN_INVALID_CAPABILITY';
    KERN_RIGHT_EXISTS                  :Result:='KERN_RIGHT_EXISTS';
   	KERN_INVALID_HOST                  :Result:='KERN_INVALID_HOST';
    KERN_MEMORY_PRESENT                :Result:='KERN_MEMORY_PRESENT';
    KERN_MEMORY_DATA_MOVED             :Result:='KERN_MEMORY_DATA_MOVED';
    KERN_MEMORY_RESTART_COPY           :Result:='KERN_MEMORY_RESTART_COPY';
    KERN_INVALID_PROCESSOR_SET         :Result:='KERN_INVALID_PROCESSOR_SET';
    KERN_POLICY_LIMIT                  :Result:='KERN_POLICY_LIMIT';
    KERN_INVALID_POLICY                :Result:='KERN_INVALID_POLICY';
    KERN_INVALID_OBJECT                :Result:='KERN_INVALID_OBJECT';
    KERN_ALREADY_WAITING               :Result:='KERN_ALREADY_WAITING';
    KERN_DEFAULT_SET                   :Result:='KERN_DEFAULT_SET';
    KERN_EXCEPTION_PROTECTED           :Result:='KERN_EXCEPTION_PROTECTED';
    KERN_INVALID_LEDGER                :Result:='KERN_INVALID_LEDGER';
    KERN_INVALID_MEMORY_CONTROL        :Result:='KERN_INVALID_MEMORY_CONTROL';
    KERN_INVALID_SECURITY              :Result:='KERN_INVALID_SECURITY';
    KERN_NOT_DEPRESSED                 :Result:='KERN_NOT_DEPRESSED';
    KERN_TERMINATED                    :Result:='KERN_TERMINATED';
    KERN_LOCK_SET_DESTROYED            :Result:='KERN_LOCK_SET_DESTROYED';
    KERN_LOCK_UNSTABLE                 :Result:='KERN_LOCK_UNSTABLE';
    KERN_LOCK_OWNED                    :Result:='KERN_LOCK_OWNED';
    KERN_LOCK_OWNED_SELF               :Result:='KERN_LOCK_OWNED_SELF';
    KERN_SEMAPHORE_DESTROYED           :Result:='KERN_SEMAPHORE_DESTROYED';
    KERN_RPC_SERVER_TERMINATED         :Result:='KERN_RPC_SERVER_TERMINATED';
    KERN_RPC_TERMINATE_ORPHAN          :Result:='KERN_RPC_TERMINATE_ORPHAN';
    KERN_RPC_CONTINUE_ORPHAN           :Result:='KERN_RPC_CONTINUE_ORPHAN';
    KERN_NOT_SUPPORTED                 :Result:='KERN_NOT_SUPPORTED';
    KERN_NODE_DOWN                     :Result:='KERN_NODE_DOWN';
    KERN_NOT_WAITING                   :Result:='KERN_NOT_WAITING';
    KERN_OPERATION_TIMED_OUT           :Result:='KERN_OPERATION_TIMED_OUT';
    MACH_MSG_IPC_SPACE                 :Result:='MACH_MSG_IPC_SPACE';
    MACH_MSG_VM_SPACE                  :Result:='MACH_MSG_VM_SPACE';
    MACH_MSG_IPC_KERNEL                :Result:='MACH_MSG_IPC_KERNEL';
    MACH_MSG_VM_KERNEL                 :Result:='MACH_MSG_VM_KERNEL';
    MACH_SEND_IN_PROGRESS              :Result:='MACH_SEND_IN_PROGRESS';
    MACH_SEND_INVALID_DATA             :Result:='MACH_SEND_INVALID_DATA';
    MACH_SEND_INVALID_DEST             :Result:='MACH_SEND_INVALID_DEST';
    MACH_SEND_TIMED_OUT                :Result:='MACH_SEND_TIMED_OUT';
    MACH_SEND_INTERRUPTED              :Result:='MACH_SEND_INTERRUPTED';
    MACH_SEND_MSG_TOO_SMALL            :Result:='MACH_SEND_MSG_TOO_SMALL';
    MACH_SEND_INVALID_REPLY            :Result:='MACH_SEND_INVALID_REPLY';
    MACH_SEND_INVALID_RIGHT            :Result:='MACH_SEND_INVALID_RIGHT';
    MACH_SEND_INVALID_NOTIFY           :Result:='MACH_SEND_INVALID_NOTIFY';
    MACH_SEND_INVALID_MEMORY           :Result:='MACH_SEND_INVALID_MEMORY';
    MACH_SEND_NO_BUFFER                :Result:='MACH_SEND_NO_BUFFER';
    MACH_SEND_TOO_LARGE                :Result:='MACH_SEND_TOO_LARGE';
    MACH_SEND_INVALID_TYPE             :Result:='MACH_SEND_INVALID_TYPE';
    MACH_SEND_INVALID_HEADER           :Result:='MACH_SEND_INVALID_HEADER';
    MACH_SEND_INVALID_TRAILER          :Result:='MACH_SEND_INVALID_TRAILER';
    MACH_SEND_INVALID_RT_OOL_SIZE      :Result:='MACH_SEND_INVALID_RT_OOL_SIZE';
    MACH_RCV_IN_PROGRESS               :Result:='MACH_RCV_IN_PROGRESS';
    MACH_RCV_INVALID_NAME              :Result:='MACH_RCV_INVALID_NAME';
    MACH_RCV_TIMED_OUT                 :Result:='MACH_RCV_TIMED_OUT';
    MACH_RCV_TOO_LARGE                 :Result:='MACH_RCV_TOO_LARGE';
    MACH_RCV_INTERRUPTED               :Result:='MACH_RCV_INTERRUPTED';
    MACH_RCV_PORT_CHANGED              :Result:='MACH_RCV_PORT_CHANGED';
    MACH_RCV_INVALID_NOTIFY            :Result:='MACH_RCV_INVALID_NOTIFY';
    MACH_RCV_INVALID_DATA              :Result:='MACH_RCV_INVALID_DATA';
    MACH_RCV_PORT_DIED                 :Result:='MACH_RCV_PORT_DIED';
    MACH_RCV_IN_SET                    :Result:='MACH_RCV_IN_SET';
    MACH_RCV_HEADER_ERROR              :Result:='MACH_RCV_HEADER_ERROR';
    MACH_RCV_BODY_ERROR                :Result:='MACH_RCV_BODY_ERROR';
    MACH_RCV_INVALID_TYPE              :Result:='MACH_RCV_INVALID_TYPE';
    MACH_RCV_SCATTER_SMALL             :Result:='MACH_RCV_SCATTER_SMALL';
    MACH_RCV_INVALID_TRAILER           :Result:='MACH_RCV_INVALID_TRAILER';
    MACH_RCV_IN_PROGRESS_TIMED         :Result:='MACH_RCV_IN_PROGRESS_TIMED';
  else
    Result := 'unknown '+ INtToHex(err, 8);
  end;
end;

function debugout_kret(err: kern_return_t; const comment: AnsiString): kern_return_t;
begin
  if err <> 0 then writeln(comment, ': ', err, ' ', IntToHex(err, 8), ' ', kern_err_str(err));
  Result := err;
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

function ExcMaskStr(excMask: Integer): string;
var
  i : integer;
begin
  Result:='';
  for i:=EXC_BAD_ACCESS to EXC_CRASH do
    if (excMask and (1 shl i))>0 then
      Result:=Result+debugExceptionType(i)+' ';
end;

function isTaskSuspended(atask: mach_port_t; var Suspended: Boolean): Boolean;
var
  ret   : kern_return_t;
  info  : task_basic_info;
  cnt   : Integer;
begin
  Result:=false;
  FillChar(info, sizeof(info), 0);
  cnt := TASK_BASIC_INFO_32_COUNT;
  ret:=task_info(atask, TASK_BASIC_INFO_32, @info, cnt);
  Result:=ret=KERN_SUCCESS;
  if Result then Suspended:=info.suspend_count>0;
end;

function GetMls: Integer;
var
  p : timeval;
begin
  FillChar(p, sizeof(p),0);
  fpgettimeofday(@p, nil);
  Result:=(p.tv_sec*1000)+(p.tv_usec div 10000000);
end;

function WaitForChildSuspend(atask: mach_port_t; TimeOut: Integer): Boolean;
var
  i     : Integer;
  susp  : Boolean;
begin
  Result:=False;
  susp:=False;
  if TimeOut>0 then begin
    i:=GetMls;
    repeat
      if not isTaskSuspended(atask, susp) then
        Exit;
      if susp then Sleep(100);
    until (not susp) or (GetMls-i>=TimeOut);

    if susp then isTaskSuspended(atask, susp);
    Result:=susp;

  end else
    while True do begin
      if not isTaskSuspended(atask, susp) then Exit;
      if susp then begin
        Result:=True;
        Exit;
      end;
      Sleep(100);
    end;
end;

end.

