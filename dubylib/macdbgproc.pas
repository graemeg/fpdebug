unit macDbgProc; 

{$mode objfpc}{$H+}

interface

uses
  BaseUnix,
  Classes, SysUtils,
  machapi,  machexc, mach_port;
  
// if err <> 0, writes to stdout comment and error.
// function returns err value
function debugout_kret(err: kern_return_t; const comment: AnsiString): kern_return_t;

// mach-port allocation routines
function MachAllocPortForSelf: mach_port_name_t;
function MachAllocPortForTask(atask: task_t): mach_port_name_t;

//function MachRecvMessage(port: mach_port_name_t; var buf: array of byte; bufSize: Integer): Integer;

// reads tasks memory
function ReadTaskMem(task: task_t; const Offset, Size: Qword; var data: array of byte; var BytesRead: Qword): kern_return_t;

// ???
procedure RecvMessage(port: port_t);

function GetSigStr(sig: Integer): String;
function kern_err_str(err: Integer): string;
function ExceptionType(exc_type: Integer): String;

implementation

function MachAllocPortForSelf: mach_port_name_t;
begin
  Result := MachAllocPortForTask( task_t(mach_task_self) );
end;

function MachAllocPortForTask(atask: task_t): mach_port_name_t;
var
  portname: mach_port_name_t;
begin
  if debugout_kret(
    mach_port_allocate( ipc_space_t(atask), MACH_PORT_RIGHT_RECEIVE, portname)
    ,'mach_port_allocate') = 0 then
    Result := portname
  else
    Result := 0;
end;

function ReadTaskMem(task: task_t; const Offset, Size: Qword; var data: array of byte; var BytesRead: Qword): kern_return_t;
begin
  Result := 0;
  BytesRead := 0;
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

procedure RecvMessage(port: port_t);
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

function ExceptionType(exc_type: Integer): String;
begin
  case exc_type of
    EXC_BAD_ACCESS            :Result := 'EXC_BAD_ACCESS';
    EXC_BAD_INSTRUCTION       :Result := 'EXC_BAD_INSTRUCTION';
    EXC_ARITHMETIC            :Result := 'EXC_ARITHMETIC';
    EXC_EMULATION             :Result := 'EXC_EMULATION';
    EXC_SOFTWARE              :Result := 'EXC_SOFTWARE';
    EXC_BREAKPOINT            :Result := 'EXC_BREAKPOINT';
    EXC_SYSCALL               :Result := 'EXC_SYSCALL';
    EXC_MACH_SYSCALL          :Result := 'EXC_MACH_SYSCALL';
    EXC_RPC_ALERT             :Result := 'EXC_RPC_ALERT';
    EXC_CRASH                 :Result := 'EXC_CRASH';
    {EXCEPTION_DEFAULT         :Result := 'EXCEPTION_DEFAULT';
    EXCEPTION_STATE           :Result := 'EXCEPTION_STATE';
    EXCEPTION_STATE_IDENTITY  :Result := 'EXCEPTION_STATE_IDENTITY';}
  else
    Result := 'Unknown ' + IntToHex(exc_type, 8);
  end;

end;


end.

