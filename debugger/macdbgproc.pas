unit macDbgProc;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, // todo: remove
  machapi, mach_port;


function AllocSelfPort: mach_port_name_t;
function ReadTaskMem(target_task: vm_map_t; offset: QWord;
  count: Integer;	data: Pointer; var dataCnt: QWord): kern_return_t;

function macherr(kret: kern_return_t): String;

function debugout_kret(kret: kern_return_t; const Comment: string): kern_return_t;


procedure RecvMessage(port: mach_port_t);

implementation

function debugout_kret(kret: kern_return_t; const Comment: string): kern_return_t;
begin
  if kret <> KERN_SUCCESS then writeln(comment, ' ', macherr(kret));
  Result := kret;
end;

function macherr(kret: kern_return_t): String;
begin
  case kret of
    KERN_SUCCESS                : Result := ''; // success!
    KERN_INVALID_ADDRESS        : Result := 'KERN_INVALID_ADDRESS';
    KERN_PROTECTION_FAILURE     : Result := 'KERN_PROTECTION_FAILURE';
    KERN_NO_SPACE               : Result := 'KERN_NO_SPACE';
    KERN_INVALID_ARGUMENT       : Result := 'KERN_INVALID_ARGUMENT';
    KERN_FAILURE                : Result := 'KERN_FAILURE';
    KERN_RESOURCE_SHORTAGE      : Result := 'KERN_RESOURCE_SHORTAGE';
    KERN_NOT_RECEIVER           : Result := 'KERN_NOT_RECEIVER';
    KERN_NO_ACCESS              : Result := 'KERN_NO_ACCESS';
    KERN_MEMORY_FAILURE         : Result := 'KERN_MEMORY_FAILURE';
    KERN_MEMORY_ERROR           : Result := 'KERN_MEMORY_ERROR';
   	KERN_ALREADY_IN_SET         : Result := 'KERN_ALREADY_IN_SET';
    KERN_NOT_IN_SET             : Result := 'KERN_NOT_IN_SET';
    KERN_NAME_EXISTS            : Result := 'KERN_NAME_EXISTS';
    KERN_ABORTED                : Result := 'KERN_ABORTED';
    KERN_INVALID_NAME           : Result := 'KERN_INVALID_NAME';
   	KERN_INVALID_TASK           : Result := 'KERN_INVALID_TASK';
    KERN_INVALID_RIGHT          : Result := 'KERN_INVALID_RIGHT';
    KERN_INVALID_VALUE          : Result := 'KERN_INVALID_VALUE';
   	KERN_UREFS_OVERFLOW         : Result := 'KERN_UREFS_OVERFLOW';
   	KERN_INVALID_CAPABILITY     : Result := 'KERN_INVALID_CAPABILITY';
    KERN_RIGHT_EXISTS           : Result := 'KERN_RIGHT_EXISTS';
   	KERN_INVALID_HOST           : Result := 'KERN_INVALID_HOST';
    KERN_MEMORY_PRESENT         : Result := 'KERN_MEMORY_PRESENT';
    KERN_MEMORY_DATA_MOVED      : Result := 'KERN_MEMORY_DATA_MOVED';
    KERN_MEMORY_RESTART_COPY    : Result := 'KERN_MEMORY_RESTART_COPY';
    KERN_INVALID_PROCESSOR_SET  : Result := 'KERN_INVALID_PROCESSOR_SET';
    KERN_POLICY_LIMIT           : Result := 'KERN_POLICY_LIMIT';
    KERN_INVALID_POLICY         : Result := 'KERN_INVALID_POLICY';
    KERN_INVALID_OBJECT         : Result := 'KERN_INVALID_OBJECT';
    KERN_ALREADY_WAITING        : Result := 'KERN_ALREADY_WAITING';
    KERN_DEFAULT_SET            : Result := 'KERN_DEFAULT_SET';
    KERN_EXCEPTION_PROTECTED    : Result := 'KERN_EXCEPTION_PROTECTED';
    KERN_INVALID_LEDGER         : Result := 'KERN_INVALID_LEDGER';
    KERN_INVALID_MEMORY_CONTROL : Result := 'KERN_INVALID_MEMORY_CONTROL';
    KERN_INVALID_SECURITY       : Result := 'KERN_INVALID_SECURITY';
    KERN_NOT_DEPRESSED          : Result := 'KERN_NOT_DEPRESSED';
    KERN_TERMINATED             : Result := 'KERN_TERMINATED';
    KERN_LOCK_SET_DESTROYED     : Result := 'KERN_LOCK_SET_DESTROYED';
    KERN_LOCK_UNSTABLE          : Result := 'KERN_LOCK_UNSTABLE';
    KERN_LOCK_OWNED             : Result := 'KERN_LOCK_OWNED';
    KERN_LOCK_OWNED_SELF        : Result := 'KERN_LOCK_OWNED_SELF';
    KERN_SEMAPHORE_DESTROYED    : Result := 'KERN_SEMAPHORE_DESTROYED';
    KERN_RPC_SERVER_TERMINATED  : Result := 'KERN_RPC_SERVER_TERMINATED';
    KERN_RPC_TERMINATE_ORPHAN   : Result := 'KERN_RPC_TERMINATE_ORPHAN';
    KERN_RPC_CONTINUE_ORPHAN    : Result := 'KERN_RPC_CONTINUE_ORPHAN';
    KERN_NOT_SUPPORTED          : Result := 'KERN_NOT_SUPPORTED';
    KERN_NODE_DOWN              : Result := 'KERN_NODE_DOWN';
    KERN_NOT_WAITING            : Result := 'KERN_NOT_WAITING';
    KERN_OPERATION_TIMED_OUT    : Result := 'KERN_OPERATION_TIMED_OUT';
    KERN_RETURN_MAX             : Result := 'KERN_RETURN_MAX';

    MACH_MSG_IPC_SPACE               : Result := 'MACH_MSG_IPC_SPACE';
    MACH_MSG_VM_SPACE                : Result := 'MACH_MSG_VM_SPACE';
    MACH_MSG_IPC_KERNEL              : Result := 'MACH_MSG_IPC_KERNEL';
    MACH_MSG_VM_KERNEL               : Result := 'MACH_MSG_VM_KERNEL';

    MACH_SEND_IN_PROGRESS            : Result := 'MACH_SEND_IN_PROGRESS';
    MACH_SEND_INVALID_DATA           : Result := 'MACH_SEND_INVALID_DATA';
    MACH_SEND_INVALID_DEST           : Result := 'MACH_SEND_INVALID_DEST';
    MACH_SEND_TIMED_OUT              : Result := 'MACH_SEND_TIMED_OUT';
    MACH_SEND_INTERRUPTED            : Result := 'MACH_SEND_INTERRUPTED';
    MACH_SEND_MSG_TOO_SMALL          : Result := 'MACH_SEND_MSG_TOO_SMALL';
    MACH_SEND_INVALID_REPLY          : Result := 'MACH_SEND_INVALID_REPLY';
    MACH_SEND_INVALID_RIGHT          : Result := 'MACH_SEND_INVALID_RIGHT';
    MACH_SEND_INVALID_NOTIFY         : Result := 'MACH_SEND_INVALID_NOTIFY';
    MACH_SEND_INVALID_MEMORY         : Result := 'MACH_SEND_INVALID_MEMORY';
    MACH_SEND_NO_BUFFER              : Result := 'MACH_SEND_NO_BUFFER';
    MACH_SEND_TOO_LARGE              : Result := 'MACH_SEND_TOO_LARGE';
    MACH_SEND_INVALID_TYPE           : Result := 'MACH_SEND_INVALID_TYPE';
    MACH_SEND_INVALID_HEADER         : Result := 'MACH_SEND_INVALID_HEADER';
    MACH_SEND_INVALID_TRAILER        : Result := 'MACH_SEND_INVALID_TRAILER';
    MACH_SEND_INVALID_RT_OOL_SIZE    : Result := 'MACH_SEND_INVALID_RT_OOL_SIZE';

    MACH_RCV_IN_PROGRESS             : Result := 'MACH_RCV_IN_PROGRESS';
    MACH_RCV_INVALID_NAME            : Result := 'MACH_RCV_INVALID_NAME';
    MACH_RCV_TIMED_OUT               : Result := 'MACH_RCV_TIMED_OUT';
    MACH_RCV_TOO_LARGE               : Result := 'MACH_RCV_TOO_LARGE';
    MACH_RCV_INTERRUPTED             : Result := 'MACH_RCV_INTERRUPTED';
    MACH_RCV_PORT_CHANGED            : Result := 'MACH_RCV_PORT_CHANGED';
    MACH_RCV_INVALID_NOTIFY          : Result := 'MACH_RCV_INVALID_NOTIFY';
    MACH_RCV_INVALID_DATA            : Result := 'MACH_RCV_INVALID_DATA';
    MACH_RCV_PORT_DIED               : Result := 'MACH_RCV_PORT_DIED';
    MACH_RCV_IN_SET                  : Result := 'MACH_RCV_IN_SET';
    MACH_RCV_HEADER_ERROR            : Result := 'MACH_RCV_HEADER_ERROR';
    MACH_RCV_BODY_ERROR              : Result := 'MACH_RCV_BODY_ERROR';
    MACH_RCV_INVALID_TYPE            : Result := 'MACH_RCV_INVALID_TYPE';
    MACH_RCV_SCATTER_SMALL           : Result := 'MACH_RCV_SCATTER_SMALL';
    MACH_RCV_INVALID_TRAILER         : Result := 'MACH_RCV_INVALID_TRAILER';
    MACH_RCV_IN_PROGRESS_TIMED       : Result := 'MACH_RCV_IN_PROGRESS_TIMED';
  else
    Result := 'ERR #'+IntToHex(kret, sizeof(kret)*2)
  end;
end;

function ReadTaskMem(target_task: vm_map_t; offset: QWord;
	count: Integer;	data: Pointer; var dataCnt: QWord): kern_return_t;
var
  err : Integer;
begin
  writeln('reading task = ', target_task);
  err := mach_vm_protect(target_task, offset, count, false, VM_PROT_READ);
  if err <> KERN_SUCCESS then  writeln('mach_vm_protect = ', macherr(err));

  err := mach_vm_read_overwrite(target_task, offset, count, mach_vm_address_t(PtrUInt(data)), dataCnt);
  if err <> KERN_SUCCESS then writeln('mach_vm_read = ', macherr(err));

  Result := err;
end;

function AllocSelfPort: mach_port_name_t;
var
  res : Integer;
begin
  res := debugout_kret( mach_port_allocate(mach_task_self, MACH_PORT_RIGHT_RECEIVE, Result), 'mach_port_allocate');

  if res = KERN_SUCCESS then
    debugout_kret( mach_port_insert_right (mach_task_self, Result, Result, MACH_MSG_TYPE_MAKE_SEND), 'mach_port_insert_right');
end;

procedure RecvMessage(port: mach_port_t);
var
  hdr : pmach_msg_header_t;
  buf : array [0..1023] of byte;
  res : kern_return_t;
begin
  writeln('receiving message at port: ', port);
  FillChar(buf, sizeof(buf), 0);
  res := debugout_kret( mach_msg( @buf[0], MACH_RCV_MSG, 0, length(buf), port, MACH_MSG_TIMEOUT_NONE, MACH_PORT_NULL),
    'mach_msg');

  if res <> KERN_SUCCESS then Exit;
  hdr := @buf;

  writeln('bits =  ', IntToHex(hdr^.msgh_bits, 8));
  writeln('size =  ', hdr^.msgh_size);
  writeln('rport = ', hdr^.sgh_remote_port);
  writeln('lport = ', hdr^.sgh_local_port);
  writeln('resv  = ', hdr^.sgh_reserved);
  writeln('id    = ', hdr^.sgh_id);
end;

end.

