unit macDbgUtils;

{$mode delphi}

interface

uses
  SysUtils, machapi, mach_port, machexc;

procedure debugMsgHead(const head: mach_msg_header_t; const pfx: String='');
procedure debugExcReqRaise(const req: __Request__exception_raise_t; const pfx: String='');
function debugNDR(const ndr: NDR_record_t): string;
function debugPortDescr(descr: mach_msg_port_descriptor_t): String;
function debugExceptionType(exc_type: Integer): string;

function isTaskValid(t: task_t): Boolean;
function isTaskSuspended(t: task_t): Boolean;
function GetBasicTaskInfo32(t: task_t; var info: task_basic_info): kern_return_t;

implementation

function GetBasicTaskInfo32(t: task_t; var info: task_basic_info): kern_return_t;
var
  ret  : kern_return_t;
  cnt  : mach_msg_type_number_t;
begin
  cnt:=TASK_BASIC_INFO_COUNT;
  Result:=task_info (t, TASK_BASIC_INFO_32, task_info_t(@info), cnt);
end;

function isTaskSuspended(t: task_t): Boolean;
var
  info: task_basic_info;
  ret: kern_return_t;
begin
  ret:=GetBasicTaskInfo32(t, info);
  Result:=ret=KERN_SUCCESS;
  if not Result then writeln('ret, is task suspended ', ret);
  if Result then Result := info.suspend_count>0;
  writeln(info.suspend_count,' ', info.policy,' ', info.resident_size,' ',info.system_time.microseconds,' ',info.user_time.microseconds);
end;

function isTaskValid(t: task_t): Boolean;
var
  info: task_basic_info;
begin
  Result:=GetBasicTaskInfo32(t, info)=KERN_SUCCESS;
end;


function debugMsgHeadBits(bits: Integer): String;
begin
  Result:='';
  if MACH_MSGH_BITS_REMOTE_MASK and bits = MACH_MSGH_BITS_REMOTE_MASK then
    Result:=Result+'MACH_MSGH_BITS_REMOTE_MASK ';
  if MACH_MSGH_BITS_LOCAL_MASK and bits = MACH_MSGH_BITS_LOCAL_MASK then
    Result:=Result+'MACH_MSGH_BITS_LOCAL_MASK ';
  if MACH_MSGH_BITS_COMPLEX and bits = MACH_MSGH_BITS_COMPLEX then
    Result:=Result+'MACH_MSGH_BITS_COMPLEX ';
  if MACH_MSGH_BITS_USER and bits = MACH_MSGH_BITS_USER then
    Result:=Result+'MACH_MSGH_BITS_USER ';
  if MACH_MSGH_BITS_CIRCULAR and bits = MACH_MSGH_BITS_CIRCULAR then
    Result:=Result+'MACH_MSGH_BITS_CIRCULAR ';
  if MACH_MSGH_BITS_USED and bits = MACH_MSGH_BITS_USED then
    Result:=Result+'MACH_MSGH_BITS_USED ';
end;

procedure debugMsgHead(const head: mach_msg_header_t; const pfx: String);
begin
  writeln(pfx+'msgh_bits:        ', IntToHex(head.msgh_bits,8),' ',debugMsgHeadBits(head.msgh_bits));
  writeln(pfx+'msgh_size:        ', head.msgh_size);
  writeln(pfx+'msgh_remote_port: ', head.msgh_remote_port);
  writeln(pfx+'msgh_local_port:  ', head.msgh_local_port);
  writeln(pfx+'msgh_reserved:    ', head.msgh_reserved);
  writeln(pfx+'msgh_id:          ', head.msgh_id);
end;

procedure debugExcReqRaise(const req: __Request__exception_raise_t; const pfx: String='');
begin
  writeln(pfx+'Head:');
  debugMsgHead(req.Head, pfx+'  ');
  writeln(pfx+'Body:');
  writeln(pfx+'  msgh_descriptor_count: ', req.msgh_body.msgh_descriptor_count);
  writeln(pfx+'thread:    ', debugPortDescr(req.thread));
  writeln(pfx+'task:      ', debugPortDescr(req.task));
  writeln(pfx+'NDR:       ', debugNDR(req.NDR));
  writeln(pfx+'exception: ', req.exception, ' ', debugExceptionType(req.exception));
  writeln(pfx+'codeCnt:   ', req.codeCnt);
  writeln(pfx+'code[0,1]: ', IntToHex(req.code[0], 8),' ', IntToHex(req.code[1], 8));
end;

function debugNDR(const ndr: NDR_record_t): string;
begin
  Result:='NDR: '
    +IntToStr(ndr.mig_vers)
    +' '+IntToStr(ndr.if_vers)
    +' '+IntToStr(ndr.reserved1)
    +' '+IntToStr(ndr.mig_encoding)
    +' '+IntToStr(ndr.int_rep)
    +' '+IntToStr(ndr.char_rep)
    +' '+IntToStr(ndr.float_rep)
    +' '+IntToStr(ndr.reserved2);
end;

function debugPortDescr(descr: mach_msg_port_descriptor_t): String;
begin
  Result:='name: '+IntToStr(descr.name) +' ('
    +' (pad1: '+ IntToStr(descr.pad1)
    +' pad2: '+ IntTostr( descr.pad2)
    +' disposition: '+ IntTostr( descr.disposition)
    +' _type: '+IntToStr(descr._type)
    +')';
end;

function debugExceptionType(exc_type: Integer): String;
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
  else
    Result := 'Unknown ' + IntToHex(exc_type, 8);
  end;

end;


end.

