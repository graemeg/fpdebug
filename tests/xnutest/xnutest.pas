// the purpose of the test, is to check out XNU Macho-API.
program xnutest;

{$mode objfpc}{$h+}

uses
  BaseUnix, Unix,
  macDbgProc, macDbgType, macPtrace,
  machapi, machexc, mach_port;

var
  target : TMacDbgTarget;

procedure UserCheck;
var
  userid  : TUid;
begin
  userid:=FpGetuid;
  if (userid <> 0) then
    writeln('WARNING: you''re launching as non root user. Debugging might fail on OSX 10.5 (and higher)');
end;


procedure TestExceptionPorts(task: task_t);
var
  cnt     : Integer;
  masks   : array [0..63] of exception_mask_t;
  ports   : array [0..63] of exception_port_array_t;
  behav   : array [0..63] of exception_behavior_t;
  flav    : array [0..63] of thread_state_flavor_t;
  i       : Integer;
begin
  FillChar(masks[0], length(masks)*sizeof(exception_mask_t), 0);
  cnt:=length(masks);
  task_get_exception_ports(task, EXC_MASK_ALL,
    @masks, cnt,
    @ports, @behav, @flav);
  writeln('old ports: ', cnt);
  writelN('mask':12,'handler':12,'behaviour':12, 'flavor':12);
  for i := 0 to cnt - 1 do begin
    writeln( hexStr(masks[i], 8):12, PtrUInt(ports[i]):12, HexStr(behav[i],8):12, HexStr(flav[i],8):12,' ', ExcMaskStr(masks[i]));
  end;
end;


function WaitForMsg(waitport: mach_port_t): Boolean;
var
  buf : array [0..4095] of byte;
  ret : kern_return_t;
  req : PRequestUnion__exc_subsystem;
begin
  FillChar(buf, sizeof(buf), 0);
  ret := mach_msg( @buf, MACH_RCV_MSG or MACH_RCV_LARGE or MACH_RCV_TIMEOUT,
    0, sizeof(buf), waitport, 1000, 0);

  Result:=ret=MACH_MSG_SUCCESS;

  if not Result then begin
    if ret=MACH_RCV_TIMED_OUT then writeln('timeout...')
    else  writeln('mach_msg failed: ', ret, ' ', HexStr(ret, 8));
    Exit;
  end;

  req:=@buf[0];
  writeln('exception type = ', ExceptionType(req^.req_raise.exception));

end;

procedure DebugExe(const ExeFileName: Ansistring);
var
  childpid  : TPid;
  childtask : mach_port_t;
  status  : Integer;

  excport  : mach_port_t;

  ret : kern_return_t;
  i   : Integer;
begin
  // fork and run the application.
  // the child sets PTRACE_ME and SIGEXC flags
  // so don't use WaitForPid anymore.
  // Install the exception handler and catch the exceptions
  if not ForkAndRun(ExeFileName, childpid, childtask, True) then begin
    writeln('unable to launch: ', ExeFileName);
    Exit;
  end;

  writeln('child pid:  ', childpid);
  writeln('child task: ', childtask);
  PrintTaskAllInfo(childtask);
  PrintTaskThreads(childtask);

  TestExceptionPorts(childtask);

  excport := MachAllocPortForSelf;
  writeln('self port = ', excport);

  ret := mach_port_insert_right( ipc_space_t(mach_task_self),
           excport, excport, MACH_MSG_TYPE_MAKE_SEND);
  if ret <> KERN_SUCCESS then
    writeln('mach_port_insert_right failed: ', ret, ' ', hexStr(ret, 8));
  ret := task_set_exception_ports(
           childtask, EXC_MASK_ALL, excport, EXCEPTION_DEFAULT, 0);
  if ret <> KERN_SUCCESS then
    writeln('task_set_exception_ports failed: ', ret, ' ', hexStr(ret, 8));

  TestExceptionPorts(childtask);
  ret:=task_resume(childtask);
  if ret <> KERN_SUCCESS then
    writeln('task_resume failed: ', ret, ' ', hexStr(ret, 8));

  Writeln('waiting for the message to arrive!');
  for i:=1 to 10 do
    if not WaitForMsg(excport) then begin
      PrintTaskAllInfo(childtask);
      PrintTaskThreads(childtask);
    end else
      break;
end;



begin
  UserCheck;

  if ParamCount<1 then begin
    writeln('please specify executable file name');
    exit;
  end;

  DebugExe(ParamStr(1));

end.
