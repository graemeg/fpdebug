// the purpose of the test, is to check out XNU Macho-API.
program xnutest;

{$mode objfpc}{$h+}

uses
  BaseUnix,
  Unix,
  macDbgProc,
  macDbgType,
  macPtrace,
  machapi,
  machexc,
  mach_port,
  macdbgutils;

var
  target: TMacDbgTarget;

var
  GlobalChild : TPid = 0;

  procedure UserCheck;
  var
    userid: TUid;
  begin
    userid := FpGetuid;
    if (userid <> 0) then
    begin
      writeln('WARNING: you''re launching as non root user. Debugging might fail on OSX 10.5 (and higher)');
      writeln('press enter to continue');
      readln;
    end;
  end;


  procedure TestExceptionPorts(task: task_t);
  var
    cnt: integer;
    masks: array [0..63] of exception_mask_t;
    ports: array [0..63] of exception_port_array_t;
    behav: array [0..63] of exception_behavior_t;
    flav: array [0..63] of thread_state_flavor_t;
    i: integer;
  begin
    FillChar(masks[0], length(masks) * sizeof(exception_mask_t), 0);
    cnt := length(masks);
    task_get_exception_ports(task, EXC_MASK_ALL, @masks, cnt, @ports, @behav, @flav);
    writeln('old ports: ', cnt);
    writelN('mask': 12, 'handler': 12, 'behaviour': 12, 'flavor': 12);
    for i := 0 to cnt - 1 do
    begin
      writeln(hexStr(masks[i], 8): 12, PtrUInt(ports[i]): 12, HexStr(behav[i], 8): 12,
        HexStr(flav[i], 8): 12, ' ', ExcMaskStr(masks[i]));
    end;
  end;


  function WaitForMsg(waitport: mach_port_t): boolean;
  var
    buf: array [0..4095] of byte;
    ret: kern_return_t;
    req: PRequestUnion__exc_subsystem;

    respbuf: array [0..4095] of byte;
    resp: pmach_msg_header_t;

    susp: Boolean;

  begin
    FillChar(buf, sizeof(buf), 0);
    ret := mach_msg(@buf, MACH_RCV_MSG or MACH_RCV_LARGE or MACH_RCV_TIMEOUT or MACH_RCV_INTERRUPT,
      0, sizeof(buf), waitport, 1000, 0);

    Result := ret = MACH_MSG_SUCCESS;

    if not Result then
    begin
      if ret = MACH_RCV_TIMED_OUT then
        writeln('timeout...')
      else
        writeln('mach_msg failed: ', ret, ' ', HexStr(ret, 8));
      Exit;
    end;

    req := @buf[0];
    if req^.req_raise.Head.msgh_id = msgid_exception_raise then
    begin
      writeln('EXCEPTION REQUEST!');
      debugExcReqRaise(req^.req_raise, '  ');

      FillChar(respbuf, sizeof(respbuf), 0);
      resp := @respbuf[0];

      writeln('calling exc_server(), trying to handle the exception');
      writeln('@catch_exception_raise = ', PtrUInt(@catch_exception_raise));
      writeln('@response id = ', resp^.msgh_id);

      writeln(' is suspended = ', isTaskSuspended(req^.req_raise.task.name));
      Result := exc_server(pmach_msg_header_t(req), resp);
      writeln(' is suspended = ', isTaskSuspended(req^.req_raise.task.name));

      if (req^.req_raise.exception=EXC_SOFTWARE) and (req^.req_raise.code[0]=EXC_SOFT_SIGNAL) then begin
         case req^.req_raise.code[1] of
           SIGTRAP: begin
             writeln('SIGTRAP');
             ptrace(PT_CONTINUE, GlobalChild, 1, 0);
           end;
         end;
      end;


      writeln('@response id = ', resp^.msgh_id);
      writeln('RESPONSE: ');
      debugMsgHead(resp^,'  ');
      writeln('sizeof pmach_msg_header_t = ', sizeof(mach_msg_header_t));
      if Result then
        writeln('exc_server() success!')
      else
        writeln('exc_server() failed!');

      ret := mach_msg(resp, MACH_SEND_MSG, resp^.msgh_size, 0, MACH_PORT_NULL, 0, MACH_PORT_NULL);
      if (ret <> KERN_SUCCESS) then
        writeln('Error sending message to original reply port, ', ret, ' ', HexStr(ret, 8))
      else
      begin
        writeln('sending hanlding message success! ', ret);
        Result := True;
      end;
    end;
  end;

  procedure DebugExe(const ExeFileName: ansistring);
  var
    childpid: TPid;
    childtask: mach_port_t;
    status: integer;

    excport: mach_port_t;

    ret: kern_return_t;
    i: integer;
  begin
    // fork and run the application.
    // the child sets PTRACE_ME and SIGEXC flags
    // so don't use WaitForPid anymore.
    // Install the exception handler and catch the exceptions
    if not ForkAndRun(ExeFileName, childpid, childtask, True) then
    begin
      writeln('unable to launch: ', ExeFileName);
      Exit;
    end;

    writeln('child pid:  ', childpid);
    GlobalChild:=childpid;
    writeln('child task: ', childtask);
    writeln('child task info:');
    PrintTaskAllInfo(childtask);
    writeln('child task threads:');
    PrintTaskThreads(childtask);

    TestExceptionPorts(childtask);

    excport := MachAllocPortForSelf;
    writeln('self port = ', excport);

    ret := mach_port_insert_right(ipc_space_t(mach_task_self),
      excport, excport, MACH_MSG_TYPE_MAKE_SEND);
    if ret <> KERN_SUCCESS then
      writeln('mach_port_insert_right failed: ', ret, ' ', hexStr(ret, 8));
    ret := task_set_exception_ports(childtask, EXC_MASK_ALL,
      excport,
      EXCEPTION_DEFAULT, 0);
    if ret <> KERN_SUCCESS then
      writeln('task_set_exception_ports failed: ', ret, ' ', hexStr(ret, 8));

    TestExceptionPorts(childtask);

    writeln('resuming execution');

    while True do
    begin
      ret := task_resume(childtask);
      if ret <> KERN_SUCCESS then
        writeln('task_resume failed: ', ret, ' ', hexStr(ret, 8));

      Writeln(':: waiting for the message to arrive!');
      if not WaitForMsg(excport) then
      begin
        writeln(':: failed to recv the message');
        PrintTaskAllInfo(childtask);
        PrintTaskThreads(childtask);
        writeln(':: leaving the loop');
        Break;
      end
      else
      begin
        writeln(':: msg received');
        writeln('press enter to continue');
        readln;
      end;
    end;
  end;




{$R xnutest.res}

begin
  UserCheck;

  if ParamCount < 1 then
  begin
    writeln('please specify executable file name');
    exit;
  end;

  DebugExe(ParamStr(1));
end.

