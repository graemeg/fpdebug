unit macDbgType;

{$mode objfpc}{$H+}

interface

uses
  BaseUnix, Unix, machapi, dbgTypes;


type
  TMachDbgProcess = class(TDbgProcess)
  protected
    function GetProcessState: TDbgState; override;
  public
    function StartProcess(const ACmdLine: String): Boolean;

    procedure Terminate; override;
    function WaitNextEvent(var Event: TDbgEvent): Boolean; override;
    property State: TDbgState read GetProcessState;
  end;

implementation

//todo: consider using mach api to launch the child process, rather than unix's fork
function ForkAndRun(const CommandLine: String; var ChildId: TPid; var ChildTask: mach_port_name_t): Boolean;
var
  apipe : TFilDes;
  len   : Integer;
  res   : Integer;
begin
  Result := false;
  if FpPipe(apipe) < 0 then Exit;

  childid := FpFork;
  if childid < 0 then Exit;

  if childid = 0 then begin
    task_for_pid( mach_task_self, FpGetpid, childtask);
    FpWrite(apipe[1], childtask, sizeof(childtask));

    res := FpExecV(CommandLine, nil);
    if res < 0 then begin
      writeln('failed to run: ', CommandLine);
      Halt;
    end;

  end else begin
    len := FpRead(apipe[0], ChildTask, sizeof(childtask) );
    Result := len = sizeof(childtask);
  end;
  FpClose(apipe[0]);
  FpClose(apipe[1]);
end;

function TMachDbgProcess.GetProcessState: TDbgState;
begin
  Result := ds_Nonstarted;
end;

procedure TMachDbgProcess.Terminate;
begin
end;

function TMachDbgProcess.WaitNextEvent(var Event: TDbgEvent): Boolean;
begin
  Result := false;
end;


function TMachDbgProcess.StartProcess(const ACmdLine: String): Boolean;
begin
  Result := true;
end;

(*

  machname := ParamStr(1);
  //  if machname = '' then machname := 'main';

  if machname = '' then begin
    writeln('please specify executable filename. Exiting...');
    Exit;
  end;

  if not StartDebug then begin
    writeln('unable to start debug. Exiting...');
    Exit;
  end;
  writeln('debugged process started');
  writeln('pid = ', childpid);

  try
    macherr := task_for_pid( mach_task_self, childpid, childtask);
    if macherr <> 0 then begin
      writeln('unable to get task port for child process. Exiting...');
      Exit;
    end;
    writeln('task port = ', childtask);

    nixerr := WaitProcess(childpid);
    writeln('[1] wait err: ', nixerr);
    ptrace(PT_CONTINUE, childpid, PtrUInt(1), 0);

    {writeln('[2] wait err: ',  FpWaitPid(childpid, nixerr, 0));
    nixerr := -nixerr;
    writeln('status = ', nixerr);}
    nixerr := WaitProcess(childpid);

    if nixerr < -1 then nixerr := -nixerr;
    writeln('[2] wait err: ', nixerr);
    if WIFSTOPPED(nixerr) then begin
      writeln('stopped');
      writeln('  signal = ', SignalToStr(wstopsig(nixerr)));
      ReportRegisters;

    end else if wifsignaled(nixerr) then begin
      writeln('signaled...');
      writeln('  termsign = ', wtermsig(nixerr));
    end else if wifexited(nixerr) then
      writeln('exited');


{    for i := 1 to 10 do begin
      nixerr := WaitProcess(childpid);
      writeln('wait err: ', nixerr);

      macherr := task_threads(childtask, thread_list, thread_count);
      writeln('task_threads = ', thread_count, ' err = ', macherr);
      if macherr = 0 then begin
        FillChar(regs, sizeof(regs), 0);
        regsize := sizeof(regs);
        macherr := thread_get_state(thread_list[0], x86_THREAD_STATE32, thread_state_t(@regs), regsize);
        writeln('thread_get_state = ', regsize, ' err = ', macherr);
        Reporti386(regs);
      end else
        writeln('unable to get registers');


      nixerr := ptrace(PT_STEP, childpid, PtrUint(1), 0);
      writeln('ptrace err: ', nixerr);


    end;}


  finally
    ptrace(PT_CONTINUE, childpid, PtrUint(1), 0);
    FpKill(childpid, 1);
    nixerr := WaitProcess(childpid);
    writeln('[after kill] wait err: ', nixerr);
  end;
*)

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

