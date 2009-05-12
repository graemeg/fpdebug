unit nixDbgTypes;

{$mode objfpc}{$H+}

interface

uses
  BaseUnix, Unix, dbgTypes, nixPtrace, linuxDbgProc;

type

  { TLinuxProcess }

  TLinuxProcess = class(TDbgProcess)
  private
    fState  : TDbgState;
    fChild  : TPid;
    fContSig    : Integer;
    fTerminated : Boolean;
    fWaited     : Boolean;
  protected
    function GetProcessState: TDbgState; override;
  public
    function StartProcess(const ACmdLine: String): Boolean;
    function WaitNextEvent(var Event: TDbgEvent): Boolean; override;
    procedure Terminate; override;
  end;

function DebugLinuxProcessStart(const ACmdLine: String): TDbgProcess;

implementation

function DebugLinuxProcessStart(const ACmdLine: String): TDbgProcess;
var
  dbg : TLinuxProcess;
begin
  dbg := TLinuxProcess.Create;
  if not dbg.StartProcess(ACmdLine) then begin
    dbg.Free;
    Result := nil;
  end else
    Result := dbg;
end;

{ TLinuxProcess }

function TLinuxProcess.GetProcessState: TDbgState;
begin
  Result := fState;
end;

function TLinuxProcess.StartProcess(const ACmdLine: String): Boolean;
begin
  Result := ForkAndDebugProcess(ACmdLine, fChild);
  if not Result then Exit;
end;

procedure TLinuxProcess.Terminate;
begin

end;

function TLinuxProcess.WaitNextEvent(var Event: TDbgEvent): Boolean;
var
  Status : Integer;
  fch    : TPid;
begin
  if fChild = 0 then begin
    Result := false;
    Exit;
  end;

  if fWaited then _ptrace_cont(fChild, fContSig);

  if fTerminated then begin
    Result := false;
    Exit;
  end;

  fCh := FpWaitPid(fChild, Status, 0);
  if fCh < 0 then begin // failed to wait
    Result := false;
    fChild := 0;
    fTerminated := true;
    Exit;
  end;

  if isStopped(Status, fContSig) then begin
    case fContSig of
      SIGTRAP: fContSig := 0;
    end;
  end;

  Result := WaitStatusToDbgEvent(fChild, Status, Event);
  fWaited := Result;
  fTerminated := Event.Kind = dek_ProcessTerminated;
end;

initialization
  DebugProcessStart := @DebugLinuxProcessStart;

end.

