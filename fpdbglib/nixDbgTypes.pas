unit nixDbgTypes;

{$mode objfpc}{$H+}

interface

uses
  BaseUnix, Unix, dbgTypes, nixPtrace;

type

  { TUnixProcess }

  TUnixProcess = class(TDbgProcess)
  private
    fState  : TDbgState;
    fChild  : TPid;
  protected
    function GetProcessState: TDbgState; override;
  public
    function StartProcess(const ACmdLine: String): Boolean;
    function WaitNextEvent(var Event: TDbgEvent): Boolean; override;
    procedure Terminate; override;
  end;

function DebugUnixProcessStart(const ACmdLine: String): TDbgProcess;

implementation

function DebugUnixProcessStart(const ACmdLine: String): TDbgProcess;
var
  dbg : TUnixProcess;
begin
  dbg := TUnixProcess.Create;
  dbg.StartProcess(ACmdLine);
  Result := dbg;
end;

{ TUnixProcess }

function TUnixProcess.GetProcessState: TDbgState;
begin
  Result := fState;
end;

function ForkAndDebugProcess(const ACmdLine: String; var childid: TPid): Boolean;
var
  res     : Integer;
begin
  childid := FpFork;
  if childid < 0 then begin
    Result := false;
  end else if childid = 0 then begin
    res := ptrace(PTRACE_TRACEME, childid, nil, nil);
    if res < 0 then Exit; // process cannot be traced
    FpExecV(ACmdLine, nil);
  end else
    Result := true;
end;

function TUnixProcess.StartProcess(const ACmdLine: String): Boolean;
begin
  Result := ForkAndDebugProcess(ACmdLine, fChild);
  if not Result then Exit;
end;

procedure TUnixProcess.Terminate;
begin

end;

function TUnixProcess.WaitNextEvent(var Event: TDbgEvent): Boolean;
var
  Status : Integer;
begin
  if fChild = 0 then begin
    Result := false;
    Exit;
  end;
  fChild := FpWaitPid(fChild, Status, 0);
  if fChild < 0 then begin // failed to wait
    Result := false;
    fChild := 0;
    Exit;
  end;

  Result := false;
end;

initialization

end.

