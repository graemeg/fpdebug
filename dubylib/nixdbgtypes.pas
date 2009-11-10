unit nixDbgTypes;

{ Linux base debugging type }

{$mode objfpc}{$H+}

interface

uses
  SysUtils, BaseUnix, Unix,
  nixPtrace, linuxDbgProc,
  dbgTypes, dbgCPU, dbgUtils;

type
  TCpuType = (cpi386, cpx64);

  { TLinuxProcess }

  TLinuxProcess = class(TDbgTarget)
  private
    fState      : TDbgState;
    fChild      : TPid;
    fContSig    : Integer;
    fTerminated : Boolean;
    fWaited     : Boolean;
    fcputype    : TCpuType;

  public
    constructor Create;
    function StartProcess(const ACmdLine: String): Boolean;
    function WaitNextEvent(var Event: TDbgEvent): Boolean; override;
    procedure Terminate; override;
    function GetProcessState(procID: TDbgProcessID): TDbgState; override;

    function GetThreadsCount(procID: TDbgProcessID): Integer; override;
    function GetThreadID(procID: TDbgProcessID; AIndex: Integer): TDbgThreadID; override;
    function GetThreadRegs(procID: TDbgProcessID; ThreadID: TDbgThreadID; Registers: TDbgDataList): Boolean; override;
    function SetThreadRegs(procID: TDbgProcessID; ThreadID: TDbgThreadID; Registers: TDbgDataList): Boolean; override;

    function SetSingleStep(procID: TDbgProcessID; ThreadID: TDbgThreadID): Boolean; override;

    function MainThreadID(procID: TDbgProcessID): TDbgThreadID; override;

    function ReadMem(procID: TDbgProcessID; Offset: TDbgPtr; Count: Integer; var Data: array of byte): Integer; override;
    function WriteMem(procID: TDbgProcessID; Offset: TDbgPtr; Count: Integer; const Data: array of byte): Integer; override;
  end;

function DebugLinuxProcessStart(const ACmdLine: String): TDbgTarget;

implementation

const
  HexSize = sizeof(TDbgPtr)*2;

function DebugLinuxProcessStart(const ACmdLine: String): TDbgTarget;
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

function TLinuxProcess.GetProcessState(procID: TDbgProcessID): TDbgState;
begin
  Result := fState;
end;

function TLinuxProcess.GetThreadsCount(procID: TDbgProcessID): Integer;
begin
  Result := 0;
end;

function TLinuxProcess.GetThreadID(procID: TDbgProcessID; AIndex: Integer): TDbgThreadID;
begin
  Result := 0;
end;

function TLinuxProcess.GetThreadRegs(procID: TDbgProcessID; ThreadID: TDbgThreadID; Registers: TDbgDataList): Boolean;
begin
  case fcputype of
    cpi386:
      Result := ReadRegsi386(fChild, Registers);
  else
    Result := false;
  end;
end;

function TLinuxProcess.SetThreadRegs(procID: TDbgProcessID; ThreadID: TDbgThreadID; Registers: TDbgDataList): Boolean;
begin
  case fcputype of
    cpi386: begin
      Result := WriteRegsi386(fChild, Registers);
    end;
  else
    Result := false;
  end;
end;

function TLinuxProcess.ReadMem(procID: TDbgProcessID; Offset: TDbgPtr; Count: Integer; var Data: array of byte): Integer;
begin
  Result := ReadProcMem(fChild, Offset, Count, Data);
end;

function TLinuxProcess.WriteMem(procID: TDbgProcessID; Offset: TDbgPtr; Count: Integer; const Data: array of byte): Integer;
begin
  Result := WriteProcMem(fChild, Offset, Count, Data);
end;

function TLinuxProcess.SetSingleStep(procID: TDbgProcessID; ThreadID: TDbgThreadID): Boolean;
begin
  Result := ptraceSingleStep(ThreadID);
end;

function TLinuxProcess.MainThreadID(procID: TDbgProcessID): TDbgThreadID;
begin
  Result := fChild;
end;

constructor TLinuxProcess.Create;
begin
  {$ifdef cpui386}fcputype:=cpi386;{$endif}
end;

function TLinuxProcess.StartProcess(const ACmdLine: String): Boolean;
begin
  Result := ForkAndDebugProcess(ACmdLine, fChild);
  if not Result then Exit;
end;

procedure TLinuxProcess.Terminate;
begin
  // Terminate
  FpKill(fChild, SIGKILL);
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

  if fWaited then ptraceCont(fChild, fContSig);

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
  end else if Status = 0 then begin

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

