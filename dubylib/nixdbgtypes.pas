unit nixDbgTypes;

{$mode objfpc}{$H+}

interface

uses
  SysUtils,
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
  public
    function StartProcess(const ACmdLine: String): Boolean;
    function WaitNextEvent(var Event: TDbgEvent): Boolean; override;
    procedure Terminate; override;
    function GetProcessState: TDbgState; override;

    function GetThreadsCount: Integer; override;
    function GetThreadID(AIndex: Integer): TDbgThreadID; override;
    function GetThreadRegs(ThreadID: TDbgThreadID; Regs: TDbgRegisters): Boolean; override;

    function ReadMem(Offset: TDbgPtr; Count: Integer; var Data: array of byte): Integer; override;
    function WriteMem(Offset: TDbgPtr; Count: Integer; const Data: array of byte): Integer; override;
  end;

function DebugLinuxProcessStart(const ACmdLine: String): TDbgProcess;

implementation

const
  HexSize = sizeof(TDbgPtr)*2;

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

function TLinuxProcess.GetThreadsCount: Integer;
begin
  Result := 0;
end;

function TLinuxProcess.GetThreadID(AIndex: Integer): TDbgThreadID;
begin
  Result := 0;
end;

function TLinuxProcess.GetThreadRegs(ThreadID: TDbgThreadID; Regs: TDbgRegisters): Boolean;
begin
  Result := false;
end;

function TLinuxProcess.ReadMem(Offset: TDbgPtr; Count: Integer; var Data: array of byte): Integer;
begin
  Result := ReadProcMem(fChild, Offset, Count, Data);
end;

function TLinuxProcess.WriteMem(Offset: TDbgPtr; Count: Integer; const Data: array of byte): Integer;
begin
  Result := 0;
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

  u   : tuser;
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

