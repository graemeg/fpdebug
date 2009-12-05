unit winDbgTypes; 

{$mode objfpc}{$H+}

interface

uses
  Windows, 
  SysUtils, //todo: remove sysutils. 
  dbgTypes, winDbgProc;
  
type
  { TWinDbgProcess }
  
  TWinThreadInfo = record
    ID      : TThreadId;
    Handle  : THandle;
  end;

  TWinDbgProcess = class(TDbgTarget)
  private
    fCmdLine  : String;
    fis32proc : Boolean; //todo:
    
    fProcInfo     : TProcessInformation;
    fLastEvent    : TDebugEvent;
    fWaiting      : Boolean;
    fWaited       : Boolean;
    fTerminated   : Boolean;
    fEHandled     : Boolean;
    
    fThreadsCount : Integer;
    fThreads      : array of TWinThreadInfo;
  protected
    
    procedure AddThread(ThreadID: TThreadID; ThreadHandle: THandle);
    procedure RemoveThread(ThreadID: TThreadID);
    function GetThreadIndex(ThreadID: TThreadID): Integer;
  public
    constructor Create;
    destructor Destroy; override;
    
    function Execute(const ACommandLine: String): Boolean; 
    procedure Terminate; override;
    function WaitNextEvent(var Event: TDbgEvent): Boolean; override;
    
    function GetThreadsCount(procID: TDbgProcessID): Integer; override;
    function GetThreadID(procID: TDbgProcessID; AIndex: Integer): TDbgThreadID; override;
    function GetThreadRegs(procID: TDbgProcessID; ThreadID: TDbgThreadID; Regs: TDbgDataList): Boolean; override;
    function SetThreadRegs(procID: TDbgProcessID; ThreadID: TDbgThreadID; Regs: TDbgDataList): Boolean; override;
    function SetSingleStep(procID: TDbgProcessID; ThreadID: TDbgThreadID): Boolean; override;

    function ReadMem(procID: TDbgProcessID; Offset: TDbgPtr; Count: Integer; var Data: array of byte): Integer; override;
    function WriteMem(procID: TDbgProcessID; Offset: TDbgPtr; Count: Integer; const Data: array of byte): Integer; override;
  end;

implementation

function WinDebugProcessStart(const ACommandLine: String): TDbgTarget;
var
  win : TWinDbgProcess;
begin
  win := TWinDbgProcess.Create;
  if not win.Execute(ACommandLine) then begin
    win.Free;
    Result := nil
  end else
    Result := win;
end;


{ TWinDbgProcess }

constructor TWinDbgProcess.Create; 
begin
  fis32proc := True;
end;

destructor TWinDbgProcess.Destroy;  
begin
  inherited Destroy;  
end;

function TWinDbgProcess.ReadMem(procID: TDbgProcessID; Offset: TDbgPtr; Count: Integer; var Data: array of byte): Integer;
begin
  Result := ReadProcMem(fProcInfo.hProcess, Offset, Count, Data);
end;

function TWinDbgProcess.WriteMem(procID: TDbgProcessID; Offset: TDbgPtr; Count: Integer; const Data: array of byte): Integer;
begin
  Result := WriteProcMem(fProcInfo.hProcess, Offset, Count, Data);
  FlushInstructionCache(fProcInfo.hProcess, @Offset, Count);
end;

procedure TWinDbgProcess.AddThread(ThreadID: TThreadID; ThreadHandle: THandle);
var
  i : Integer;
begin
  for i:=0 to fThreadsCount-1 do 
    if fThreads[i].id = ThreadID then 
      Exit;
  if fThreadsCount = length(fThreads) then begin
    if fThreadsCount = 0 then SetLength(fThreads, 4)
    else SetLength(fThreads, fThreadsCount*2);
  end;
  fThreads[fThreadsCount].id := ThreadID;
  fThreads[fThreadsCount].Handle := ThreadHandle;
  inc(fThreadsCount);  
end;

procedure TWinDbgProcess.RemoveThread(ThreadID: TThreadID); 
var
  i : Integer;
  j : Integer;
begin
  for i:=0 to fThreadsCount-1 do 
    if fThreads[i].id=ThreadID then begin
      for j := i + 1 to  fThreadsCount-1 do fThreads[j-1]:=fThreads[j];
      dec(fThreadsCount);
      Exit;
    end;
end;

function TWinDbgProcess.GetThreadIndex(ThreadID: TThreadID): Integer;
var
  i : Integer;
begin
  for i := 0 to fThreadsCount - 1 do
    if fThreads[i].id=ThreadID then begin
      Result := i;
      Exit;
    end;
  Result := -1;
end;


function TWinDbgProcess.Execute(const ACommandLine: String): Boolean;  
begin
  fCmdLine := ACommandLine;
  
  Result := CreateDebugProcess(ACommandLine, fProcInfo);
  if not Result then Exit;
end;

procedure TWinDbgProcess.Terminate;  
begin
end;

function TWinDbgProcess.WaitNextEvent(var Event: TDbgEvent): Boolean;  
var
  ContStatus  : LongWord;
const
  HandledStatus : array [Boolean] of LongWord =(DBG_EXCEPTION_NOT_HANDLED, DBG_CONTINUE);
begin
  Result := false;
  if fWaited and (fLastEvent.dwDebugEventCode = EXCEPTION_DEBUG_EVENT) then begin
    case fLastEvent.Exception.ExceptionRecord.ExceptionCode of
      EXCEPTION_BREAKPOINT, 
      EXCEPTION_SINGLE_STEP: ContStatus := DBG_CONTINUE
    else 
      ContStatus := HandledStatus[fEHandled]
    end;
  end else
    ContStatus := DBG_CONTINUE;

  try
    with fLastEvent do
      ContinueDebugEvent(dwProcessId, dwThreadId, ContStatus);
  except
    //writeln('exception while ContinueDebugEvent');
  end;

  if fTerminated then begin
    Result := false;
    Exit;
  end;
  
  FillChar(fLastEvent, sizeof(fLastEvent), 0);
  try
    fWaiting:=true;
    Result := Windows.WaitForDebugEvent(fLastEvent, INFINITE);
  except
    //writeln('exception while WaitForDebugEvent');
  end;
  fWaiting:=false;

  fWaited:=Result;
  
  if Result then begin
    Event.Debug := DebugWinEvent(fProcInfo.hProcess, fLastEvent);
    WinEventToDbgEvent(fLastEvent, Event);
    Event.Process:=fLastEvent.dwProcessId;    
    Event.Thread:=fLastEvent.dwThreadId;
    
    case fLastEvent.dwDebugEventCode of
      CREATE_PROCESS_DEBUG_EVENT: 
      begin
        writeln('Win Process Started: ', fLastEvent.dwProcessId);
        AddThread(fLastEvent.dwThreadId, fLastEvent.CreateProcessInfo.hThread); 
      end;
      CREATE_THREAD_DEBUG_EVENT:
        AddThread(fLastEvent.dwThreadId, fLastEvent.CreateThread.hThread);
      EXIT_PROCESS_DEBUG_EVENT, EXIT_THREAD_DEBUG_EVENT: 
      begin
        if fLastEvent.dwDebugEventCode =  EXIT_PROCESS_DEBUG_EVENT then
          writeln('Win Process Terminated: ', fLastEvent.dwProcessId);
        RemoveThread(fLastEvent.dwThreadId);
      end;
    end;
  
    fTerminated := (Event.Kind = dek_ProcessTerminated) and (fLastEvent.dwProcessId = fProcInfo.dwProcessId);
    Event.Debug := '*'+Event.Debug + ' ' + IntToStr(fLastEvent.dwDebugEventCode);
  end else
    event.Debug := 'GetLastError = ' + IntToStr(GetLastError);
end;

function TWinDbgProcess.GetThreadsCount(procID: TDbgProcessID): Integer;
begin
  Result := fThreadsCount;  
end;

function TWinDbgProcess.GetThreadID(procID: TDbgProcessID; AIndex: Integer): TDbgThreadID;
begin
  if (AIndex < 0) or (AIndex >= fThreadsCount) 
    then Result := 0
    else Result := fThreads[AIndex].id;
end;

function TWinDbgProcess.GetThreadRegs(procID: TDbgProcessID; ThreadID: TDbgThreadID; Regs: TDbgDataList): Boolean;
var
  idx   : Integer;
  hnd   : THandle;
begin
  Result := false;
  if fWaiting then Exit;

  idx := GetThreadIndex(ThreadID);
  Result := idx >= 0;
  if not Result then Exit;
  
  hnd := fThreads[idx].Handle;
  
  if fis32proc then begin
    Result:=DoReadThreadRegs32(hnd, Regs);
  end else
    Result := false;
end;

function TWinDbgProcess.SetThreadRegs(procID: TDbgProcessID; ThreadID: TDbgThreadID; Regs: TDbgDataList): Boolean;
var
  idx   : Integer;
  hnd   : THandle;
begin
  Result := false;
  if fWaiting then Exit;

  idx := GetThreadIndex(ThreadID);
  Result := idx >= 0;
  if not Result then Exit;
  
  hnd := fThreads[idx].Handle;
  
  if fis32proc then 
    Result:=DoWriteThreadRegs32(hnd, Regs)
  else
    Result := false;
end;

function TWinDbgProcess.SetSingleStep(procID: TDbgProcessID; ThreadID: TDbgThreadID): Boolean;
var
  idx : Integer;
begin
  idx := GetThreadIndex(ThreadID);
  Result := idx >= 0;
  if not Result then Exit;

  if fis32proc then 
    Result := SetThread32SingleStep(fThreads[idx].Handle)
  else 
    //todo:
    Result := false;
end;

procedure InitWindowsError;
begin
  SetErrorMode(SEM_FAILCRITICALERRORS or SEM_NOALIGNMENTFAULTEXCEPT or SEM_NOGPFAULTERRORBOX or SEM_NOOPENFILEERRORBOX);
end;

procedure InitWindowsDebug;
begin
  DebugProcessStart := @WinDebugProcessStart;
end;

initialization
  InitWindowsError;
  InitWindowsDebug;
  
end.

