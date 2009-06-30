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
    id  : TThreadId;
  end;

  TWinDbgProcess = class(TDbgProcess)
  private
    fState    : TDbgState;
    fCmdLine  : String;
    
    fProcInfo     : TProcessInformation;
    fLastEvent    : TDebugEvent;
    fWaited       : Boolean;
    fTerminated   : Boolean;
    
    fThreadsCount : Integer;
    fThreads      : array of TWinThreadInfo;
  protected
    
    procedure AddThread(ThreadID: TThreadID);
    procedure RemoveThread(ThreadID: TThreadID);
  public
    constructor Create;
    destructor Destroy; override;
    
    function Execute(const ACommandLine: String): Boolean; 
    procedure Terminate; override;
    function WaitNextEvent(var Event: TDbgEvent): Boolean; override;
    
    function GetThreadsCount: Integer; override;
    function GetThreadID(AIndex: Integer): TDbgThreadID; override;
    function GetThreadRegs(ThreadID: TDbgThreadID; Regs: TDbgRegisters): Boolean; override;

    function GetProcessState: TDbgState; override;
    
    function ReadMem(Offset: TDbgPtr; Count: Integer; var Data: array of byte): Integer; override;
    function WriteMem(Offset: TDbgPtr; Count: Integer; const Data: array of byte): Integer; override;
  end;

implementation

function WinDebugProcessStart(const ACommandLine: String): TDbgProcess;
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
end;

destructor TWinDbgProcess.Destroy;  
begin
  inherited Destroy;  
end;

function TWinDbgProcess.GetProcessState: TDbgState;  
begin
  Result := fState;
end;

function TWinDbgProcess.ReadMem(Offset: TDbgPtr; Count: Integer; var Data: array of byte): Integer;  
begin
  Result := ReadProcMem(fProcInfo.hProcess, Offset, Count, Data);
end;

function TWinDbgProcess.WriteMem(Offset: TDbgPtr; Count: Integer; const Data: array of byte): Integer;  
begin
  Result := WriteProcMem(fProcInfo.hProcess, Offset, Count, Data);
end;

procedure TWinDbgProcess.AddThread(ThreadID: TThreadID); 
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

function TWinDbgProcess.Execute(const ACommandLine: String): Boolean;  
begin
  fCmdLine := ACommandLine;
  fState := ds_ReadToRun;
  Result := true;
  
  Result := CreateDebugProcess(ACommandLine, fProcInfo);
  if not Result then Exit;
end;

procedure TWinDbgProcess.Terminate;  
begin
  if fState in [ds_Nonstarted, ds_Terminated] then Exit;
end;

function TWinDbgProcess.WaitNextEvent(var Event: TDbgEvent): Boolean;  
var
  cont  : LongWord;
begin
  if fWaited then begin
    case fLastEvent.dwDebugEventCode of
      EXCEPTION_DEBUG_EVENT: cont := DBG_EXCEPTION_NOT_HANDLED;
      EXCEPTION_BREAKPOINT:  cont := DBG_CONTINUE;
    else
      cont := DBG_CONTINUE;
    end;
    
    try
      with fLastEvent do 
        ContinueDebugEvent(dwProcessId, dwThreadId, Cont);
    except
      writeln('exception while ContinueDebugEvent');
    end;
    
  end;

  if fTerminated then begin
    Result := false;
    Exit;
  end;
  
  FillChar(fLastEvent, sizeof(fLastEvent), 0);
  try
    Result := Windows.WaitForDebugEvent(fLastEvent, INFINITE);
  except
    writeln('exception while WaitForDebugEvent');
  end;
  
  fWaited := Result;
  
  if Result then begin
    WinEventToDbgEvent(fProcInfo.hProcess, fLastEvent, Event);
    case fLastEvent.dwDebugEventCode of
      CREATE_PROCESS_DEBUG_EVENT, CREATE_THREAD_DEBUG_EVENT: 
        AddThread(fLastEvent.dwThreadId);
      EXIT_PROCESS_DEBUG_EVENT, EXIT_THREAD_DEBUG_EVENT: 
        RemoveThread(fLastEvent.dwThreadId);
    end;
  
    fTerminated := (Event.Kind = dek_ProcessTerminated);
  end else
    event.Debug := 'GetLastError = ' + IntToStr(GetLastError);
end;

function TWinDbgProcess.GetThreadsCount: Integer;  
begin
  Result := fThreadsCount;  
end;

function TWinDbgProcess.GetThreadID(AIndex: Integer): TDbgThreadID;  
begin
  if (AIndex < 0) or (AIndex >= fThreadsCount) 
    then Result := 0
    else Result := fThreads[AIndex].id;
end;

function TWinDbgProcess.GetThreadRegs(ThreadID: TDbgThreadID; Regs: TDbgRegisters): Boolean;  
begin
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

