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
  
  TWinHandleIDPair = record
    Hnd   : THandle;
    ID    : DWORD;    
    Tag   : TObject;
  end;
  
  { TProcessThreads }

  TProcessThreads = class(TObject)
    Count   : Integer;
    IDs     : array of DWORD;
    Is32Bit : Boolean; // todo:
    procedure AddID(ThreadID: DWORD);
    procedure RemoveID(ThreadID: DWORD);
  end;
  
  { THandleList }

  THandleList = class(TObject)
  private
    fList   : array of TWinHandleIDPair;
    fCount  : Integer;
    procedure DeleteByIndex(i: Integer; FreeTag: Boolean);
    function FindByID(ID: DWORD): Integer;
  public
    procedure Add(AHandle: THandle; ID: DWORD; Tag: TObject);
    procedure DeleteByID(ID: DWORD; FreeTag: Boolean=False);
    procedure DeleteByHandle(AHandle: THandle; FreeTag: Boolean=False);
    function HandleByID(ID: DWORD): THandle;
    function TagByID(ID: DWORD): TObject;
  end;
  

  TWinDbgTarget = class(TDbgTarget)
  private
    fCmdLine  : String;
    fis32proc : Boolean; //todo:
  
    {the root process. If it's terminated the Target is terminated too}
    fMainProc   : TProcessInformation; 
    
    fProcesses  : THandleList;  
    fThreads    : THandleList;
    fLastEvent    : TDebugEvent;
    fWaiting      : Boolean;
    fWaited       : Boolean;
    fTerminated   : Boolean;
    fEHandled     : Boolean;
    
  protected
    procedure AddThread(ThreadID: TThreadID; ThreadHandle: THandle);
    procedure RemoveThread(ThreadID: TThreadID);
    
  public
    constructor Create;
    destructor Destroy; override;

    procedure Terminate; override;
    function WaitNextEvent(var Event: TDbgEvent): Boolean; override;
    
    function GetThreadsCount(procID: TDbgProcessID): Integer; override;
    function GetThreadID(procID: TDbgProcessID; AIndex: Integer): TDbgThreadID; override;
    function GetThreadRegs(procID: TDbgProcessID; ThreadID: TDbgThreadID; Regs: TDbgDataList): Boolean; override;
    function SetThreadRegs(procID: TDbgProcessID; ThreadID: TDbgThreadID; Regs: TDbgDataList): Boolean; override;
    function SetSingleStep(procID: TDbgProcessID; ThreadID: TDbgThreadID): Boolean; override;

    function ReadMem(procID: TDbgProcessID; Offset: TDbgPtr; Count: Integer; var Data: array of byte): Integer; override;
    function WriteMem(procID: TDbgProcessID; Offset: TDbgPtr; Count: Integer; const Data: array of byte): Integer; override;
    
    function StartDebugProcess(const ACommandLine: String; OnlyProcess: Boolean): Boolean; 
  end;

implementation

function WinDebugProcessStart(const ACommandLine: String): TDbgTarget;
var
  win : TWinDbgTarget;
begin
  win := TWinDbgTarget.Create;
  if not win.StartDebugProcess(ACommandLine, True) then begin
    win.Free;
    Result := nil
  end else
    Result := win;
end;


{ TWinDbgTarget }

constructor TWinDbgTarget.Create; 
begin
  fis32proc := True;
end;

destructor TWinDbgTarget.Destroy;  
begin
  inherited Destroy;  
end;

function TWinDbgTarget.ReadMem(procID: TDbgProcessID; Offset: TDbgPtr; Count: Integer; var Data: array of byte): Integer;
var
  hnd : Integer;
begin
  hnd:=fProcesses.FindByID(procID);
  if hnd=0 then Result:=-1
  else Result := ReadProcMem(hnd, Offset, Count, Data);
end;

function TWinDbgTarget.WriteMem(procID: TDbgProcessID; Offset: TDbgPtr; Count: Integer; const Data: array of byte): Integer;
var
  hnd : Integer;
begin
  hnd:=fProcesses.FindByID(procID);
  if hnd=0 then Result:=-1
  else begin
    Result := WriteProcMem(hnd, Offset, Count, Data);
    FlushInstructionCache(hnd, @Offset, Count);
  end;
end;

procedure TWinDbgTarget.AddThread(ThreadID: TThreadID; ThreadHandle: THandle);
begin
  fThreads.Add(ThreadHandle, ThreadID, nil);
end;

procedure TWinDbgTarget.RemoveThread(ThreadID: TThreadID); 
begin
  fThreads.DeleteByID(ThreadID);
end;


function TWinDbgTarget.StartDebugProcess(const ACommandLine: String; OnlyProcess: Boolean): Boolean;  
begin
  fCmdLine := ACommandLine;
  Result := CreateDebugProcess(ACommandLine, OnlyProcess, fMainProc);
  //todo:
  
  if not Result then Exit;
end;

procedure TWinDbgTarget.Terminate;  
begin
end;

function TWinDbgTarget.WaitNextEvent(var Event: TDbgEvent): Boolean;  
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
    Event.Debug := DebugWinEvent( fThreads.FindByID(fLastEvent.dwThreadId), fLastEvent);
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
  
    fTerminated := (Event.Kind = dek_ProcessTerminated) and (fLastEvent.dwProcessId = fMainProc.dwProcessId);
    Event.Debug := '*'+Event.Debug + ' ' + IntToStr(fLastEvent.dwDebugEventCode);
  end else
    event.Debug := 'GetLastError = ' + IntToStr(GetLastError);
end;

function TWinDbgTarget.GetThreadsCount(procID: TDbgProcessID): Integer;
var
  info : TProcessThreads;
begin
  info:=TProcessThreads(fThreads.TagByID(procID));
  if not Assigned(info) then Result:=0
  else Result:=info.Count;
end;

function TWinDbgTarget.GetThreadID(procID: TDbgProcessID; AIndex: Integer): TDbgThreadID;
var
  info : TProcessThreads;
begin
  info:=TProcessThreads(fThreads.TagByID(procID));
  if not Assigned(info) then begin
    Result:=0;
    Exit;
  end;
  if (AIndex < 0) or (AIndex >= info.Count) 
    then Result := 0
    else Result := info.IDs[AIndex];
end;

function TWinDbgTarget.GetThreadRegs(procID: TDbgProcessID; ThreadID: TDbgThreadID; Regs: TDbgDataList): Boolean;
var
  hnd   : THandle;
begin
  Result := false;
 
  { "Thread Handles and Identifiers" http://msdn.microsoft.com/en-us/library/ms686746(VS.85).aspx }
  { Threads are uniquelty identified throughout the system }
  { there's no need to find "a thread of a process"        }
  hnd:=fThreads.HandleByID(threadID);
  if hnd=0 then Exit;
  
  if fis32proc then begin
    Result:=DoReadThreadRegs32(hnd, Regs);
  end else
    Result := false;
end;

function TWinDbgTarget.SetThreadRegs(procID: TDbgProcessID; ThreadID: TDbgThreadID; Regs: TDbgDataList): Boolean;
var
  hnd   : THandle;
begin
  Result := false;
  if fWaiting then Exit;

  hnd:=fThreads.HandleByID(ThreadID);
  if hnd=0 then Exit;
  if fis32proc then 
    Result:=DoWriteThreadRegs32(hnd, Regs)
  else
    Result := false;
end;

function TWinDbgTarget.SetSingleStep(procID: TDbgProcessID; ThreadID: TDbgThreadID): Boolean;
var
  hnd : THandle;
begin
  Result:=false;
  hnd:=fThreads.FindByID(ThreadID);
  if hnd=0 then Exit;
  if fis32proc then 
    Result := SetThread32SingleStep(hnd)
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

{ THandleList }

procedure THandleList.DeleteByIndex(i: Integer; FreeTag: Boolean); 
begin
  if FreeTag then fList[i].Tag.Free;
  if i<fCount-1 then  
    fList[i]:=fList[fCount-1];
  dec(fCount);
end;

function THandleList.FindByID(ID: DWORD): Integer;
var
  i: Integer;
begin
  for i:=0 to fCount-1 do
    if fList[i].ID=ID then begin
      Result:=i;
      Exit;
    end;
  Result:=-1;
end;

procedure THandleList.Add(AHandle: THandle; ID: DWORD; Tag: TObject); 
begin
  if fCount=length(fList) then begin
    if fCount=0 then SetLength(fList, 4)
    else SetLength(fList, fCount*2);
  end;
  fList[fCount].Hnd:=AHandle;
  fList[fCount].ID:=ID;
  fList[fCount].Tag:=Tag;
  inc(fCount);
end;

procedure THandleList.DeleteByID(ID: DWORD; FreeTag: Boolean); 
var
  i : Integer;
begin
  for i:=0 to fCount-1 do
    if fList[i].ID=ID then begin
      DeleteByIndex(i, FreeTag);
      Exit;
    end;
end;

procedure THandleList.DeleteByHandle(AHandle: THandle; FreeTag: Boolean); 
var
  i : Integer;
begin
  for i:=0 to fCount-1 do
    if fList[i].Hnd=AHandle then begin
      DeleteByIndex(i, FreeTag);
      Exit;
    end;
end;

function THandleList.HandleByID(ID: DWORD): THandle; 
var
  i : Integer;
begin
  i:=FindByID(ID);
  if i < 0 then Result:=0
  else Result:=fList[i].Hnd;
end;

function THandleList.TagByID(ID: DWORD): TObject; 
var
  i : Integer;
begin
  i:=FindByID(ID);
  if i < 0 then Result:=nil
  else Result:=fList[i].Tag;
end;

{ TProcessThreads }

procedure TProcessThreads.AddID(ThreadID: DWORD); 
begin
  if Count=length(IDs) then begin
    if Count=0 then SetLength(IDs, 4)
    else SetLength(IDs, Count*2);
  end;
  IDs[Count]:=ThreadID;
  inc(Count);
end;

procedure TProcessThreads.RemoveID(ThreadID: DWORD); 
var
  i : Integer;
begin
  for i:=0 to Count - 1 do
    if IDs[i]=ThreadID then begin
      IDs[i]:=IDs[Count-1];
      dec(Count);
      Exit;
    end;
end;

initialization
  InitWindowsError;
  InitWindowsDebug;
  
end.

