unit winDbgTypes; 

{$mode objfpc}{$H+}

interface

uses
  Windows,
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
    
    fProcInfo   : TProcessInformation;
    fLastEvent      : TDebugEvent;
    fLastEvenThread : TThreadID;
    fLastEventProc  : LongWord;
    fContinueStatus : LongWord;
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

    function GetProcessState: TDbgState; override;
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
begin
  if fWaited then ContinueDebugEvent(fLastEventProc, fLastEvenThread, fContinueStatus);

  if fTerminated then begin
    Result := false;
    Exit;
  end;
  
  FillChar(fLastEvent, sizeof(fLastEvent), 0);
  Result := Windows.WaitForDebugEvent(fLastEvent, INFINITE);
  
  fWaited := Result;
  fContinueStatus := DBG_CONTINUE;
  if Result then begin
    WinEventToDbgEvent(fLastEvent, Event);
    case fLastEvent.dwDebugEventCode of
      CREATE_PROCESS_DEBUG_EVENT, CREATE_THREAD_DEBUG_EVENT: begin
        
        inc(fThreadsCount);
      end;
      EXIT_PROCESS_DEBUG_EVENT, EXIT_THREAD_DEBUG_EVENT: begin
        dec(fThreadsCount);
      end;
    end;
    
    fLastEventProc := fLastEvent.dwProcessId;
    fLastEvenThread := fLastEvent.dwThreadId;
    fTerminated := (Event.Kind = dek_ProcessTerminated);
  end;
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

initialization
  DebugProcessStart := @WinDebugProcessStart;
end.

