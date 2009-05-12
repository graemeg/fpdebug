unit winDbgTypes; 

{$mode objfpc}{$H+}

interface

uses
  Windows,
  dbgTypes, winDbgProc;
  
type
  { TWinDbgProcess }

  TWinDbgProcess = class(TDbgProcess)
  private
    fState    : TDbgState;
    fCmdLine  : String;
    
    fProcInfo   : TProcessInformation;
    fLastEvent      : TDebugEvent;
    fLastEvenThread : TThreadID;
    fLastEventProc  : LongWord;
    fContinueStatus : LongWord;
    fWaited     : Boolean;
    fTerminated : Boolean;
  protected
    function GetProcessState: TDbgState; override;
  public
    constructor Create;
    destructor Destroy; override;
    
    function Execute(const ACommandLine: String): Boolean; 
    procedure Terminate; override;
    function WaitNextEvent(var Event: TDbgEvent): Boolean; override;
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
    fLastEventProc := fLastEvent.dwProcessId;
    fLastEvenThread := fLastEvent.dwThreadId;
    fTerminated := (Event.Kind = dek_ProcessTerminated);
  end;
  
end;

initialization
  DebugProcessStart := @WinDebugProcessStart;
end.

