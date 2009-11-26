unit dbgMain; 

{$mode objfpc}{$H+}

interface

uses
  contnrs, dbgTypes; 
  
type
  TDbgMain = class;
  TDbgProcess = class;

  TDbgThreadState = (dts_Starting, dts_Running, dts_Terminating);

  { TDbgThread }

  TDbgThread = class(TObject)
  private
    fID     : TDbgThreadID;
    fOwner  : TDbgProcess;
    fState  : TDbgThreadState;
  protected
    function DbgTarget: TDbgTarget; 
  public
    constructor Create(AOwner: TDbgProcess; AID: TDbgThreadID);
    function GetThreadRegs(Registers: TDbgDataList): Boolean; 
    function SetThreadRegs(Registers: TDbgDataList): Boolean; 
    function SetSingleStep: Boolean; 
    property ID: TDbgThreadID read fID;
    property State: TDbgThreadState read fState;
  end;

  TDbgProcessState = (dps_Starting, dps_Running, dps_Terminating);

  { TDbgProcess }

  TDbgProcess  = class(TObject)
  private
    fOwner    : TDbgMain;
    fID       : Integer;
    fProcID   : TDbgProcessID;
    fThreads  : TFPObjectList;
    fState    : TDbgProcessState;

  protected
    function DbgTarget: TDbgTarget; 
    
    procedure AddThread(threadid: Integer);
    procedure RemoveThread(threadid: Integer);
    function GetThread(i: Integer): TDbgThread;
    function GetThreadsCount: Integer;
    
  public
    constructor Create(AOwner: TDbgMain; AProcessID: TDbgProcessID);
    destructor Destroy; override;
    function FindThread(AThreadid: TDbgThreadID): TDbgThread;
    function ReadMem(Offset: TDbgPtr; Count: Integer; var Data: array of byte): Integer; 
    function WriteMem(Offset: TDbgPtr; Count: Integer; const Data: array of byte): Integer;

    property ID: TDbgProcessID read fProcID;
    property State: TDbgProcessState read fState;
    property ThreadsCount: Integer read GetThreadsCount;
    property Thread[i: Integer]: TDbgThread read GetThread;
  end;

  { TMemAccessHandler }

  TMemAccessHandler = class(TObject)
  public
    procedure AfterRead(Proc: TDbgProcessID; var Data: array of byte; Offset: TDbgPtr; Count: Integer); virtual; abstract;
    procedure BeforeWrite(Proc: TDbgProcessID; var Data: array of byte; Offset: TDbgPtr; Count: Integer); virtual; abstract;
  end;

  { TDbgMain }

  TDbgMain = class(TObject)
  private
    fTarget   : TDbgTarget;
    fProcList : TFPObjectList;

    fMemHandlers  : TFPObjectList;
  protected
    function DoAddProcess(AProcessID: Integer): TDbgProcess;
    procedure DoRemoveProcess(AProcessID: Integer);

    function DoReadMem(Proc: TDbgProcess; Offset: TDbgPtr; Count: Integer; var Data: array of byte): Integer;
    function DoWriteMem(Proc: TDbgProcess; Offset: TDbgPtr; Count: Integer; const Data: array of byte): Integer;

    function GetProcessCount: Integer;
    function GetProcess(i: Integer): TDbgProcess;

    procedure UpdateProcThreadState;
    procedure DoHandleEvent(Event: TDbgEvent);
  public
    constructor Create(ATarget: TDbgTarget; AProcessID: TDbgProcessID);
    destructor Destroy; override;
    function WaitForEvent(var Event: TDbgEvent): Boolean;  
    function FindProcess(processID: TDbgProcessID): TDbgProcess;
    function FindThread(processID: TDbgProcessID; ThreadID: TDbgThreadID): TDbgThread;

    procedure RegisterMemHandler(AHandler: TMemAccessHandler);

    property ProcessCount: Integer read GetProcessCount;
    property Process[i: Integer]: TDbgProcess read GetProcess;
  end;

implementation

{ TDbgMain }

function TDbgMain.DoAddProcess(AProcessID: Integer): TDbgProcess;
begin
  Result := TDbgProcess.Create(Self, AProcessID);
  fProcList.Add(Result);
end;

procedure TDbgMain.DoRemoveProcess(AProcessID: Integer); 
var
  proc  : TDbgProcess;
begin
  proc:=FindProcess(AProcessID);
  fProcList.Remove(proc);
end;

function TDbgMain.DoReadMem(Proc: TDbgProcess; Offset: TDbgPtr; Count: Integer;  
  var Data: array of byte): Integer; 
var
  i   : Integer;
begin
  Result := fTarget.ReadMem(Proc.ID, Offset, Count, Data);
  for i:=0 to fMemHandlers.Count-1 do
    TMemAccessHandler(fMemHandlers[i]).AfterRead(Proc.ID, Data, Offset, Count);
end;

function TDbgMain.DoWriteMem(Proc: TDbgProcess; Offset: TDbgPtr;  
  Count: Integer; const Data: array of byte): Integer;
var
  buf : array of byte;
  i   : Integer;
  hnd : TMemAccessHandler;
begin
  //todo: handling wrappers
  if fMemHandlers.Count=0 then
    Result := fTarget.WriteMem(Proc.ID, Offset, Count, Data)
  else begin
    SetLength(buf, Count);
    Move(Data[0], buf[0], Count);
    for i:=0 to fMemHandlers.Count-1 do begin
      hnd:=TMemAccessHandler(fMemHandlers[i]);
      hnd.BeforeWrite(Proc.ID, buf, Offset, Count);
    end;
    Result:=fTarget.WriteMem(Proc.ID, Offset,Count, Buf);
  end;
end;

function TDbgMain.GetProcessCount: Integer; 
begin
  Result:=fProcList.Count;
end;

function TDbgMain.GetProcess(i: Integer): TDbgProcess; 
begin
  Result:=TDbgProcess(fProcList[i]);
end;

procedure TDbgMain.UpdateProcThreadState;
var
  i,j : Integer;
  p   : TDbgProcess;
  t   : TDbgThread;
begin
  for i:=ProcessCount-1 downto 0 do begin
    p:=Process[i];

    case p.State of
      dps_Starting: p.fState:=dps_Running;
    end;

    for j:=p.ThreadsCount-1 downto 0 do begin
      t:=p.Thread[j];
      case t.fState of
        dts_Starting: t.fState:=dts_Running;
        dts_Terminating: p.RemoveThread(t.ID);
      end;
    end;
  end;
end;

procedure TDbgMain.DoHandleEvent(Event: TDbgEvent);
var
  proc  : TDbgProcess;
  thr   : TDbgThread;
begin
  case Event.Kind of
    dek_ProcessStart:
      DoAddProcess(Event.Process).AddThread(Event.Thread);
    dek_ThreadStart: begin
      proc:=FindProcess(Event.Process);
      if not Assigned(proc) then proc:=DoAddProcess(event.Process);
      proc.AddThread(event.Thread);
    end;
    dek_ThreadTerminated: begin
      thr:=FindThread(event.Process, event.Thread);
      thr.fState:=dts_Terminating;
    end;

  end;
end;

constructor TDbgMain.Create(ATarget: TDbgTarget; AProcessID: TDbgProcessID); 
begin
  inherited Create;
  fTarget:=ATarget;
  fProcList:=TFPObjectList.Create;
  fMemHandlers:=TFPObjectList.Create(false);
  DoAddProcess(AProcessID);
end;

destructor TDbgMain.Destroy;  
begin
  fMemHandlers.Free;
  fProcList.Free;
  inherited Destroy;  
end;

function TDbgMain.WaitForEvent(var Event: TDbgEvent): Boolean;  
begin
  UpdateProcThreadState;
  Result:=fTarget.WaitNextEvent(Event);
  if Result then DoHandleEvent(Event);
end;

function TDbgMain.FindProcess(processID: TDbgProcessID): TDbgProcess; 
var
  i : integer;
begin
  for i:=0 to ProcessCount-1 do 
    if Process[i].ID=processID then begin
      Result:=Process[i];
      Exit;
    end;
  Result:=nil;
end;

function TDbgMain.FindThread(processID: TDbgProcessID; ThreadID: TDbgThreadID): TDbgThread; 
var
  prc : TDbgProcess;
begin
  Result:=nil;
  prc:=FindProcess(processID);
  if not Assigned(prc) then Exit;
  Result:=prc.FindThread(ThreadID);
end;

procedure TDbgMain.RegisterMemHandler(AHandler: TMemAccessHandler);
begin
  fMemHandlers.Add(AHandler);
end;

{ TDbgProcess }

constructor TDbgProcess.Create(AOwner: TDbgMain; AProcessID: TDbgProcessID); 
var
  i     : Integer;
  thrid : TDbgThreadID;
begin
  inherited Create;
  
  fOwner:=AOwner;
  fProcID:=AProcessID;
  fThreads:=TFPObjectList.Create(true);
  
  {filling existing threads}
  for i:=0 to DbgTarget.GetThreadsCount(fID) - 1 do begin
    thrid:=DbgTarget.GetThreadID(fID, i);
    AddThread(thrid);
  end;
end;

destructor TDbgProcess.Destroy;  
begin
  fThreads.Free;
  inherited Destroy;  
end;

function TDbgProcess.DbgTarget: TDbgTarget; 
begin
  Result:=fOwner.fTarget;
end;

procedure TDbgProcess.AddThread(threadid: Integer); 
begin
  fThreads.Add(TDbgThread.Create(Self, threadid));
end;

procedure TDbgProcess.RemoveThread(threadid: Integer); 
var
  t: TDbgThread;
begin
  t:=FindThread(threadid);
  fThreads.Remove(t);
end;

function TDbgProcess.GetThread(i: Integer): TDbgThread; 
begin
  Result:=TDbgThread(fThreads[i]);
end;

function TDbgProcess.GetThreadsCount: Integer; 
begin
  Result:=fThreads.Count;
end;

function TDbgProcess.FindThread(AThreadID: TDbgThreadID): TDbgThread; 
var
  i: Integer;
begin
  for i:=0 to fThreads.Count-1 do 
    if Thread[i].ID = athreadid then begin
      Result:=Thread[i];
      Exit;
    end;
  Result:=nil;
end;

function TDbgProcess.ReadMem(Offset: TDbgPtr; Count: Integer; var Data: array of byte): Integer;
begin
  Result:=fOwner.DoReadMem(Self, Offset, Count, Data);
end;

function TDbgProcess.WriteMem(Offset: TDbgPtr; Count: Integer; const Data: array of byte): Integer;
begin
  Result:=fOwner.DoWriteMem(Self, Offset, Count, Data);
end;

{ TDbgThread }

constructor TDbgThread.Create(AOwner: TDbgProcess; AID: TDbgThreadID); 
begin
  inherited Create;
  fOwner:=AOwner;
  fID:=AID;
  fState:=dts_Starting;
end;

function TDbgThread.DbgTarget: TDbgTarget; 
begin
  Result:=fOwner.DbgTarget;
end;

function TDbgThread.GetThreadRegs(Registers: TDbgDataList): Boolean; 
begin
  Result:=DbgTarget.GetThreadRegs(fOwner.ID, fID, Registers);
end;

function TDbgThread.SetThreadRegs(Registers: TDbgDataList): Boolean; 
begin
  Result:=DbgTarget.GetThreadRegs(fOwner.ID, fID, Registers);
end;

function TDbgThread.SetSingleStep: Boolean; 
begin
  Result:=DbgTarget.SetSingleStep(fOwner.ID, fID);
end;

end.

