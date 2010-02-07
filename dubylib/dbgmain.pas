unit dbgMain; 

{$mode objfpc}{$H+}

interface                      

uses
  Classes, contnrs, dbgTypes, dbgCPU; 
  
type
  TDbgMain = class;
  TDbgProcess = class;

  TDbgThreadState = (dts_Starting, dts_Running, dts_Terminating);

  { TDbgThread }

  // process thread wrapper
  
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
    function NextSingleStep: Boolean; 
    property ID: TDbgThreadID read fID;
    property State: TDbgThreadState read fState;
  end;

  TDbgProcessState = (dps_Starting, dps_Running, dps_Terminating);

  TDbgHandlerEvent = procedure (Sender: TObject; Addr: TDbgPtr; var NeedStopWithBreak: Boolean) of object;
  
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
    //   Tags   : TList; // process relative information: Breakpoints, DebugInfo... etc
    
    constructor Create(AOwner: TDbgMain; AProcessID: TDbgProcessID);
    destructor Destroy; override;

    function FindThread(AThreadid: TDbgThreadID): TDbgThread;
    function ReadMem(Offset: TDbgPtr; Count: Integer; var Data: array of byte): Integer; 
    function WriteMem(Offset: TDbgPtr; Count: Integer; const Data: array of byte): Integer;

    function EnabledBreakpoint(const Addr: TDbgPtr): Boolean;
    procedure DisableBreakpoint(const Addr: TDbgPtr);
    function AddBreakHandler(const Addr: TDbgPtr; Handler: TDbgHandlerEvent): Boolean;
    procedure RemoveBreakHandler(const Addr: TDbgPtr; Handler: TDbgHandlerEvent);

    property ID: TDbgProcessID read fProcID;
    property State: TDbgProcessState read fState;
    property ThreadsCount: Integer read GetThreadsCount;
    property Thread[i: Integer]: TDbgThread read GetThread;
  end;

  { TMemAccessHandler }

  TMemAccessEvent = procedure (Proc: TDbgProcessID; var Data: array of byte; Offset: TDbgPtr; Count: Integer) of object;
  
  THandleState = (ehs_NotHandled, ehs_Handled, ehs_HandledIsBreakPointEvent);
  
  TDbgHandleEvent = procedure (const Event: TDbgEvent; var EventHandled: THandleState) of object;

  TDbgPointHandler = procedure (Sender: TObject; Addr: TDbgPtr; var Handled: Boolean) of object;

  { TDbgBreakWatch }

  TRawBreakpoint = class(TObject)
  public
    procedure Enable; virtual; abstract;
    procedure Disable; virtual; abstract;
  end;
  
  TDbgBreakWatch = class(TObject)
  private
    fAddr       : TDbgPtr;
    fEnabled    : Boolean;
    fOwner      : TDbgMain;
    fRaw        : TRawBreakpoint;    
    fRawEnabled : Boolean;
    Handlers    : array of TDbgPointHandler;
    Count       : Integer;
    procedure SetEnabled(AEnabled: Boolean);
    procedure RefreshRaw;
  public
    constructor Create(AOwner: TDbgMain; AAddr: TDbgPtr);
    destructor Destroy; override;
    procedure NotifyHandlers(var NeedToStop: Boolean);
    procedure AddHandler(AHandleEvent: TDbgPointHandler);
    procedure RemoveHandler(AHandleEvent: TDbgPointHandler);
    property Addr: TDbgPtr read fAddr;
    property Enabled: Boolean read fEnabled write SetEnabled;
  end;
  
  { TDbgMain }

  TDbgMain = class(TObject)
  private
    fTarget   : TDbgTarget;
    fProcList : TFPObjectList;

    fReadHandlers  : TFPObjectList;
    fWriteHandlers : TFPObjectList;
    fEventHandlers : TFPObjectList;
  protected
    function DoAddProcess(AProcessID: Integer): TDbgProcess;
    procedure DoRemoveProcess(AProcessID: Integer);

    function DoReadMem(Proc: TDbgProcess; Offset: TDbgPtr; Count: Integer; var Data: array of byte): Integer;
    function DoWriteMem(Proc: TDbgProcess; Offset: TDbgPtr; Count: Integer; const Data: array of byte): Integer;

    function GetProcessCount: Integer;
    function GetProcess(i: Integer): TDbgProcess;

    procedure UpdateProcThreadState;
    procedure DoHandleEvent(Event: TDbgEvent);

    procedure AddReadHandler(AHandler: TMemAccessEvent);
    procedure AddWriteHandler(AHandler: TMemAccessEvent);
    procedure AddEventHandler(AHandle: TDbgHandleEvent);
  public
    constructor Create(ATarget: TDbgTarget; AProcessID: TDbgProcessID);
    destructor Destroy; override;
    function WaitNextEvent(var Event: TDbgEvent): Boolean;  
    function FindProcess(processID: TDbgProcessID): TDbgProcess;
    function FindThread(processID: TDbgProcessID; ThreadID: TDbgThreadID): TDbgThread;

    function CPU: TCPUCode;
    property ProcessCount: Integer read GetProcessCount;
    property Process[i: Integer]: TDbgProcess read GetProcess;
  end;
  
  
  { TDbgBreak }

  TDbgBreak = class(TObject)
    ProcID    : TDbgProcessID;
    Addr      : TDbgPtr;
    Buf       : array of byte;
    RefCount  : Integer;
    Handler   : TDbgPointHandler;
    constructor Create(AProcID: TDbgProcessID; AAddr: TDbgPtr);
  end;
  
  { TDbgMainBreakpoints }

  TDbgMainBreakpoints = class(TObject)
  private
    fMain   : TDbgMain;
    fBPList : TFPList;
    
    function InstallBreak(process: TDbgProcess; addr: TDbgPtr; var buf: array of byte; CodeSize: Integer): Boolean;
    procedure RemoveBreak(process: TDbgProcess; addr: TDbgPtr; const buf: array of byte; CodeSize: Integer);
    
    procedure BeforeProcWrite(Proc: TDbgProcessID; var Data: array of byte; Offset: TDbgPtr; Count: Integer);
    procedure AfterProcRead(Proc: TDbgProcessID; var Data: array of byte; Offset: TDbgPtr; Count: Integer);
    procedure HandleDbgEvent(const Event: TDbgEvent; var EventHandled: THandleState);
    
    function FindBreakPoint(processID: TDbgProcessID; addr: TDbgPtr): TDbgBreak;
  public
    constructor Create(AMain: TDbgMain);
    destructor Destroy; override;
    function AddBreakpoint(process: TDbgProcess; addr: TDbgPtr): Boolean; overload;
    function AddBreakpoint(process: TDbgProcess; addr: TDbgPtr; Handler: TDbgPointHandler): Boolean; overload;
    procedure RemoveBreakpoint(process: TDbgProcess; addr: TDbgPtr);
  end;

implementation

{var
  memallocs   : TFPList;
  eventallos  : TFPList;

procedure RegisterMemHandler(allocator: TEventHandlerAlloctor);
begin
  if not Assigned(allocator) or (memallocs.IndexOf(@allocator) >=0)then Exit;
  memallocs.Add(allocator);
end;

procedure RegisterEventHandler(allocator: TEventHandlerAlloctor);
begin
  if not Assigned(allocator) or (eventallos.IndexOf(@allocator) >=0)then Exit;
  eventallos.Add(allocator);
end;
}

type
  { TMemAccessObject }
  
  TMemAccessObject = class(TObject)
  public
    event: TMemAccessEvent;
    constructor Create(AEvent: TMemAccessEvent);
    procedure CallEvent(Proc: TDbgProcessID; var Data: array of byte; Offset: TDbgPtr; Count: Integer);
  end;
  
  { TDbgHandleObject }
  
  TDbgHandleObject = class(TObject)
    event: TDbgHandleEvent;
    constructor Create(AEvent: TDbgHandleEvent);
    procedure CallEvent(const AEvent: TDbgEvent; var EventHandled: THandleState);
  end;


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
  for i:=0 to fReadHandlers.Count-1 do
    TMemAccessObject(fReadHandlers[i]).CallEvent(Proc.ID, Data, Offset, Count);
end;

function TDbgMain.DoWriteMem(Proc: TDbgProcess; Offset: TDbgPtr;  
  Count: Integer; const Data: array of byte): Integer;
var
  buf : array of byte;
  i   : Integer;
begin
  //todo: handling wrappers
  if fWriteHandlers.Count=0 then
    Result := fTarget.WriteMem(Proc.ID, Offset, Count, Data)
  else begin
    SetLength(buf, Count);
    Move(Data[0], buf[0], Count);
    for i:=0 to fWriteHandlers.Count-1 do 
      TMemAccessObject(fWriteHandlers[i]).CallEvent(Proc.ID, buf, Offset, Count);
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
  
  fReadHandlers  := TFPObjectList.Create(true);
  fWriteHandlers := TFPObjectList.Create(true);
  fEventHandlers := TFPObjectList.Create(true);
    
  DoAddProcess(AProcessID);
end;

destructor TDbgMain.Destroy;  
begin
  fReadHandlers.Free;
  fWriteHandlers.Free;
  fEventHandlers.Free;
  
  fProcList.Free;
  inherited Destroy;  
end;

function TDbgMain.WaitNextEvent(var Event: TDbgEvent): Boolean;  
var
  loopdone : Boolean;
  hnd      : TDbgHandleObject;
  i        : Integer;
  handled  : THandleState;
begin
  UpdateProcThreadState;
  
  repeat
    Result:=fTarget.WaitNextEvent(Event);
    if Result then DoHandleEvent(Event);

    loopdone:=True;
    for i := 0 to fEventHandlers.Count-1 do begin
      hnd:=TDbgHandleObject(fEventHandlers[i]);
      handled:=ehs_NotHandled;
      hnd.CallEvent(Event, handled);
      case handled of
        ehs_Handled: 
          loopdone:=Event.Kind=dek_BreakPoint;
        ehs_HandledIsBreakPointEvent: begin
          loopdone:=event.Kind in [dek_SingleStep, dek_BreakPoint];
          if loopdone then event.Kind:=dek_BreakPoint;
        end;
      end;
    end;

  until loopdone;
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

function TDbgMain.CPU: TCPUCode; 
begin
  Result:=dbgCPU.CPUCode;
end;

procedure TDbgMain.AddReadHandler(AHandler: TMemAccessEvent);
begin
  fReadHandlers.Add(TMemAccessObject.Create(AHandler));
end;

procedure TDbgMain.AddWriteHandler(AHandler: TMemAccessEvent);
begin
  fWriteHandlers.Add(TMemAccessObject.Create(AHandler));
end;

procedure TDbgMain.AddEventHandler(AHandle: TDbgHandleEvent);
begin
  fEventHandlers.Add(TDbgHandleObject.Create(AHandle));
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

function TDbgProcess.EnabledBreakpoint(const Addr: TDbgPtr): Boolean; 
begin

end;

procedure TDbgProcess.DisableBreakpoint(const Addr: TDbgPtr); 
begin

end;

function TDbgProcess.AddBreakHandler(const Addr: TDbgPtr;  
  Handler: TDbgHandlerEvent): Boolean; 
begin

end;

procedure TDbgProcess.RemoveBreakHandler(const Addr: TDbgPtr;  
  Handler: TDbgHandlerEvent); 
begin

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

function TDbgThread.NextSingleStep: Boolean; 
begin
  Result:=DbgTarget.SetSingleStep(fOwner.ID, fID);
end;


{procedure InitAllocs;
begin
  memallocs.Free;
  eventallos.Free;
end;

procedure ReleaseAllocs;
begin
  memallocs.Free;
  eventallos.Free;
end;
}

{ TDbgMainBreakpoints }

function  TDbgMainBreakpoints.InstallBreak(process: TDbgProcess; addr: TDbgPtr;  
  var buf: array of byte; CodeSize: Integer): Boolean; 
var
  temp  : array of byte;
begin
  Result:=(fMain.CPU.BreakPointSize<=CodeSize);
  
  process.ReadMem(addr, CodeSize, buf);
  {checking if hard breakpoint is written}
  if fMain.CPU.IsBreakPoint(buf, 0) then begin
    Result:=false;
    Exit;
  end;
  {reusing the original buffer (in case the original content is critical)}
  SetLength(temp, fMain.CPU.BreakPointSize);
  move(buf[0], temp[0], fMain.CPU.BreakPointSize);
  {writing breakpoint code to the buffer}
  fMain.CPU.WriteBreakPoint(temp, 0);
  {writing break point to process address space}
  process.WriteMem(addr, CodeSize, buf);
  
  Result:=true;
end;

procedure TDbgMainBreakpoints.RemoveBreak(process: TDbgProcess; addr: TDbgPtr;
  const buf: array of byte; CodeSize: Integer); 
begin
  process.WriteMem(addr, Codesize, buf);
end;

procedure TDbgMainBreakpoints.BeforeProcWrite(Proc: TDbgProcessID;  
  var Data: array of byte; Offset: TDbgPtr; Count: Integer); 
begin

end;

procedure TDbgMainBreakpoints.AfterProcRead(Proc: TDbgProcessID;  
  var Data: array of byte; Offset: TDbgPtr; Count: Integer); 
begin

end;

procedure TDbgMainBreakpoints.HandleDbgEvent(const Event: TDbgEvent;  
  var EventHandled: THandleState); 
begin

end;

function TDbgMainBreakpoints.FindBreakPoint(processID: TDbgProcessID; addr: TDbgPtr): TDbgBreak; 
var
  i : Integer;
begin
  for i:=0 to fBPList.Count-1 do begin
    
  end;
end;

constructor TDbgMainBreakpoints.Create(AMain: TDbgMain); 
begin
  inherited Create;
  fMain:=AMain;
  fBPList := TFPList.Create;
  
  fMain.AddWriteHandler(@BeforeProcWrite);
  fMain.AddReadHandler(@AfterProcRead);
  fMain.AddEventHandler(@HandleDbgEvent);
end;

destructor TDbgMainBreakpoints.Destroy;
begin
  fBPList.Free;
end;

function TDbgMainBreakpoints.AddBreakpoint(process: TDbgProcess; addr: TDbgPtr): Boolean; 
begin
  Result:=AddBreakpoint(process, addr, nil);
end;

function TDbgMainBreakpoints.AddBreakpoint(process: TDbgProcess;  
  addr: TDbgPtr; Handler: TDbgPointHandler): Boolean; 
begin
  Result:=false;
  if not Assigned(process) then Exit;
    
  Result:=true;
end;

procedure TDbgMainBreakpoints.RemoveBreakpoint(process: TDbgProcess; addr: TDbgPtr); 
begin

end;


{ TMemAccessObject }

constructor TMemAccessObject.Create(AEvent: TMemAccessEvent); 
begin
  inherited Create;
  event:=AEvent;
end;

procedure TMemAccessObject.CallEvent(Proc: TDbgProcessID;  
  var Data: array of byte; Offset: TDbgPtr; Count: Integer); 
begin
  if Assigned(event) then event(Proc, Data, Offset, Count);
end;

{ TDbgHandleObject }

constructor TDbgHandleObject.Create(AEvent: TDbgHandleEvent); 
begin
  inherited Create;
  event:=AEvent;
end;

procedure TDbgHandleObject.CallEvent(const AEvent: TDbgEvent;  
  var EventHandled: THandleState); 
begin
  if Assigned(event) then event(AEvent, EventHandled);
end;

{ TDbgBreak }

constructor TDbgBreak.Create(AProcID: TDbgProcessID; AAddr: TDbgPtr); 
begin
  inherited Create;
  ProcID:=AProcID;
  Addr:=AAddr;
end;

{ TDbgBreakWatch }

procedure TDbgBreakWatch.SetEnabled(AEnabled: Boolean); 
begin
  if fEnabled=AEnabled then Exit;
  fEnabled:=AEnabled;
  RefreshRaw;
end;

procedure TDbgBreakWatch.RefreshRaw; 
var
  WantedRawState : Boolean;
begin
  WantedRawState := fEnabled or (Count>0);
  if WantedRawState = fRawEnabled then Exit;
  fRawEnabled:=WantedRawState;
  if fRawEnabled then fRaw.Enable else fRaw.Disable;
end;

constructor TDbgBreakWatch.Create(AOwner: TDbgMain; AAddr: TDbgPtr); 
begin
  inherited Create;
  fAddr:=AAddr; 
  fOwner:=AOwner;
end;

destructor TDbgBreakWatch.Destroy;  
begin
  inherited Destroy;  
end;

procedure TDbgBreakWatch.NotifyHandlers(var NeedToStop: Boolean); 
var
  i       : Integer;
  doStop  : Boolean;
begin
  NeedToStop:=False;
  for i:=0 to Count-1 do
    try
      doStop:=false;
      Handlers[i](fOwner, fAddr, NeedToStop);
      NeedToStop:=NeedToStop or doStop;
    except
    end;
end;

procedure TDbgBreakWatch.AddHandler(AHandleEvent: TDbgPointHandler); 
var
  i: Integer;
begin
  for i:=0 to Count-1 do
    if Handlers[i] = AHandleEvent then Exit;
  if length(Handlers)=Count then begin
    if Count=0 then SetLength(Handlers, 4)
    else SetLength(Handlers,Count*2);
  end;
  Handlers[Count]:=AHandleEvent;
  inc(Count);
  RefreshRaw;
end;

procedure TDbgBreakWatch.RemoveHandler(AHandleEvent: TDbgPointHandler); 
var
  i,  j : Integer;
begin
  for i:=0 to Count-1 do
    if Handlers[i]=AHandleEvent then begin
      for j:=i to count - 2 do
        Handlers[j]:=Handlers[j+1];
      dec(Count);
      Exit;
    end;
  RefreshRaw;
end;

initialization
  //InitAllocs;

finalization
  //ReleaseAllocs;

end.

