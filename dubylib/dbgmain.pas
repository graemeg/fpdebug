unit dbgMain;

{$mode objfpc}{$H+}

interface                      

uses
  Classes, contnrs, dbgTypes, dbgCPU, dbgUtils;
  
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
    // process waits for the thread for single step to RESTORE breakpoint
    // ProcBreakAddr - is the address of breakpoint to be restores
    ProcWaitStep    : Boolean;
    ProcBreakAddr   : TDbgPtr;

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

  TDbgBreakPointHandler = procedure (Sender: TObject; Addr: TDbgPtr; var Handled: Boolean) of object;

  { TRawCPUBreakpoint }

  TRawCPUBreakpoint = class(TObject)
  private
    fProcID : TDbgProcessID;
    fTarget : TDbgTarget;
    fAddr   : TDbgPtr;
    buf     : array of Byte;
  public
    constructor Create(AAddr: TDbgPtr; AProcID: TDbgProcessID; ATarget: TDbgTarget);
    function Enable: Boolean;
    procedure Disable;
    function isEnabled: Boolean;
  end;

  { TDbgBreakpoint }

  TDbgBreakpoint = class(TObject)
  private
    fAddr       : TDbgPtr;
    fEnabled    : Boolean;
    fOwner      : TDbgProcess;
    fRaw        : TRawCPUBreakpoint;
    fRawEnabled : Boolean;
    Handlers    : array of TDbgBreakPointHandler;
    Count       : Integer;

    procedure SetEnabled(AEnabled: Boolean);
    procedure RefreshRaw;
  public
    constructor Create(AOwner: TDbgProcess; AAddr: TDbgPtr);
    destructor Destroy; override;
    procedure NotifyHandlers(var NeedToStop: Boolean);
    procedure AddHandler(AHandleEvent: TDbgBreakPointHandler);
    procedure RemoveHandler(AHandleEvent: TDbgBreakPointHandler);
    function isEnabledOrHandled: Boolean;
    property Addr: TDbgPtr read fAddr;
    function Enable: Boolean;
    procedure Disable;
    property isEnabled: Boolean read fEnabled;
  end;

  { TDbgProcess }

  TDbgProcess  = class(TObject)
  private
    fOwner    : TDbgMain;
    fID       : TDbgProcessID;
    fThreads  : TFPObjectList;
    fState    : TDbgProcessState;
    fBreaks   : TFPObjectList;
  protected
    function DbgTarget: TDbgTarget;

    procedure AddThread(threadid: TDbgThreadID);
    procedure RemoveThread(threadid: TDbgThreadID);
    function GetThread(i: Integer): TDbgThread;
    function GetThreadsCount: Integer;

    function FindBreakpoint(Addr: TDbgPtr; Forced: Boolean): TDbgBreakpoint;

    procedure UninstallBreaks(Addr, Count: TDbgPtr; BPList: TFPList);
    procedure ReinstallBreaks(Addr, Count: TDbgPtr; BPList: TFPList);

    procedure HandleBreakpointEvent(const Event: TDbgEvent; var StartedSingleStep: Boolean);
    procedure HandleManualStep(const Event: TDbgEvent; var ReportToUser: Boolean);
  public
    constructor Create(AOwner: TDbgMain; AProcessID: TDbgProcessID);
    destructor Destroy; override;

    function FindThread(AThreadid: TDbgThreadID): TDbgThread;
    function ReadMem(Offset: TDbgPtr; Count: Integer; var Data: array of byte): Integer; 
    function WriteMem(Offset: TDbgPtr; Count: Integer; const Data: array of byte): Integer;

    function EnableBreakpoint(const Addr: TDbgPtr): Boolean;
    procedure DisableBreakpoint(const Addr: TDbgPtr);
    function AddBreakHandler(const Addr: TDbgPtr; Handler: TDbgHandlerEvent): Boolean;
    procedure RemoveBreakHandler(const Addr: TDbgPtr; Handler: TDbgHandlerEvent);

    property ID: TDbgProcessID read fID;
    property State: TDbgProcessState read fState;
    property ThreadsCount: Integer read GetThreadsCount;
    property Thread[i: Integer]: TDbgThread read GetThread;
  end;

  { TMemAccessHandler }

  TMemAccessEvent = procedure (Proc: TDbgProcessID; var Data: array of byte; Offset: TDbgPtr; Count: Integer) of object;
  
  THandleState = (ehs_NotHandled, ehs_Handled, ehs_HandledIsBreakPointEvent);
  
  TDbgHandleEvent = procedure (const Event: TDbgEvent; var EventHandled: THandleState) of object;

  { TDbgMain }

  TDbgMain = class(TObject)
  private
    fTarget   : TDbgTarget;
    fProcList : TFPObjectList;

    fReadHandlers  : TFPObjectList;
    fWriteHandlers : TFPObjectList;
    fEventHandlers : TFPObjectList;

    //the list of the stepper processes, for multi-process debugging
    fSteppers   : TFPList;
    fStepper    : TDbgProcess; // the process that must make a SINGLE step in SINGLE thread
    fStepThread : TDbgThreadID;

  protected
    function DoAddProcess(AProcessID: TDbgProcessID): TDbgProcess;
    procedure DoRemoveProcess(AProcessID: TDbgProcessID);

    function GetProcessCount: Integer;
    function GetProcess(i: Integer): TDbgProcess;

    procedure UpdateProcThreadState;
    procedure DoHandleEvent(Event: TDbgEvent; var ReportToUser: Boolean);

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
  
// utility function
function SetThreadExecAddr(athread: TDbgThread; const Addr: TDbgPtr): Boolean;

implementation

function SetThreadExecAddr(athread: TDbgThread; const Addr: TDbgPtr): Boolean;
var
  datalist  : TDbgDataBytesList;
begin
  datalist:=TDbgDataBytesList.Create;
  try
    Result:=Assigned(athread) and (athread.GetThreadRegs(datalist));
    if Result then begin
      datalist.Reg[CPUCode.ExecuteRegisterName].DbgPtr:=Addr;
      Result:=athread.SetThreadRegs(datalist);
    end;
  finally
    datalist.Free;
  end;
end;

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

{ TRawCPUBreakpoint }

constructor TRawCPUBreakpoint.Create(AAddr: TDbgPtr; AProcID: TDbgProcessID; ATarget: TDbgTarget);
begin
  inherited Create;
  fAddr:=AAddr;
  fProcID:=AProcID;
  fTarget:=ATarget;
end;

function TRawCPUBreakpoint.Enable: Boolean;
var
  temp      : array of byte;
  CodeSize  : Integer;
begin
  if length(buf)<>0 then begin
    {already enabled!}
    Result:=True;
    Exit;
  end;

  Codesize:=CPUCode.BreakPointSize;
  SetLength(buf, Codesize);

  fTarget.ReadMem(fProcID, fAddr, CodeSize, buf);
  {checking if hard breakpoint is written}
  if CPUCode.IsBreakPoint(buf, 0) then begin
    SetLength(buf, 0);
    Result:=False;
    Exit;
  end;

  {reusing the original buffer (in case the original content is critical)}
  SetLength(temp, CodeSize);
  move(buf[0], temp[0], CodeSize);
  {writing breakpoint code to the buffer}
  CPUcode.WriteBreakPoint(temp, 0);
  {writing break point to process address space}
  Result:=fTarget.WriteMem(fProcID, fAddr, CodeSize, temp)=CodeSize;
  if not Result then
    SetLength(buf, 0);
end;

procedure TRawCPUBreakpoint.Disable;
var
  res : Integer;
begin
  res:=fTarget.WriteMem(fProcID, fAddr, length(buf), buf);
  SetLength(buf,0);
end;

function TRawCPUBreakpoint.isEnabled:Boolean;
begin
  Result:=length(buf)>0;
end;

{ TDbgMain }

function TDbgMain.DoAddProcess(AProcessID: TDbgProcessID): TDbgProcess;
begin
  Result := TDbgProcess.Create(Self, AProcessID);
  fProcList.Add(Result);
end;

procedure TDbgMain.DoRemoveProcess(AProcessID: TDbgProcessID);
var
  proc  : TDbgProcess;
begin
  proc:=FindProcess(AProcessID);
  fProcList.Remove(proc);
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

procedure TDbgMain.DoHandleEvent(Event: TDbgEvent; var ReportToUser: Boolean);
var
  proc  : TDbgProcess;
  thr   : TDbgThread;

  DoSingle  : Boolean;
  i : Integer;
begin
  ReportToUser:=True;
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
    dek_BreakPoint: begin
      proc:=FindProcess(event.Process);
      proc.HandleBreakpointEvent(Event, DoSingle);

      //Note: if DoSingle step returns "true", then all other threads in the process
      //MUST BE suspended or other thread my jump through the disabled breakpoint
      if DoSingle then begin
        fStepper:=FindProcess(event.Process);
        fStepThread:=event.Thread;
      end;
    end;
    dek_SingleStep: begin
      proc:=FindProcess(event.Process);
      i:=fSteppers.IndexOf(proc);
      if i>=0 then begin
        proc.HandleManualStep(event, ReportToUser);
        fSteppers.Delete(i);
      end;

    end;
  end;
end;

constructor TDbgMain.Create(ATarget: TDbgTarget; AProcessID: TDbgProcessID); 
begin
  inherited Create;
  fTarget:=ATarget;
  fProcList:=TFPObjectList.Create;
  fSteppers:=TFPList.Create;
  
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
  fSteppers.Free;
  inherited Destroy;  
end;

function TDbgMain.WaitNextEvent(var Event: TDbgEvent): Boolean;  
var
  loopdone : Boolean;
  i        : Integer;
  ReportToUser  : Boolean;
begin
  repeat
    loopdone:=False;
    UpdateProcThreadState;

    if Assigned(fStepper) then begin
      if Assigned(fStepper.FindThread(fStepThread)) then begin
        fSteppers.Add(fStepper);
        for i:=0 to fStepper.ThreadsCount-1 do begin
          if fStepper.Thread[i].ID<>fStepThread then
            //todo: suspend thread
            {fStepper.Thread[i].Suspend;}
            ;
        end;
      end else
        // stepping thread wad removed... for some reason?!
        // can't step the process
        ;
      fStepper:=nil;
    end;
  
    Result:=fTarget.WaitNextEvent(Event);

    if Result then DoHandleEvent(Event, ReportToUser);

    if ReportToUser then loopdone:=True;

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
  fID := AProcessID;
  fThreads:=TFPObjectList.Create(true);
  fBreaks:=TFPObjectList.Create(true);

  {filling existing threads}
  for i:=0 to DbgTarget.GetThreadsCount(fID) - 1 do begin
    thrid:=DbgTarget.GetThreadID(fID, i);
    AddThread(thrid);
  end;
end;

destructor TDbgProcess.Destroy;  
begin
  fThreads.Free;
  fBreaks.Free;
  inherited Destroy;  
end;

function TDbgProcess.DbgTarget: TDbgTarget; 
begin
  Result:=fOwner.fTarget;
end;

procedure TDbgProcess.AddThread(threadid: TDbgThreadID);
begin
  fThreads.Add(TDbgThread.Create(Self, threadid));
end;

procedure TDbgProcess.RemoveThread(threadid: TDbgThreadID);
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

function TDbgProcess.FindBreakpoint(Addr:TDbgPtr; Forced: Boolean):TDbgBreakpoint;
var
  i : Integer;
begin
  for i:=0 to fBreaks.Count-1 do begin
    Result:=TDbgBreakpoint(fBreaks[i]);
    if Result.Addr=Addr then Exit;
  end;
  Result:=nil;
  if not Assigned(Result) and Forced then begin
    Result:=TDbgBreakpoint.Create(Self, Addr);
    Result.fRaw:=TRawCPUBreakpoint.Create(Addr, fID, fOwner.fTarget);
    fBreaks.Add(Result);
  end;
end;

procedure TDbgProcess.UninstallBreaks(Addr,Count:TDbgPtr; BPList: TFPList);
var
  i   : integer;
  raw : TRawCPUBreakpoint;
begin
  for i:=0 to fBreaks.Count-1 do begin
    raw:=TRawCPUBreakpoint(fBreaks[i]);
    if (raw.fAddr>=Addr) and (raw.fAddr<Addr+Count) and raw.isEnabled then begin
      BPList.Add(raw);
      raw.Disable;
    end;
  end;
end;

procedure TDbgProcess.ReinstallBreaks(Addr,Count:TDbgPtr; BPList:TFPList);
var
  i   : Integer;
  raw : TRawCPUBreakpoint;
begin
  for i:=0 to BPList.Count-1 do begin
    raw:=TRawCPUBreakpoint(BPList[i]);
    raw.Enable;
  end;
end;

procedure TDbgProcess.HandleBreakpointEvent(const Event:TDbgEvent; var StartedSingleStep: Boolean);
var
  brk : TDbgBreakpoint;
  thr : TDbgThread;
  rep : Boolean;
begin
  StartedSingleStep:=False;
  thr:=FindThread(Event.Thread);
  if Assigned(thr) and thr.ProcWaitStep then
    HandleManualStep(Event, rep);

  brk:=FindBreakpoint(Event.Addr, False);
  thr:=FindThread(Event.Thread);

  if Assigned(brk) and Assigned(thr) then begin
    brk.fRaw.Disable;
    if not SetThreadExecAddr(thr, Event.Addr) then
      Exit;

    StartedSingleStep:=thr.NextSingleStep; // trying to single step!
    if StartedSingleStep then begin
      thr.ProcWaitStep:=True;
      thr.ProcBreakAddr:=Event.Addr;
    end;
  end;
end;

procedure TDbgProcess.HandleManualStep(const Event: TDbgEvent; var ReportToUser: Boolean);
var
  thr : TDbgThread;
  brk : TDbgBreakpoint;
begin
  thr:=FindThread(Event.Thread);
  if thr.ProcWaitStep then begin
    thr.ProcWaitStep:=False;
    brk:=FindBreakpoint(thr.ProcBreakAddr, False);
    if Assigned(brk) and brk.isEnabledOrHandled then
      brk.fRaw.Enable;
    ReportToUser:=False;
  end else
    ReportToUser:=True;
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
var
  bp    : TFPList;
begin
  bp:=TFPList.Create;
  //UninstallBreaks(Offset, Count, bp);
  Result := fOwner.fTarget.ReadMem(fID, Offset, Count, Data);
  //ReinstallBreaks(Offset, Count, bp);
  bp.Free;
end;

function TDbgProcess.WriteMem(Offset: TDbgPtr; Count: Integer; const Data: array of byte): Integer;
var
  bp    : TFPList;
begin
  bp:=TFPList.Create;
  //UninstallBreaks(Offset, Count, bp);

  Result := fOwner.fTarget.WriteMem(fID, Offset, Count, Data);

  //ReinstallBreaks(Offset, Count, bp);
  bp.Free;
end;

function TDbgProcess.EnableBreakpoint(const Addr: TDbgPtr): Boolean;
var
  bp  : TDbgBreakpoint;
begin
  bp:=FindBreakpoint(Addr, True);
  Result:=Assigned(bp) and (bp.Enable);
end;

procedure TDbgProcess.DisableBreakpoint(const Addr: TDbgPtr); 
var
  bp  : TDbgBreakpoint;
begin
  bp:=FindBreakpoint(Addr, False);
  if Assigned(bp) then bp.Disable;
end;

function TDbgProcess.AddBreakHandler(const Addr: TDbgPtr; Handler: TDbgHandlerEvent): Boolean;
var
  bp  : TDbgBreakpoint;
begin
  bp:=FindBreakpoint(Addr, True);
  bp.AddHandler(Handler);
  Result:=True;
end;

procedure TDbgProcess.RemoveBreakHandler(const Addr: TDbgPtr; Handler: TDbgHandlerEvent);
var
  bp  : TDbgBreakpoint;
begin
  bp:=FindBreakpoint(Addr, False);
  if Assigned(bp) then bp.RemoveHandler(Handler);
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
  Result:=DbgTarget.SetThreadRegs(fOwner.ID, fID, Registers);
end;

function TDbgThread.NextSingleStep: Boolean; 
begin
  Result:=DbgTarget.SetSingleStep(fOwner.ID, fID);
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

{ TDbgBreakpoint }

procedure TDbgBreakpoint.SetEnabled(AEnabled: Boolean);
begin
  if fEnabled=AEnabled then Exit;
  fEnabled:=AEnabled;
  RefreshRaw;
end;

procedure TDbgBreakpoint.RefreshRaw;
var
  WantedRawState : Boolean;
begin
  WantedRawState := fEnabled or (Count>0);
  if WantedRawState = fRawEnabled then Exit;
  fRawEnabled:=WantedRawState;
  if fRawEnabled then
    fRaw.Enable
  else
    fRaw.Disable;
end;

constructor TDbgBreakpoint.Create(AOwner: TDbgProcess; AAddr: TDbgPtr);
begin
  inherited Create;
  fAddr:=AAddr; 
  fOwner:=AOwner;
end;

destructor TDbgBreakpoint.Destroy;
begin
  inherited Destroy;  
end;

procedure TDbgBreakpoint.NotifyHandlers(var NeedToStop: Boolean);
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

procedure TDbgBreakpoint.AddHandler(AHandleEvent: TDbgBreakPointHandler);
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

procedure TDbgBreakpoint.RemoveHandler(AHandleEvent: TDbgBreakPointHandler);
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

function TDbgBreakpoint.isEnabledOrHandled:Boolean;
begin
  Result:=fEnabled or (Count>0);
end;

function TDbgBreakpoint.Enable:Boolean;
begin
  fEnabled:=True;
  Result:=fRaw.isEnabled;
  if not Result then begin
    RefreshRaw;
    fEnabled:=fRaw.isEnabled;
  end;
  Result:=fEnabled;
end;

procedure TDbgBreakpoint.Disable;
begin
  fEnabled:=False;
  RefreshRaw;
end;

end.

