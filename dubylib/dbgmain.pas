unit dbgMain; 

{$mode objfpc}{$H+}

interface

uses
  contnrs, dbgTypes; 
  
type
  TDbgMain = class;
  TDbgProcess = class;
  
  { TDbgThread }

  TDbgThread = class(TObject)
  private
    fID     : TDbgThreadID;
    fOwner  : TDbgProcess;
  protected
    function DbgTarget: TDbgTarget; 
  public
    constructor Create(AOwner: TDbgProcess; AID: TDbgThreadID);
    function GetThreadRegs(Registers: TDbgDataList): Boolean; 
    function SetThreadRegs(Registers: TDbgDataList): Boolean; 
    function SetSingleStep: Boolean; 
    property ID: TDbgThreadID read fID;
  end;

  { TDbgProcess }

  TDbgProcess  = class(TObject)
  private
    fOwner    : TDbgMain;
    fID       : Integer;
    fProcID   : TDbgProcessID;
    fThreads  : TFPObjectList;
  protected
    function DbgTarget: TDbgTarget; 
    
    function GetState: TDbgState;
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
    property State: TDbgState read GetState;
    property ThreadsCount: Integer read GetThreadsCount;
    property Thread[i: Integer]: TDbgThread read GetThread;
  end;
  
  { TDbgMain }

  TDbgMain = class(TObject)
  private
    fTarget   : TDbgTarget;
    fProcList : TFPObjectList;
  protected
    procedure DoAddProcess(AProcessID: Integer);
    procedure DoRemoveProcess(AProcessID: Integer);
    function DoReadMem(Proc: TDbgProcess; Offset: TDbgPtr; Count: Integer; var Data: array of byte): Integer; 
    function DoWriteMem(Proc: TDbgProcess; Offset: TDbgPtr; Count: Integer; const Data: array of byte): Integer; 
    function GetProcessCount: Integer;
    function GetProcess(i: Integer): TDbgProcess;
  private
    constructor Create(ATarget: TDbgTarget; AProcessID: TDbgProcessID);
    destructor Destroy; override;
    function WaitForEvent(var Event: TDbgEvent): Boolean;  
    function FindProcess(processID: TDbgProcessID): TDbgProcess;
    function FindThread(processID: TDbgProcessID; ThreadID: TDbgThreadID): TDbgThread;

    property ProcessCount: Integer read GetProcessCount;
    property Process[i: Integer]: TDbgProcess read GetProcess;
  end;

implementation

{ TDbgMain }

procedure TDbgMain.DoAddProcess(AProcessID: Integer); 
var
  proc  : TDbgProcess;
begin
  proc := TDbgProcess.Create(Self, AProcessID);
  fProcList.Add(proc);
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
begin
  //todo: handling wrappers

  Result := fTarget.WriteMem(Proc.ID, Offset, Count, Data);
end;

function TDbgMain.DoWriteMem(Proc: TDbgProcess; Offset: TDbgPtr;  
  Count: Integer; const Data: array of byte): Integer; 
begin
  //todo: handling wrappers

  Result := fTarget.WriteMem(Proc.ID, Offset, Count, Data);
end;

function TDbgMain.GetProcessCount: Integer; 
begin
  Result:=fProcList.Count;
end;

function TDbgMain.GetProcess(i: Integer): TDbgProcess; 
begin
  Result:=TDbgProcess(fProcList[i]);
end;

constructor TDbgMain.Create(ATarget: TDbgTarget; AProcessID: TDbgProcessID); 
begin
  inherited Create;
  fTarget:=ATarget;
  fProcList:=TFPObjectList.Create;
  
  DoAddProcess(AProcessID);
end;

destructor TDbgMain.Destroy;  
begin
  fProcList.Free;
  inherited Destroy;  
end;

function TDbgMain.WaitForEvent(var Event: TDbgEvent): Boolean;  
begin
  Result:=fTarget.WaitNextEvent(Event);
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

function TDbgProcess.GetState: TDbgState; 
begin
  Result:=DbgTarget.GetProcessState(fID);
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

function TDbgProcess.ReadMem(Offset: TDbgPtr; Count: Integer; 
  var Data: array of byte): Integer; 
begin
  Result:=fOwner.DoReadMem(Self, Offset, Count, Data);
end;

function TDbgProcess.WriteMem(Offset: TDbgPtr; Count: Integer; 
  const Data: array of byte): Integer; 
begin
  Result:=fOwner.DoWriteMem(Self, Offset, Count, Data);
end;

{ TDbgThread }

constructor TDbgThread.Create(AOwner: TDbgProcess; AID: TDbgThreadID); 
begin
  inherited Create;
  fOwner:=AOwner;
  fID:=AID;
end;

function TDbgThread.DbgTarget: TDbgTarget; 
begin
  Result:=fOwner.DbgTarget;
end;

function TDbgThread.GetThreadRegs(Registers: TDbgDataList): Boolean; 
begin
  DbgTarget.GetThreadRegs(fOwner.ID, fID, Registers);
end;

function TDbgThread.SetThreadRegs(Registers: TDbgDataList): Boolean; 
begin
  DbgTarget.GetThreadRegs(fOwner.ID, fID, Registers);
end;

function TDbgThread.SetSingleStep: Boolean; 
begin
  DbgTarget.SetSingleStep(fOwner.ID, fID);
end;

end.

