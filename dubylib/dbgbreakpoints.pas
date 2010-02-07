unit dbgBreakPoints; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, 
  dbgTypes, dbgCPU, dbgUtils, memviewer, dbgMain; 
  
type
  { --- low-level breakpoints --- }
  
  TRawBreakpoint = class(TObject);
  
  TBreakpointAccess = class(TObject)
  public
    function CanSetBreakpoint(Addr: TDbgPtr; AProcess: TDbgProcess): Boolean; virtual; abstract;
    function SetBreakpoint(Addr: TDbgPtr; AProcess: TDbgProcess): TRawBreakpoint; virtual; abstract;
    procedure UnsetBreakpoint(AProcess: TDbgProcess; var Point: TRawBreakpoint); virtual; abstract;
  end;
  
  { TCPURawBreakpoint }
  
  TCPUBreakpointAcccess = class;

  TCPURawBreakpoint = class(TRawBreakpoint)
  private
    FOwner    : TCPUBreakpointAcccess;
  public
    Addr      : TDbgPtr;
    CodeSize  : Integer;
    Code      : array of byte;
    constructor Create(AOwner: TCPUBreakpointAcccess);
    function Install(AAddr: TDbgPtr; AProcess: TDbgProcess): Boolean;
    procedure Uninstall(AProcess: TDbgProcess);
    destructor Destroy; override;
  end;
  
  { TCPUBreakpointAcccess }

  TCPUBreakpointAcccess = class(TBreakpointAccess)
  private
    fCPU      : TCPUCode;
  public
    constructor Create(ACPU: TCPUCode);
    function CanSetBreakpoint(Addr: TDbgPtr; AProcess: TDbgProcess): Boolean; override;
    function SetBreakpoint(Addr: TDbgPtr; AProcess: TDbgProcess): TRawBreakpoint; override;
    procedure UnsetBreakpoint(AProcess: TDbgProcess; var Point: TRawBreakpoint); override;
  end;

  { --- high-level breakpoints --- }  
  
  TDbgProcessBreakpoints = class;
  TProcessBreakpoint = class;
  
  { TDbgBreakPoint }

  TDbgBreakPoint = class(TObject)
  private
    fProcBP   : TProcessBreakpoint;
    fEnabled  : Boolean;
  protected
    function GetAddr: TDbgPtr;
    procedure SetEnabled(AEnabled: Boolean);    
  public
    Tag  : TObject;
    constructor Create(AOwner: TProcessBreakpoint; AEnabled: Boolean);
    property Enabled : Boolean read fEnabled write SetEnabled;
    property Addr: TDbgPtr read GetAddr;
  end;
 
  { TProcessBreakpoint }

  TProcessBreakpoint = class(TObject)
  private
    fRefCount: Integer;
    procedure IncRefCount;
    procedure DecRefCount;
  public
    Owner     : TDbgProcessBreakpoints;
    RawPoint  : TRawBreakpoint;
    Handlers  : array of TNotifyEvent;
    HndCount  : Integer;
    Addr      : TDbgPtr;
    constructor Create(AOwner: TDbgProcessBreakpoints; AAddr: TDbgPtr);
    function AddBreakpoint(AEnabled: Boolean): TDbgBreakPoint;
    procedure RemoveBreakPoint(var Dbg: TDbgBreakPoint);
    procedure AddHandler(Event: TNotifyEvent);
    procedure RemoveHandler(Event: TNotifyEvent);
    property RefCount  : Integer read fRefCount;
  end;
  
  
  { TDbgProcessBreakpoints }

  TDbgProcessBreakpoints = class(TObject)
  private
    fProcess  : TDbgProcess;
    fAccesss  : TBreakpointAccess;
    fList     : TFPList; // todo: binary-search (sorted) list? AVL tree? 
  protected
    function FindBreakpointInt(const addr: TDbgPtr): Integer;
  public
    constructor Create(AProcess: TDbgProcess; BpAccess: TBreakpointAccess);
    destructor Destroy; override;
    
    function AddBreakPoint(const addr: TDbgPtr; AEnabled: Boolean): TDbgBreakPoint; 
    function FindBreakpoint(const addr: TDbgPtr): TDbgBreakPoint;
    procedure RemoveBreakPoint(var Bp: TDbgBreakPoint);
    procedure Clear;
    
    property Process: TDbgProcess read fProcess;
  end;
  
//function AddBreakPoint(const addr: TDbgPtr): TBreakPoint;
// function fails, if break point already exists, cannot be enabled or matches hard-coded breakpoint
//todo:
{function AddEnabledBreakPoint(const addr: TDbgPtr; AProcess: TDbgTarget): TBreakPoint;
function FindBreakpoint(const addr: TDbgPtr): TBreakPoint;
procedure RemoveBreakPoint(var Bp: TBreakPoint);

function HandleBreakpoint(bp: TBreakpoint; ThreadID: TDbgThreadID; Process: TDbgTarget): Boolean; overload;
function HandleBreakpoint(addr: TDbgPtr; ThreadID: TDbgThreadID; Process: TDbgTarget; RemoveIfHandled: Boolean = false): Boolean; overload;}

implementation
(*
function AddBreakPoint(const addr: TDbgPtr): TBreakPoint;
begin
//  Result := fBreakPoints.AddBreakPoint(addr);
end;

function AddEnabledBreakPoint(const addr: TDbgPtr; AProcess: TDbgTarget): TBreakPoint;
begin
(*  Result := FindBreakpoint(addr);
  if Assigned(Result) and Result.Enabled then begin
    Result := nil;
    Exit;
  end;
  if not Assigned(Result) then Result := AddBreakPoint(addr);
  Result.Enable(AProcess);
  if not Result.Enabled then begin
    Result.Free;
    Result := nil;
  end;*)
end;

function FindBreakpoint(const addr: TDbgPtr): TBreakPoint;
begin
//  Result := fBreakPoints.FindBreakPoint(addr);
end;

procedure RemoveBreakPoint(var Bp: TBreakPoint);
begin
//  fBreakPoints.RemoveBreakPoint(bp);
end;

function HandleBreakpoint( bp: TBreakpoint; ThreadID: TDbgThreadID; Process: TDbgTarget): Boolean;
var
  list : TDbgDataBytesList;

begin
  if not Assigned(bp) or not Assigned(Process) then Exit;

  Result := not bp.Enabled;
  if Result then Exit;  
  
  Result := bp.Disable(Process);
  if not Result then Exit;
  
  list := TDbgDataBytesList.Create;
  try
    Result := Process.GetThreadRegs(0, ThreadID, list);
    PrintI386Regs(list);

    list[CPUCode.ExecuteRegisterName].DbgPtr := bp.Addr;
    Result := Result and Process.SetThreadRegs(0, ThreadID, list);
  finally  
    list.Free;
  end;
end;

function HandleBreakpoint(addr: TDbgPtr; ThreadID: TDbgThreadID; Process: TDbgTarget; RemoveIfHandled: Boolean): Boolean; overload;
var
  bp  : TBreakPoint;
begin
  bp := FindBreakpoint(addr);
  if not Assigned(bp) then begin
    Result := false;
    Exit;
  end;
  Result := HandleBreakpoint(bp, ThreadID, Process);
  
  if Result and RemoveIfHandled then RemoveBreakPoint(bp);
end;
*)

{ TDbgProcessBreakpoints }

constructor TDbgProcessBreakpoints.Create(AProcess: TDbgProcess; BpAccess: TBreakpointAccess); 
begin
  inherited Create;
  fList:=TFPList.Create;
end;

destructor TDbgProcessBreakpoints.Destroy;  
begin
  Clear;
  fList.Free;
  inherited Destroy;  
end;

function TDbgProcessBreakpoints.AddBreakPoint(const addr: TDbgPtr; AEnabled: Boolean): TDbgBreakPoint; 
begin
{  Result := FindBreakpoint(addr);
  if Assigned(Result) then Exit;
  Result := TDbgBreakPoint.Create(Self, ;
  Result.fAddr := addr;
  fList.Add(Result);}
end;

function TDbgProcessBreakpoints.FindBreakpointInt(const addr: TDbgPtr): Integer;
{var
  i : Integer;}
begin
{  Result := -1;
  for i := 0 to fList.Count - 1 do begin
    if TBreakPoint(fList[i]).Addr = addr then begin
      Result := i;
      Exit;
    end;
  end;}
end;

function TDbgProcessBreakpoints.FindBreakpoint(const addr: TDbgPtr): TDbgBreakPoint; 
{var
  i : Integer;}
begin
{  i := FindBreakpointInt(addr);
  if i < 0 then 
    Result := nil
  else
    Result := TBreakPoint(fList[i]);}
end;

procedure TDbgProcessBreakpoints.RemoveBreakPoint(var bp: TDbgBreakPoint); 
{var
  i : Integer;}
begin
{  i := fList.IndexOf(bp);
  if i < 0 then Exit;
  
  bp.Free;
  fList.Delete(i);
  bp:=nil;}
end;

procedure TDbgProcessBreakpoints.Clear; 
{var
  i   : Integer;}
begin
{  for i := 0 to fList.Count - 1 do 
    TBreakPoint(FList[i]).Free;
  FList.Clear;}
end;

{ TDbgBreakPoint }

function TDbgBreakPoint.GetAddr: TDbgPtr; 
begin

end;

procedure TDbgBreakPoint.SetEnabled(AEnabled: Boolean);
var
  buf : array of byte;
begin
  if fEnabled then begin
    //Result := true;
    Exit;
  end;
  (*
  CodeSize := CPUCode.BreakPointSize;
  
  if length(code) < CodeSize then SetLength(code, CodeSize);
  Result := (AProcess.ReadMem(0, addr, CodeSize, Code) = CodeSize) and not CPUCode.IsBreakPoint(Code, 0);
  
  if not Result then begin
    writeln('bp: cannot read proces mem');
    Exit;
  end;
  
  SetLength(buf, CodeSize);
  CPUCode.WriteBreakPoint(buf, 0);
  fEnabled := AProcess.WriteMem(0, addr, CodeSize, buf) = CodeSize;
  Result := fEnabled;
  *)
end;

constructor TDbgBreakPoint.Create(AOwner: TProcessBreakpoint; AEnabled: Boolean
  ); 
begin

end;

(*
function TDbgBreakPoint.Disable(AProcess: TDbgTarget): Boolean;
begin
  if not fEnabled then begin
    Result := True;
    Exit;
  end;
  if CodeSize > 0 then  
    fEnabled := not (AProcess.WriteMem(0, addr, CodeSize, Code) = CodeSize);
    
  Result := not fEnabled;
end;
*)

{ TCPUBreakpointAcccess }

constructor TCPUBreakpointAcccess.Create(ACPU: TCPUCode); 
begin
  inherited Create;
  fCPU:=ACPU;
end;

function TCPUBreakpointAcccess.CanSetBreakpoint(Addr: TDbgPtr; AProcess: TDbgProcess): Boolean;  
var
  buf : array of byte;
begin
  SetLength(buf, fCPU.BreakPointSize);
  Result:=not fCPU.IsBreakPoint(buf, 0);
end;

function TCPUBreakpointAcccess.SetBreakpoint(Addr: TDbgPtr; AProcess: TDbgProcess): TRawBreakpoint;  
var
  cpubp : TCPURawBreakpoint;
begin
  Result:=nil;
  cpubp:=TCPURawBreakpoint.Create(Self);
  if not cpubp.Install(Addr, AProcess) then 
    cpubp.Free
  else
    Result:=cpubp;
end;

procedure TCPUBreakpointAcccess.UnsetBreakpoint(AProcess: TDbgProcess; var Point: TRawBreakpoint);  
begin
  if not Assigned(Point) or not (Point is TCPURawBreakpoint) then Exit;
  if Assigned(AProcess) then TCPURawBreakpoint(Point).Uninstall(AProcess);
  Point.Free;
  Point:=nil;
end;

{ TCPURawBreakpoint }

constructor TCPURawBreakpoint.Create(AOwner: TCPUBreakpointAcccess); 
begin
  inherited Create;
  fOwner:=AOwner;
end;

destructor TCPURawBreakpoint.Destroy; 
begin
  inherited;
end;

function TCPURawBreakpoint.Install(AAddr: TDbgPtr; AProcess: TDbgProcess): Boolean; 
var
  buf : array of byte;
  CPU : TCPUCode;
begin
  CPU:=FOwner.fCPU;
  Addr:=AAddr;
  CodeSize := CPU.BreakPointSize;
  
  if length(Code) < CodeSize then SetLength(code, CodeSize);
  Result := (AProcess.ReadMem(Addr, CodeSize, Code) = CodeSize);
  if Result then Result:= not CPUCode.IsBreakPoint(Code, 0);
  if not Result then Exit;
  
  SetLength(buf, CodeSize);
  CPUCode.WriteBreakPoint(buf, 0);
  Result := AProcess.WriteMem(Addr, CodeSize, buf) = CodeSize;
end;

procedure TCPURawBreakpoint.Uninstall(AProcess: TDbgProcess); 
begin
  if CodeSize=0 then Exit;
  if Assigned(AProcess) then AProcess.WriteMem(Addr, CodeSize, Code);
end;

{ TProcessBreakpoint }

procedure TProcessBreakpoint.IncRefCount; 
begin
  if fRefCount=0 then 
    RawPoint:=Owner.fAccesss.SetBreakpoint(Addr, Owner.Process);    
  inc(fRefCount);
end;

procedure TProcessBreakpoint.DecRefCount; 
begin
  if fRefCount = 0 then begin
    //todo: ERROR!
    Exit;
  end;
  
  dec(fRefCount);
  if fRefCount=0 then
    Owner.fAccesss.UnsetBreakpoint(Owner.Process, RawPoint);
end;

constructor TProcessBreakpoint.Create(AOwner: TDbgProcessBreakpoints; AAddr: TDbgPtr); 
begin
  inherited Create;
  Owner:=AOwner;
  Addr:=AAddr;
end;

function TProcessBreakpoint.AddBreakpoint(AEnabled: Boolean): TDbgBreakPoint; 
begin
  Result:=TDbgBreakPoint.Create(Self, AEnabled);
end;

procedure TProcessBreakpoint.AddHandler(Event: TNotifyEvent); 
begin
  if HndCount=length(Handlers) then begin
    if HndCount=0 then SetLength(Handlers, 4)
    else SetLength(Handlers, HndCount*2);
  end;
  Handlers[HndCount]:=Event;
  inc(HndCount);
end;

procedure TProcessBreakpoint.RemoveHandler(Event: TNotifyEvent); 
var
  i : Integer;
begin
  for i:=0 to HndCount-1 do 
    if Handlers[i]=Event then begin
      Handlers[i]:=Handlers[HndCount-1];
      dec(HndCount);
      Exit;
    end;
end;

procedure TProcessBreakpoint.RemoveBreakPoint(var Dbg: TDbgBreakPoint); 
begin
  if not Assigned(Dbg) or (Dbg.fProcBP<>Self) then Exit;
  Dbg.Free;
  Dbg:=nil;
end;

initialization

finalization

end.

