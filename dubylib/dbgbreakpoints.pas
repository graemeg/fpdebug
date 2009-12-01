unit dbgBreakPoints; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, 
  dbgTypes, dbgCPU, dbgUtils, memviewer, dbgMain; 
  
type
  
  { TBreakPoint }

  TBreakPoint = class(TObject)
  private
    fAddr     : TDbgPtr;
    CodeSize  : Integer;
    Code      : array of byte;
    fEnabled  : Boolean;
  public
    Tag       : TObject;
    function Enable(AProcess: TDbgTarget): Boolean;
    function Disable(AProcess: TDbgTarget): Boolean;
    
    property Enabled : Boolean read fEnabled;
    property Addr: TDbgPtr read fAddr;
  end;
  
  { TBreakPointsList }

  TBreakPointsList = class(TObject)
  private
    fList : TFPList; // todo: binary-search (sorted) list? AVL tree? 
  protected
    function FindBreakpointInt(const addr: TDbgPtr): Integer;
  public
    constructor Create;
    destructor Destroy; override;
    
    function AddBreakPoint(const addr: TDbgPtr): TBreakPoint; 
    function FindBreakpoint(const addr: TDbgPtr): TBreakPoint;
    procedure RemoveBreakPoint(var Bp: TBreakPoint);
    procedure Clear;
  end;
  
function AddBreakPoint(const addr: TDbgPtr): TBreakPoint;
// function fails, if break point already exists, cannot be enabled or matches hard-coded breakpoint
function AddEnabledBreakPoint(const addr: TDbgPtr; AProcess: TDbgTarget): TBreakPoint;
function FindBreakpoint(const addr: TDbgPtr): TBreakPoint;
procedure RemoveBreakPoint(var Bp: TBreakPoint);

function HandleBreakpoint(bp: TBreakpoint; ThreadID: TDbgThreadID; Process: TDbgTarget): Boolean; overload;
function HandleBreakpoint(addr: TDbgPtr; ThreadID: TDbgThreadID; Process: TDbgTarget; RemoveIfHandled: Boolean = false): Boolean; overload;


implementation

var
  fBreakPoints : TBreakPointsList = nil;

function AddBreakPoint(const addr: TDbgPtr): TBreakPoint;
begin
  Result := fBreakPoints.AddBreakPoint(addr);
end;

function AddEnabledBreakPoint(const addr: TDbgPtr; AProcess: TDbgTarget): TBreakPoint;
begin
  Result := FindBreakpoint(addr);
  if Assigned(Result) and Result.Enabled then begin
    Result := nil;
    Exit;
  end;
  if not Assigned(Result) then Result := AddBreakPoint(addr);
  Result.Enable(AProcess);
  if not Result.Enabled then begin
    Result.Free;
    Result := nil;
  end;
end;

function FindBreakpoint(const addr: TDbgPtr): TBreakPoint;
begin
  Result := fBreakPoints.FindBreakPoint(addr);
end;

procedure RemoveBreakPoint(var Bp: TBreakPoint);
begin
  fBreakPoints.RemoveBreakPoint(bp);
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
 
{ TBreakPointsList }

constructor TBreakPointsList.Create; 
begin
  inherited Create;
  fList:=TFPList.Create;
end;

destructor TBreakPointsList.Destroy;  
begin
  Clear;
  fList.Free;
  inherited Destroy;  
end;

function TBreakPointsList.AddBreakPoint(const addr: TDbgPtr): TBreakPoint; 
begin
  Result := FindBreakpoint(addr);
  if Assigned(Result) then Exit;
  Result := TBreakPoint.Create;
  Result.fAddr := addr;
  fList.Add(Result);
end;

function TBreakPointsList.FindBreakpointInt(const addr: TDbgPtr): Integer;
var
  i : Integer;
begin
  Result := -1;
  for i := 0 to fList.Count - 1 do begin
    if TBreakPoint(fList[i]).Addr = addr then begin
      Result := i;
      Exit;
    end;
  end;
end;

function TBreakPointsList.FindBreakpoint(const addr: TDbgPtr): TBreakPoint; 
var
  i : Integer;
begin
  i := FindBreakpointInt(addr);
  if i < 0 then 
    Result := nil
  else
    Result := TBreakPoint(fList[i]);
end;

procedure TBreakPointsList.RemoveBreakPoint(var bp: TBreakPoint); 
var
  i : Integer;
begin
  i := fList.IndexOf(bp);
  if i < 0 then Exit;
  
  bp.Free;
  fList.Delete(i);
  bp:=nil;
end;

procedure TBreakPointsList.Clear; 
var
  i   : Integer;
begin
  for i := 0 to fList.Count - 1 do 
    TBreakPoint(FList[i]).Free;
  FList.Clear;
end;

{ TBreakPoint }

function TBreakPoint.Enable(AProcess: TDbgTarget): Boolean;
var
  buf : array of byte;
begin
  if fEnabled then begin
    Result := true;
    Exit;
  end;
  
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
end;

function TBreakPoint.Disable(AProcess: TDbgTarget): Boolean;
begin
  if not fEnabled then begin
    Result := True;
    Exit;
  end;
  if CodeSize > 0 then  
    fEnabled := not (AProcess.WriteMem(0, addr, CodeSize, Code) = CodeSize);
    
  Result := not fEnabled;
end;

procedure InitBreakpoints;
begin
  fBreakPoints := TBreakPointsList.Create;
end;

procedure ReleaseBreakpoints;
begin
  fBreakPoints.Free;
end;

initialization
  InitBreakpoints;

finalization
  ReleaseBreakpoints;

end.

