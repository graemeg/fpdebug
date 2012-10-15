{
    fpDebug  -  A debugger for the Free Pascal Compiler.

    Copyright (c) 2012 by Graeme Geldenhuys.

    See the file LICENSE.txt, included in this distribution,
    for details about redistributing fpDebug.

    Description:
      .
}
unit dbgAsyncMain;

interface

{$mode objfpc}{$h+}

uses
  SysUtils, Classes, syncobjs,
  dbgTypes, dbgMain;

type
  TDbgMainState = (mstStopped, mstExecuting, mstError, mstTerminated);

  TDbgEventStateChange = procedure (Sender: TObject; NewState: TDbgMainState) of object;
  TDbgASyncProc = function (Main: TDbgMain; AProcData: TObject): Integer of object;

  TDbgAsyncMain=class;

  { TDbgASyncCallback }

  TDbgASyncCallback = class(TObject)
  private
    fAsync  : TDbgAsyncMain;
  protected
    procedure DoStateChanged;
  public
    procedure StateChanged; virtual; abstract;
  end;

  { TDbgMainThread }

  TDbgMainThread = class(TThread)
  private
    fMain : TDbgAsyncMain;
  protected
    procedure Execute; override;
  public
    constructor Create(AMain: TDbgAsyncMain);
  end;

  { TDbgAsyncMain }

  TDbgAsyncMain=class(TObject)
  private
    fThread     : TDbgMainThread;
    fMain       : TDbgMain;
    fCallback   : TDbgASyncCallback;
    fExeLock    : TEvent;
    fLock       : TCriticalSection;
    fState      : TDbgMainState;

    fExeProc    : TDbgASyncProc;
    fExeData    : TObject;
    fExeRet     : Integer;

    fQuitLoop   : Boolean;
    fDbgEvent   : TDbgEvent;

    fOnStateChanged   : TNotifyEvent;
  protected
    procedure Lock;
    procedure Unlock;

    procedure ThreadWorkLoop(WorkMain: TDbgMain);
    procedure ThreadLoop;

    procedure ResumeExec(AProc: TDbgASyncProc; AData: TObject);

    function GetLastEvent: TDbgEvent;
    procedure SetLastEvent(const AEvent: TDbgEvent);

    function GetState:TDbgMainState;
    procedure SetState(AState: TDbgMainState);
    procedure FinishExecute;

  public
    constructor Create(ACallback: TDbgASyncCallback);
    destructor Destroy; override;

    function SetMain(AMain: TDbgMain): Boolean;

    function ExecuteASync(AProc: TDbgASyncProc; AProcData: TObject): Boolean;
    function GetAsyncResult(var AProcData: TObject; var RetValue: Integer): Boolean; overload;
    function GetAsyncResult(var RetValue: Integer): Boolean; overload;

    procedure Resume;
    procedure Terminate;
    procedure WaitFor;
    property State: TDbgMainState read GetState;
    property Main: TDbgMain read fMain;
    property LastEvent: TDbgEvent read GetLastEvent;
    property Callback: TDbgASyncCallback read fCallback;

    property OnStateChanged: TNotifyEvent read fOnStateChanged write fOnStateChanged;
  end;

implementation

{ TDbgAsyncMain }

constructor TDbgAsyncMain.Create(ACallback: TDbgASyncCallback);
begin
  inherited Create;
  fCallback:=ACallback;
  fCallback.fAsync:=Self;
  fLock:=TCriticalSection.Create;
  fExeLock:=TEvent.Create(nil, True, False, '');
  fExeLock.ResetEvent;
  fThread:=TDbgMainThread.Create(Self);
end;

destructor TDbgAsyncMain.Destroy;
begin
  //todo: make it safe!
  FinishExecute;
  fThread.Free;
  fLock.Free;
  fExeLock.Free;
  inherited Destroy;
end;

function TDbgAsyncMain.ExecuteASync(AProc:TDbgASyncProc;AProcData:TObject): Boolean;
begin
  Result:=(GetState<>mstExecuting) and (Assigned(AProc)) and Assigned(fMain);
  if not Result then Exit;
  ResumeExec(AProc, AProcData);
end;

function TDbgAsyncMain.GetAsyncResult(var AProcData:TObject;var RetValue:Integer):
  Boolean;
begin
  Result:=Assigned(fExeProc);
  if Result then begin
    AProcData:=fExeData;
    RetValue:=fExeRet;
  end;
end;

function TDbgAsyncMain.GetAsyncResult(var RetValue: Integer): Boolean; overload;
var
  data : TObject;
begin
  Result:=GetAsyncResult(Data, RetValue);
end;

procedure TDbgAsyncMain.Resume;
begin
  if (State=mstExecuting) and Assigned(fMain) then Exit;
  ResumeExec(nil, nil);
end;

procedure TDbgAsyncMain.Terminate;
begin

end;

procedure TDbgAsyncMain.WaitFor;
begin
  fQuitLoop:=True;
  fExeLock.SetEvent;
  fThread.WaitFor;
end;

function TDbgAsyncMain.GetState:TDbgMainState;
begin
  Lock;
  try
    Result:=fState;
  finally
    Unlock;
  end;
end;

procedure TDbgAsyncMain.SetState(AState: TDbgMainState);
begin
  Lock;
  try
    fState:=AState;
  finally
    Unlock;
  end;
  fCallback.StateChanged;
end;

procedure TDbgAsyncMain.FinishExecute;
begin
  fQuitLoop:=True;
  fExeLock.SetEvent;
  fThread.WaitFor;
end;

function TDbgAsyncMain.SetMain(AMain:TDbgMain): Boolean;
begin
  //todo: Don't allow Main change while being processed
  //      a user must terminate the execution of the previouse main
  //      before changing
  Result:=True;
  Lock;
  try
    fMain:=AMain;
    if Assigned(fMain) then fExeLock.SetEvent;
  finally
    Unlock;
  end;
end;

procedure TDbgAsyncMain.Lock;
begin
  fLock.Enter;
end;

procedure TDbgAsyncMain.Unlock;
begin
  fLock.Leave;
end;

procedure TDbgAsyncMain.ThreadWorkLoop(WorkMain:TDbgMain);
var
  event     : TDbgEvent;
  newState  : TDbgMainState;
begin
  if Assigned(fExeProc) then begin
    try
      fExeRet:=fExeProc(fMain, fExeData);
    except
    end;
    fExeLock.ResetEvent;
  end else begin
    if not fMain.WaitNextEvent(event) then
      newState:=mstError
    else begin
      if event.Kind=dek_ProcessTerminated then
        //todo: Check if MainProcess being terminated
        newState:=mstTerminated
      else
        newState:=mstStopped;
      SetLastEvent(event);
    end;
    fExeLock.ResetEvent;
  end;
  SetState(newState);
end;

procedure TDbgAsyncMain.ResumeExec(AProc:TDbgASyncProc;AData:TObject);
begin
  Lock;
  try
    fExeProc:=AProc;
    fExeData:=AData;
    SetState(mstExecuting);
    fExeLock.SetEvent;
  finally
    Unlock;
  end;
end;

procedure TDbgAsyncMain.ThreadLoop;
var
  WorkMain  : TDbgMain;
begin
  while not fQuitLoop do begin
    fExeLock.WaitFor(INFINITE);
    if fQuitLoop then Break;
    Lock;
    try
      if not Assigned(fMain) then begin
        fExeLock.ResetEvent;
        WorkMain:=nil;
      end else
        WorkMain:=fMain;
    finally
      Unlock;
    end;
    if Assigned(fMain) then
      ThreadWorkLoop(fMain);
  end;
end;

function TDbgAsyncMain.GetLastEvent:TDbgEvent;
begin
  Lock;
  try
    Result:=fDbgEvent;
  finally
    Unlock;
  end;
end;

procedure TDbgAsyncMain.SetLastEvent(const AEvent:TDbgEvent);
begin
  Lock;
  try
    fDbgEvent:=AEvent;
  finally
    Unlock;
  end;
end;

constructor TDbgMainThread.Create(AMain:TDbgAsyncMain);
begin
  fMain:=AMain;
  inherited Create(False);
end;

{ TDbgMainThread }

procedure TDbgMainThread.Execute;
begin
  try
    fMain.ThreadLoop;
  except
  end;
end;

{ TDbgASyncCallback }

procedure TDbgASyncCallback.DoStateChanged;
begin
  if Assigned(fAsync.OnStateChanged) then
    fAsync.OnStateChanged(fAsync);
end;

end.
