unit dbgProject;

interface

uses
  SysUtils,
  dbgTypes,
  dbgMain, dbgAsyncMain, LMessages, LCLIntf, LCLType, dbgInfoTypes;

function ASync: TDbgAsyncMain;

var
  dbgInfo: TDbgInfo=nil;

procedure StartDebug(const CmdLineUtf8: AnsiString);

const
  EventKindStr: array [TDbgEventKind] of String = (
    'dek_Other', 'dek_SysExc', 'dek_SingleStep', 'dek_BreakPoint',
    'dek_ProcessStart', 'dek_ProcessTerminated',
    'dek_ThreadStart', 'dek_ThreadTerminated',
    'dek_SysCall');

implementation

var
  fASync : TDbgAsyncMain = nil;

type
  { TDbgAsyncLCLCallback }

  TDbgAsyncLCLCallback = class(TDbgASyncCallback)
  protected
    fHwnd   : THandle;
    procedure WndProc(var TheMessage: TLMessage);
  public
    constructor Create;
    destructor Destroy; override;
    procedure StateChanged; override;
  end;

procedure StartDebug(const CmdLineUtf8: AnsiString);
begin
  ASync.Main:=TDbgMain.Create(DebugProcessStart(CmdLineUtf8), 0);
  dbgInfo.Free;
  dbgInfo:=TDbgInfo.Create;
  LoadDebugInfoFromFile(dbgInfo, UTF8Decode(CmdLineUtf8));
end;

function ASync: TDbgAsyncMain;
begin
  if not Assigned(fASync) then
    fASync := TDbgAsyncMain.Create(TDbgAsyncLCLCallback.Create);
  Result:=fASync;
end;


{ TDbgThreadLCLCallback }

const
  MSG_STATECHANGE=LM_USER;

constructor TDbgAsyncLCLCallback.Create;
begin
  inherited Create;
  fHwnd:=AllocateHWnd(@WndProc);
end;

destructor TDbgAsyncLCLCallback.Destroy;
begin
  DeallocateHWnd(fHwnd);
  inherited Destroy;
end;

procedure TDbgAsyncLCLCallback.WndProc(var TheMessage:TLMessage);
begin
  case TheMessage.msg of
    MSG_STATECHANGE: DoStateChanged;
  end;
end;

procedure TDbgAsyncLCLCallback.StateChanged;
begin
  PostMessage(fHwnd, MSG_STATECHANGE, 0, 0);
end;

initialization

finalization
  ASync.Free;
  dbgInfo.Free;

end.
