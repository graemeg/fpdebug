unit dbgProject;

interface

uses
  dbgTypes,
  dbgMain, dbgAsyncMain, LMessages, LCLIntf;

var
  Main    : TDbgMain;
  ASync   : TDbgAsyncMain;

procedure StartDebug(const CmdLineUtf8: AnsiString);

implementation

procedure StartDebug(const CmdLineUtf8: AnsiString);
begin
  Main:=TDbgMain.Create(DebugProcessStart(CmdLineUtf8), 0);
end;

type
  { TDbgThreadLCLCallback }

  TDbgThreadLCLCallback = class(TDbgThreadCallback)
  protected
    fHwnd   : THandle;
    procedure WndProc(var TheMessage: TLMessage);
  public
    constructor Create;
    destructor Destroy; override;
    procedure StateChanged; override;
  end;

{ TDbgThreadLCLCallback }

const
  MSG_STATECHANGE=LM_USER;

constructor TDbgThreadLCLCallback.Create;
begin
  inherited Create;
  fHwnd:=AllocateHWnd(@WndProc);
end;

destructor TDbgThreadLCLCallback.Destroy;
begin
  DeallocateHWnd(fHwnd);
  inherited Destroy;
end;

procedure TDbgThreadLCLCallback.WndProc(var TheMessage:TLMessage);
begin
  case TheMessage.msg of
    MSG_STATECHANGE: begin
      writeln('do state changed!');
      DoStateChanged;
    end;
  end;
end;

procedure TDbgThreadLCLCallback.StateChanged;
begin
  writeln('posting state change! fHwnd = ', fHwnd);
  PostMessage(fHwnd, MSG_STATECHANGE, 0,0);
end;

initialization
  ASync := TDbgAsyncMain.Create(TDbgThreadLCLCallback.Create);

finalization
  ASync.Free;

end.
