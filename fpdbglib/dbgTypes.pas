unit dbgTypes; 

{$mode objfpc}{$H+}

interface

type
  TDbgPtr      = PtrUInt;
  TDbgThreadID = TThreadID;
  
  TDbgState = (ds_Nonstarted, ds_ReadToRun, ds_Running, ds_Terminated);
   
  TDbgEventKind = (dek_Other, dek_ProcessStart, dek_ProcessTerminated);
  
  TDbgEvent = record
    Addr    : TDbgPtr;
    Thread  : TDbgThreadID;
    Kind    : TDbgEventKind;
  end;   
   
  TDbgProcess = class(TObject)
  protected
    function GetProcessState: TDbgState; virtual; abstract;
  public
    //function Execute(const CommandLine: String): Boolean; virtual; abstract;
    procedure Terminate; virtual; abstract;
    function WaitNextEvent(var Event: TDbgEvent): Boolean; virtual; abstract;
    property State: TDbgState read GetProcessState;
  end;

var
  DebugProcessStart: function(const ACmdLine: String): TDbgProcess = nil;

implementation

function DummyDebugProcessStart(const ACmdLine: String): TDbgProcess;
begin
  Result := nil;
end;

procedure InitDummyDebug;
begin
  if DebugProcessStart = nil then DebugProcessStart := @DummyDebugProcessStart;
end;

initialization
  InitDummyDebug;


end.

