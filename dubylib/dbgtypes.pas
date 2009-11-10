unit dbgTypes; 

{$ifdef fpc}{$mode delphi}{$H+}{$endif}

interface

{$ifndef fpc}
type
  PtrUInt = LongWord;
  PtrInt  = Integer;
  TThreadID = Integer;
  QWord     = Int64;
{$endif}

type
  TDbgPtr      = PtrUInt;
  PDbgPtr      = ^TDbgPtr;
  TDbgThreadID = TThreadID;
  TDbgProcessID = Longword;
  
  
  { TDbgData }

  TDbgData = class(TObject)
  private
    function GetDbgPtr: TDbgPtr;
    function GetSInt16: ShortInt;
    function GetSInt32: Integer;
    function GetSInt64: Int64;
    function GetSInt8: SmallInt;
    function GetUInt8: Byte;
    function GetUInt16: Word;
    function GetUInt32: LongWord;
    function GetUInt64: QWord;
    procedure SetDbgPtr(const AValue: TDbgPtr);
    procedure SetSInt16(const AValue: ShortInt);
    procedure SetSInt32(const AValue: Integer);
    procedure SetSInt64(const AValue: Int64);
    procedure SetSInt8(const AValue: SmallInt);
    procedure SetUInt8(const AValue: Byte);
    procedure SetUInt16(const AValue: Word);
    procedure SetUInt32(const AValue: LongWord);
    procedure SetUInt64(const AValue: QWord);
  protected
    function GetName: String; virtual; abstract;
  public
    function isFloatPoint: Boolean; virtual; abstract;
    function isReadOnly: Boolean; virtual; abstract;
    function BitSize: Integer; virtual; abstract;
    
    procedure SetValue(const Value; ValueBitSize: Integer); virtual; abstract;
    procedure GetValue(var Value; ValueBitSize: Integer); virtual; abstract;
    
    property SInt64: Int64 read GetSInt64 write SetSInt64;
    property UInt64: QWord read GetUInt64 write SetUInt64;
    
    property SInt32: Integer  read GetSInt32 write SetSInt32;
    property UInt32: LongWord read GetUInt32 write SetUInt32;

    property SInt16: ShortInt read GetSInt16 write SetSInt16;
    property UInt16: Word read GetUInt16 write SetUInt16;
    
    property SInt8: SmallInt read GetSInt8 write SetSInt8;
    property UInt8: Byte read GetUInt8 write SetUInt8;
    
    property DbgPtr: TDbgPtr read GetDbgPtr write SetDbgPtr;
  
    property Name: String read GetName;
  end;
  
  TDbgDataList = class(TObject)
  protected
    function GetCount: Integer; virtual; abstract;
    function GetRegister(const Name: String): TDbgData; virtual; abstract;
  public
    property Count: Integer read GetCount;
    property Reg[const Name: String]: TDbgData read GetRegister; default;
    function RegByIndex(idx: Integer): TDbgData; virtual; abstract;
  end;
  
  TDbgState = (ds_Nonstarted, ds_ReadToRun, ds_Running, ds_Terminated);
   
  TDbgEventKind = (
    dek_Other, dek_SysExc, dek_SingleStep, dek_BreakPoint, 
    dek_ProcessStart, dek_ProcessTerminated, dek_SysCall);
  
  TDbgEvent = record
    Addr    : TDbgPtr;
    Thread  : TDbgThreadID;
    Kind    : TDbgEventKind;
    Debug   : String;
  end;   
  
  TDbgRunMode = (drm_Normal, drm_Suspended, drm_SingleStep);
  
  { TDbgTarget }

  TDbgTarget = class(TObject)
  protected
  public
    procedure Terminate; virtual; abstract;
    function WaitNextEvent(var Event: TDbgEvent): Boolean; virtual; abstract;
    function GetProcessState(AProcess: TDbgProcessID): TDbgState; virtual; abstract;
    
    function GetThreadsCount(AProcess: TDbgProcessID): Integer; virtual; abstract;
    function GetThreadID(AProcess: TDbgProcessID; AIndex: Integer): TDbgThreadID; virtual; abstract;
    function GetThreadRegs(AProcess: TDbgProcessID; ThreadID: TDbgThreadID; Registers: TDbgDataList): Boolean; virtual; abstract;
    function SetThreadRegs(AProcess: TDbgProcessID; ThreadID: TDbgThreadID; Registers: TDbgDataList): Boolean; virtual; abstract;

    function SetSingleStep(AProcess: TDbgProcessID; ThreadID: TDbgThreadID): Boolean; virtual; abstract;

    //todo: remove TDbgProcessID=0
    function MainThreadID(AProcess: TDbgProcessID=0): TDbgThreadID; virtual;
    
    function ReadMem(AProcess: TDbgProcessID; Offset: TDbgPtr; Count: Integer; var Data: array of byte): Integer; virtual; abstract;
    function WriteMem(AProcess: TDbgProcessID; Offset: TDbgPtr; Count: Integer; const Data: array of byte): Integer; virtual; abstract;
  end;

var
  DebugProcessStart: function(const ACmdLine: String): TDbgTarget = nil;

implementation

function DummyDebugProcessStart(const ACmdLine: String): TDbgTarget;
begin
  Result := nil;
end;

procedure InitDummyDebug;
begin
  if @DebugProcessStart = nil then
    DebugProcessStart := @DummyDebugProcessStart;
end;

{ TDbgRegister }

function TDbgData.GetUInt8: Byte; 
begin
  GetValue(Result, 8);
end;

function TDbgData.GetSInt8: SmallInt;
begin
  GetValue(Result, 8);
end;

function TDbgData.GetDbgPtr: TDbgPtr;
begin
  GetValue(Result, SizeOf(DbgPtr)*8);
end;

function TDbgData.GetSInt16: ShortInt;
begin
  GetValue(Result, 16);
end;

function TDbgData.GetUInt16: Word;
begin
  GetValue(Result, 16);
end;

function TDbgData.GetSInt32: Integer;
begin
  GetValue(Result, 32);
end;

function TDbgData.GetUInt32: LongWord;
begin
  GetValue(Result, 32);
end;

function TDbgData.GetSInt64: Int64;
begin
  GetValue(Result, 64);
end;

function TDbgData.GetUInt64: QWord;
begin
  GetValue(Result, 64);
end;

procedure TDbgData.SetDbgPtr(const AValue: TDbgPtr);
begin
  SetValue(AValue, Sizeof(TDbgPtr)*8);
end;

procedure TDbgData.SetSInt16(const AValue: ShortInt);
begin
  SetValue(AValue, 16);
end;

procedure TDbgData.SetSInt32(const AValue: Integer);
begin
  SetValue(AValue, 32);
end;

procedure TDbgData.SetSInt64(const AValue: Int64);
begin
  SetValue(AValue, 64);
end;

procedure TDbgData.SetSInt8(const AValue: SmallInt);
begin
  SetValue(AValue, 8);
end;

procedure TDbgData.SetUInt8(const AValue: Byte); 
begin
  SetValue(AValue, 8);
end;

procedure TDbgData.SetUInt16(const AValue: Word);
begin
  SetValue(AValue, 16);
end;

procedure TDbgData.SetUInt32(const AValue: LongWord);
begin
  SetValue(AValue, 32);
end;

procedure TDbgData.SetUInt64(const AValue: QWord); 
begin
  SetValue(AValue, 64);
end;

{ TDbgTarget }

function TDbgTarget.MainThreadID(aprocess: TDbgProcessID): TDbgThreadID;
begin
  Result := GetThreadID(aprocess, 0);
end;

initialization
  InitDummyDebug;


end.

