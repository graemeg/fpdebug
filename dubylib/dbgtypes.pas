unit dbgTypes; 

{$mode objfpc}{$H+}

interface

type
  TDbgPtr      = PtrUInt;
  TDbgThreadID = TThreadID;
  
  
  { TDbgRegister }

  TDbgRegister = class(TObject)
  private
    function GetSInt16: ShortInt;
    function GetSInt32: Integer;
    function GetSInt64: Int64;
    function GetSInt8: SmallInt;
    function GetUInt8: Byte;
    function GetUInt16: Word;
    function GetUInt32: LongWord;
    function GetUInt64: QWord;
    procedure SetSInt16(const AValue: ShortInt);
    procedure SetSInt32(const AValue: Integer);
    procedure SetSInt64(const AValue: Int64);
    procedure SetSInt8(const AValue: SmallInt);
    procedure SetUInt8(const AValue: Byte);
    procedure SetUInt16(const AValue: Word);
    procedure SetUInt32(const AValue: LongWord);
    procedure SetUInt64(const AValue: QWord);
    
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
  end;
  
  TDbgRegisters = class(TObject)
  protected
    function GetCount: Integer; virtual; abstract;
    function GetRegister(const Name: String): TDbgRegister; virtual; abstract;
  public
    property Count: Integer read GetCount;
    property Reg[const Name: String]: TDbgRegister read GetRegister; default;
  end;
  
  TDbgState = (ds_Nonstarted, ds_ReadToRun, ds_Running, ds_Terminated);
   
  TDbgEventKind = (dek_Other, dek_BreakPoint, dek_ProcessStart, dek_ProcessTerminated);
  
  TDbgEvent = record
    Addr    : TDbgPtr;
    Thread  : TDbgThreadID;
    Kind    : TDbgEventKind;
    Debug   : String;
  end;   
  
  TDbgProcess = class(TObject)
  protected
  public
    procedure Terminate; virtual; abstract;
    function WaitNextEvent(var Event: TDbgEvent): Boolean; virtual; abstract;
    function GetProcessState: TDbgState; virtual; abstract;
    
    function GetThreadsCount: Integer; virtual; abstract;
    function GetThreadID(AIndex: Integer): TDbgThreadID; virtual; abstract;
    function GetThreadRegs(ThreadID: TDbgThreadID; Regs: TDbgRegisters): Boolean; virtual; abstract;
    
    function ReadMem(Offset: TDbgPtr; Count: Integer; var Data: array of byte): Integer; virtual; abstract;
    function WriteMem(Offset: TDbgPtr; Count: Integer; const Data: array of byte): Integer; virtual; abstract;
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

{ TDbgRegister }

function TDbgRegister.GetUInt8: Byte; 
begin
  GetValue(Result, 8);
end;

function TDbgRegister.GetSInt8: SmallInt;
begin
  GetValue(Result, 8);
end;

function TDbgRegister.GetSInt16: ShortInt;
begin
  GetValue(Result, 16);
end;

function TDbgRegister.GetUInt16: Word;
begin
  GetValue(Result, 16);
end;

function TDbgRegister.GetSInt32: Integer;
begin
  GetValue(Result, 32);
end;

function TDbgRegister.GetUInt32: LongWord;
begin
  GetValue(Result, 32);
end;

function TDbgRegister.GetSInt64: Int64;
begin
  GetValue(Result, 64);
end;

function TDbgRegister.GetUInt64: QWord;
begin
  GetValue(Result, 64);
end;

procedure TDbgRegister.SetSInt16(const AValue: ShortInt);
begin
  SetValue(AValue, 16);
end;

procedure TDbgRegister.SetSInt32(const AValue: Integer);
begin
  SetValue(AValue, 32);
end;

procedure TDbgRegister.SetSInt64(const AValue: Int64);
begin
  SetValue(AValue, 64);
end;

procedure TDbgRegister.SetSInt8(const AValue: SmallInt);
begin
  SetValue(AValue, 8);
end;

procedure TDbgRegister.SetUInt8(const AValue: Byte); 
begin
  SetValue(AValue, 8);
end;

procedure TDbgRegister.SetUInt16(const AValue: Word);
begin
  SetValue(AValue, 16);
end;

procedure TDbgRegister.SetUInt32(const AValue: LongWord);
begin
  SetValue(AValue, 32);
end;

procedure TDbgRegister.SetUInt64(const AValue: QWord); 
begin
  SetValue(AValue, 64);
end;

initialization
  InitDummyDebug;


end.
