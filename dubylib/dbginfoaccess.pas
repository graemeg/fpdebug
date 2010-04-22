unit dbgInfoAccess;

interface

type
  { TAccessEntry }

  TAccessEntry = class(TObject)
    Next : TAccessEntry;
    destructor Destroy; override;
    function isEqual(Entry: TAccessEntry): Boolean; virtual;
  end;

  { TAddrAcc }

  TAddrAcc = class(TAccessEntry)
    Addr    : QWord;
    constructor Create(AAddr: QWord);
    function isEqual(Entry: TAccessEntry): Boolean; override;
  end;

  { TRegAcc }

  TRegAcc = class(TAccessEntry)
    RegNum  : Integer;
    constructor Create(ARegNum: Integer);
    function isEqual(Entry: TAccessEntry): Boolean; override;
  end;

  { TRegRelativeAcc }

  TRegRelativeAcc  = class(TAccessEntry)
    RegNum  : Integer;
    Offset  : QWord;
    constructor Create(ARegNum: Integer; AOffset: QWord);
    function isEqual(Entry: TAccessEntry): Boolean; override;
  end;

  { TCallProcAcc }

  TCallProcAcc = class(TAccessEntry)
    ProcName  : AnsiString;
    //todo: function params!
    constructor Create(const AProcName: AnsiString);
    function isEqual(Entry: TAccessEntry): Boolean; override;
  end;

function isEqualEntry(a1, a2: TAccessEntry): Boolean;
function isEqualSeq(a1, a2:  TAccessEntry): Boolean;

implementation

function isEqualEntry(a1, a2: TAccessEntry): Boolean;
begin
  Result:=a1=a2;
  if Result then Exit;
  Result:=a1.ClassType=a2.ClassType;
  if not Result then Exit;
end;

function isEqualSeq(a1, a2:  TAccessEntry): Boolean;
begin
  while isEqualEntry(a1, a2) do begin
    a1:=a1.Next;
    a2:=a2.Next;
  end;
  Result:=(a1=a2) and (a1=nil);
end;

{ TAccessEntry }

function TAccessEntry.isEqual(Entry:TAccessEntry):Boolean;
begin
  Result:=(Entry<>nil) and (Entry.ClassType=Self.ClassType)
end;

destructor TAccessEntry.Destroy;
begin
  Next.Free;
  Next:=nil;
  inherited Destroy;
end;

{ TAddrAcc }

constructor TAddrAcc.Create(AAddr:QWord);
begin
  inherited Create;
  Addr:=AAddr;
end;

function TAddrAcc.isEqual(Entry:TAccessEntry):Boolean;
begin
  Result:=inherited isEqual(Entry);
  if Result then
    Result:=TAddrAcc(Entry).Addr=Addr;
end;

{ TRegAcc }

constructor TRegAcc.Create(ARegNum:Integer);
begin
  inherited Create;
  RegNum:=ARegNum;
end;

function TRegAcc.isEqual(Entry:TAccessEntry):Boolean;
begin
  Result:=inherited isEqual(Entry);
  if Result then
    Result:=TRegAcc(Entry).RegNum=RegNum;
end;

{ TRegRelativeAcc }

constructor TRegRelativeAcc.Create(ARegNum:Integer;AOffset:QWord);
begin
  inherited Create;
  RegNum:=ARegNum;
end;

function TRegRelativeAcc.isEqual(Entry:TAccessEntry):Boolean;
begin
  Result:=inherited isEqual(Entry);
  if Result then
    Result:=(TRegRelativeAcc(Entry).RegNum=RegNum) and (TRegRelativeAcc(Entry).Offset=Offset);
end;

{ TCallProcAcc }

constructor TCallProcAcc.Create(const AProcName:AnsiString);
begin
  inherited Create;
  ProcName:=AProcName;
end;

function TCallProcAcc.isEqual(Entry:TAccessEntry):Boolean;
begin
  Result:=inherited isEqual(Entry);
  if Result then
    Result:=(TCallProcAcc(Entry).ProcName=ProcName);
end;

end.
