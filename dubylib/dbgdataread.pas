unit dbgDataRead;

interface

uses
  SysUtils, Classes,
  dbgTypes, dbgInfoTypes;

type
  TDbgTypeRead = class(TObject)
  public
    function Dump(atype: TDbgSymType; const Data; DataSize: Integer): AnsiString; virtual; abstract;
  end;

  { TDbgSimpleTypeRead }

  TDbgSimpleTypeRead = class(TDbgTypeRead)
    function Dump(AType: TDbgSymType; const Data; DataSize: Integer): AnsiString; override;
  end;

procedure RegisterReader(ADbgTypeClass: TDbgSymClass; Reader: TDbgTypeRead);

implementation

var
  Readers : TFPList;

type
  { TTypeRead }

  TTypeRead=class(TObject)
    TypeClass : TDbgSymClass;
    Reader    : TDbgTypeRead;
    constructor Create(ATypeClass: TDbgSymClass; AReader: TDbgTypeRead);
    destructor Destroy; override;
  end;

constructor TTypeRead.Create(ATypeClass: TDbgSymClass; AReader: TDbgTypeRead);
begin
  inherited Create;
  TypeClass:=ATypeClass;
  Reader:=AReader;
end;

destructor TTypeRead.Destroy;
begin
  reader.Free;
  inherited;
end;

{ TDbgSimpleTypeRead }

function TDbgSimpleTypeRead.Dump(atype:TDbgSymType;const Data; DataSize:Integer): AnsiString;
type
  PReal = ^Real;
begin
  if not (AType is TDbgSymSimpleType) then begin
    Result:='';
    Exit;
  end;

  case TDbgSymSimpleType(atype).Simple of
    dstSInt8:   Result:=IntToStr( PShortInt(@Data)^);
    dstSInt16:  Result:=IntToStr( PSmallInt(@Data)^);
    dstSInt32:  Result:=IntToStr( PInteger(@Data)^);
    dstSInt64:  Result:=IntToStr( PInt64(@Data)^);
    dstUInt8:   Result:=IntToStr( PByte(@Data)^);
    dstUInt16:  Result:=IntToStr( PWord(@Data)^);
    dstUInt32:  Result:=IntToStr( PLongWord(@Data)^);
    dstUInt64:  Result:=IntToStr( PQWord(@Data)^);
    dstFloat32: Result:=FloatToStr( PSingle(@Data)^);
    dstFloat48: Result:=FloatToStr( PReal(@Data)^);
    dstFloat64: Result:=FloatToStr( PDouble(@Data)^);
    dstBool8:   Result:=BoolToStr( PBoolean(@Data)^, True);
    dstBool16:  Result:=BoolToStr( PWordBool(@Data)^, True);
    dstBool32:  Result:=BoolToStr( PLongBool(@Data)^, True);
    dstChar8:   Result:=PChar(@Data)^;
    dstChar16:  Result:=PWideChar(@Data)^;
  else
    Result:='';
  end;
end;

procedure RegisterReader(ADbgTypeClass: TDbgSymClass; Reader: TDbgTypeRead);
begin
  //todo:
  Readers.Add(Reader);
end;

procedure ReleaseVarReaders;
var
  i : Integer;
begin
  for i:=0 to Readers.Count-1 do TObject(Readers[i]).Free;
  Readers.Clear;
  Readers.Free;
end;

procedure InitVarReaders;
begin
  Readers:=TFPList.Create;
end;

initialization
  InitVarReaders;

finalization
  ReleaseVarReaders;

end.
