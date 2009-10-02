unit dbgUtils;

{$ifdef fpc}{$mode delphi}{$h+}{$endif}

interface

uses
  Classes, SysUtils, dbgTypes;

type
  TDbgDataBytes = class(TDbgData)
  private
    fData     : array of Byte;
    fisFloat    : Boolean;
    fisReadOnly : Boolean;
    fBitSize    : Integer;
    fName       : string;
  protected
    function GetName: String; override;
  public
    procedure SetValue(const Value; ValueBitSize: Integer); override;
    procedure GetValue(var Value; ValueBitSize: Integer); override;
    function isFloatPoint: Boolean; override;
    function isReadOnly: Boolean; override;
    function BitSize: Integer; override;
  end;

  TDbgDataBytesList = class(TDbgDataList)
  private
    fItems   : TStringList;
  protected
    function GetCount: Integer; override;
    function GetRegister(const Name: String): TDbgData; override;
  public
    constructor Create;
    destructor Destroy; override;
    property Count: Integer read GetCount;
    function RegByIndex(idx: Integer): TDbgData; override;
    procedure Clear;
  end;


function Max(a,b: Integer): Integer;
function Min(a,b: Integer): Integer;


function GetProcessRegisters(Process: TDbgProcess; data: TDbgDataList): Boolean; overload;
function GetProcessRegisters(Process: TDbgProcess): TDbgDataList; overload;

implementation

function Max(a,b: Integer): Integer;
begin
  if a < b then Result := b else Result := a;
end;

function Min(a,b: Integer): Integer;
begin
  if a < b then Result := a else Result := b;
end;

{ TDbgDataBytes }

function TDbgDataBytes.BitSize: Integer;
begin
  Result := fBitSize;
end;

function TDbgDataBytes.GetName: String;
begin
  Result := fName;
end;

procedure TDbgDataBytes.GetValue(var Value; ValueBitSize: Integer);
begin
  if fBitSize > 0 then
    Move(fData[0], Value, Min(fBitSize, ValueBitSize) div 8);
end;

function TDbgDataBytes.isFloatPoint: Boolean;
begin
  Result := fisFloat;
end;

function TDbgDataBytes.isReadOnly: Boolean;
begin
  Result := fisReadOnly;
end;

procedure TDbgDataBytes.SetValue(const Value; ValueBitSize: Integer);
begin
  fBitSize := ValueBitSize;
  if Length(fData) < fBitSize div 8 then
    SetLength(fData, fBitsize div 8);
  if fBitSize > 0 then Move(Value, fdata[0], ValueBitSize div 8);

end;

{ TDbgDataBytesList }

procedure TDbgDataBytesList.Clear;
var
  i : Integer;
begin
  for i := 0 to fItems.Count - 1 do
    TDbgDataBytes(fItems.Objects[i]).Free;
  fItems.Clear;
end;

constructor TDbgDataBytesList.Create;
begin
  fItems := TStringList.Create;
end;

destructor TDbgDataBytesList.Destroy;
begin
  Clear;
  fItems.Free;
  inherited;
end;

function TDbgDataBytesList.GetCount: Integer;
begin
  Result := fItems.Count;
end;

function TDbgDataBytesList.GetRegister(const Name: String): TDbgData;
var
  idx : Integer;
begin
  idx := fItems.IndexOf(Name);
  if idx < 0 then begin
    Result := TDbgDataBytes.Create;
    TDbgDataBytes(Result).fName := Name;
    fItems.AddObject(Name, Result)
  end else
    Result := TDbgData(fItems.Objects[idx]);
end;

function TDbgDataBytesList.RegByIndex(idx: Integer): TDbgData;
begin
  Result := TDbgData(fItems.Objects[idx]);
end;

function GetProcessRegisters(Process: TDbgProcess; data: TDbgDataList): Boolean; 
begin
  Result := false;
  if not Assigned(Process) or not Assigned(data) then Exit;
  Result := Process.GetThreadRegs(Process.MainThreadID, data);
end;

function GetProcessRegisters(Process: TDbgProcess): TDbgDataList; overload;
var
  list  : TDbgDataBytesList;
  res   : Boolean;
begin
  Result := nil;
  if not Assigned(Process) then Exit;
  list := TDbgDataBytesList.Create;
  res := GetProcessRegisters(Process, list);
  if not res then begin
    list.Free;
    Result := nil;
    Exit;
  end else
    Result := list;
end;


end.
