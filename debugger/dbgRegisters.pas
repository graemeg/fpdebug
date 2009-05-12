unit dbgRegisters; 

{$mode objfpc}{$H+}

interface

uses
  dbgTypes, contnrs;
 
type
  
  { TDbgRegisterItem }

  TDbgRegisterItem = class(TDbgRegister)
  private
    fData       : array of Byte;
    fBitSize    : Integer;
    fisReadOnly : Boolean;
    fisFloat    : Boolean;
  public
    constructor Create(ABitSize: Integer; AReadOnly, AFloat: Boolean);
    function isFloatPoint: Boolean; override;
    function isReadOnly: Boolean; override;
    function BitSize: Integer; override;
    
    procedure SetValue(const Value; ValueBitSize: Integer); override;
    procedure GetValue(var Value; ValueBitSize: Integer); override;
  end;
  
  { TDbgRegistersList }

  TDbgRegistersList = class(TDbgRegisters)
  private
    fRegs : TFPHashObjectList;      
  protected
    function GetCount: Integer; override;
    function GetRegister(const Name: String): TDbgRegister; override;
  public
    constructor Create;
    destructor Destroy; override;
    
    procedure AddRegister(const Name: String; ABitSize: Integer; isReadOnly, isFloat: Boolean);
  end;


implementation

function Max(a,b: Integer): Integer;
begin
  if a > b then Result := a
  else Result := b;
end;

function Min(a,b: Integer): Integer;
begin
  if a < b then Result := a
  else Result := b;
end;



{ TDbgRegisterItem }

constructor TDbgRegisterItem.Create(ABitSize: Integer; AReadOnly,  
  AFloat: Boolean); 
begin
  inherited Create;
  fBitSize := ABitSize;
  fisReadOnly:=AReadOnly;
  fisFloat:=AFloat;
  if fBitSize div 8 = 0 
    then SetLength(fData, 1)
    else SetLength(fData, fBitSize div 8);
end;

function TDbgRegisterItem.isFloatPoint: Boolean;  
begin
  Result:=fisFloat;
end;

function TDbgRegisterItem.isReadOnly: Boolean;  
begin
  Result:=fisReadOnly;
end;

function TDbgRegisterItem.BitSize: Integer;  
begin
  Result:=fBitSize;
end;

procedure TDbgRegisterItem.SetValue(const Value; ValueBitSize: Integer);  
var
  sz : Integer;
begin
  sz := ValueBitSize div 8;
  if sz = 0 then sz := 1;
  Move(Value, fData[0], Min(sz, length(fData)) );
end;

procedure TDbgRegisterItem.GetValue(var Value; ValueBitSize: Integer);  
var
  sz : Integer;
begin
  sz := ValueBitSize div 8;
  if sz = 0 then sz := 1;
  Move(fData[0], Value, Min(sz, length(fData)));
end;

{ TDbgRegistersList }

function TDbgRegistersList.GetCount: Integer;  
begin
  Result := fRegs.Count;
end;

function TDbgRegistersList.GetRegister(const Name: String): TDbgRegister;  
begin
  Result := TDbgRegister(fRegs.Find(Name));
end;

constructor TDbgRegistersList.Create; 
begin
  fRegs := TFPHashObjectList.Create(true);
end;

destructor TDbgRegistersList.Destroy;  
begin
  fRegs.Free;
  inherited Destroy;  
end;

procedure TDbgRegistersList.AddRegister(const Name: String; ABitSize: Integer;  
  isReadOnly, isFloat: Boolean); 
var
  r : TDbgRegisterItem;
begin
  r := TDbgRegisterItem(GetRegister(Name));
  if Assigned(r) then fRegs.Remove(r);
  r :=  TDbgRegisterItem.Create(ABitSize, isReadOnly, isFloat);
  fRegs.Add(Name, r);
end;

end.


