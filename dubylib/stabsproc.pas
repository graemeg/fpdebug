unit stabsProc; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, stabs;
  
type
  TStabSymArray = array [Word] of TStabSym;
  PStabSymArray = ^TStabSymArray;
  
  TStabProcParams = record
    Name    : AnsiString;
    SrcLine : LongWord;
  end;

  TVarLocation = (vlStack, vlRegister);

  TStabType = (
    stInt8,  stInt16, stInt32,
    stUInt8, stUInt16, stUInt32,
    stBool8, stBool16, stBool32,
    stChar, stWideChar,
    stSingle, stDouble, stLongDouble,
    stShortReal, stReal,
    stArray,
    stRecord, stUnion,
    stEnum,
    stRange,
    stAlias, stPointer, stVoid
  );

  TRecUnionElement = class;

  TEnumValue = record
    EnumName  : AnsiString;
    IntValue  : LongWord;
  end;

  { TStabTypeDescr }

  TStabTypeDescr = class(TObject)
  private
    fBaseType : TStabType;
  public
    Name      : AnsiString;
    DeclLine  : Integer;
    Related   : TStabTypeDescr; // array, alias, range, pointer types. destroyed by itself

    Count     : Integer;  // count of elements or enumcount.
    Elements  : array of TRecUnionElement;
    Enum      : array of TEnumValue;

    ArrIndType : TStabTypeDescr; // array only
    ArrPacked  : Boolean;        // packed array

    RecordSize : Integer;    // sizeof record

    LowRange  : AnsiString;
    HighRange : AnsiString;
    constructor Create(AType: TStabType);
    destructor Destroy; override;
    function AddRecUnionElement: TRecUnionElement;
    function AddEnumElement(const EnumName: AnsiString; IntValue: LongWord): Boolean;
    property BaseType: TStabType read fBaseType;
  end;

  { TRecUnionElement }

  TRecUnionElement = class(TObject)
  private
    fOwnType  : Boolean;
  public
    Name      : AnsiString;
    ElemType  : TStabTypeDescr;
    BitOffset : Integer;
    BitSize   : Integer;
    destructor Destroy; override;
    procedure SetType(AType: TStabTypeDescr; AOwnType: Boolean);
  end;
  
  TStabsCallback = class(TObject)
  public
    procedure DeclareType(StabTypeDescr: TStabTypeDescr); virtual; abstract;
    procedure StartFile(const FileName: AnsiString; FirstAddr: LongWord); virtual; abstract;
    procedure DeclareLocalVar(const Name: AnsiString; Location: TVarLocation; Addr: LongWord); virtual; abstract;
    procedure DeclareGlobalVar(const Name: AnsiString; Addr: LongWord); virtual; abstract;

    procedure CodeLine(LineNum, Addr: LongWord); virtual; abstract;
    
    procedure StartProc(const Name: AnsiString; const StabParams : array of TStabProcParams; ParamsCount: Integer; LineNum: Integer; Addr: LongWord); virtual; abstract;
    procedure EndProc(const Name: AnsiString); virtual; abstract;
    
    procedure AsmSymbol(const SymName: AnsiString; Addr: LongWord); virtual; abstract;
  end;

procedure ReadStabs(const StabsBuf : array of Byte; StabsLen: Integer; 
  const StabStrBuf: array of byte; StabStrLen: Integer; Callback: TStabsCallback);

implementation

type
  { TStabsReader }

  TStabAddr = LongWord;

  TStabVar = class(TObject)
    Name      : AnsiString;
    Location  : TVarLocation;
  end;

  { TStabProc }

  TStabProc = class(TObject)
    Name      : AnsiString;
    Addr      : TStabAddr;
    Params    : TFPList;
    Locals    : TFPList;
    constructor Create;
    destructor Destroy; override;
    function AddLocal: TStabVar;
    function AddParam: TStabVar;
  end;

  TStabsReader = class(TObject)
  private
    fCallback       : TStabsCallback;
    
    fProcStack      : TFPList;

    fUnnamed        : TStringList;

    fSourceFileName : AnsiString;
    fSourceAddr     : TStabAddr;

    fLastType       : Byte;

    fTypes          : TFPList;
    
    function CurrentProcName: AnsiString;
    function CurrentProcAddr: PtrInt;
    function CurrentProc: TStabProc;
    function PushProc(const Name: AnsiString; Addr: PtrInt): TStabProc;
    procedure PopProc;

    procedure DoReadStabs(AType, Misc: Byte; Desc: Word; Value: TStabAddr; const AStr: AnsiString);
    procedure HandleSourceFile(AType, Misc: Byte; Desc: Word; Value: TStabAddr; const AStr: AnsiString);

    function StabTypeDecl(const TypeDec, TypeVal: AnsiString; Num: Integer): TStabTypeDescr;
    procedure HandleLSym(AType, Misc: Byte; LineNum: Word; Value: TStabAddr; const AStr: AnsiString);
    procedure HandleFunc(AType, Misc: Byte; Desc: Word; Value: TStabAddr; const AStr: AnsiString);
    procedure HandleLine(AType, Misc: Byte; Desc: Word; Value: TStabAddr; const AStr: AnsiString);
    procedure HandleAsmSym(AType, Misc: Byte; Desc: Word; Value: TStabAddr; const AStr: AnsiString);
    procedure HandleRPSym(AType, Misc: Byte; Desc: Word; Value: TStabAddr; const AStr: AnsiString);
    procedure HandleVariable(AType, Misc: Byte; Desc: Word; Value: TStabAddr; const AStr: AnsiString);

    function AddType(ATypeNum: Integer; AStabType: TStabType): TStabTypeDescr;
    function GetType(ATypeNum: Integer): TStabTypeDescr;
  public
    AbsoluteLineNumbesAddress  : Boolean;
    
    constructor Create;
    destructor Destroy; override;
    
    procedure ReadStabs(const StabsBuf : array of Byte; StabsLen: Integer; 
      const StabStrBuf: array of byte; StabStrLen: Integer; Callback: TStabsCallback);
  end;

{ TStabFunction }

constructor TStabProc.Create;
begin
  Params:=TFPList.Create;
  Locals:=TFPList.Create;
end;

destructor TStabProc.Destroy;
var
  i : Integer;
begin
  for i:=0 to Params.Count-1 do TObject(Params[i]).Free;
  Params.Free;
  for i:=0 to Locals.Count-1 do TObject(Locals[i]).Free;
  Locals.Free;
  inherited Destroy;
end;

function TStabProc.AddLocal:TStabVar;
begin
  Result:=TStabVar.Create;
  Locals.Add(Result);
end;

function TStabProc.AddParam: TStabVar;
begin
  Result:=TStabVar.Create;
  Params.Add(Result);
end;

procedure ReadStabs(const StabsBuf : array of Byte; StabsLen: Integer; 
  const StabStrBuf: array of byte; StabStrLen: Integer; Callback: TStabsCallback);
var
  reader: TStabsReader;
begin
  if StabsLen = 0 then Exit;
  reader:=TStabsReader.Create;
  try
    reader.ReadStabs(StabsBuf, StabsLen, StabStrBuf, StabStrLen, Callback);
  finally
    reader.Free;
  end;
end;

{ TStabsReader }

function TStabsReader.CurrentProcName: AnsiString; 
begin
  if Assigned(CurrentProc) then Result:=CurrentProc.Name
  else Result:='';
end;

function TStabsReader.CurrentProcAddr: PtrInt;
begin
  if Assigned(CurrentProc) then Result:=CurrentProc.Addr
  else Result:=0;
end;

function TStabsReader.CurrentProc: TStabProc;
begin
  if fProcStack.Count=0 then Result:=nil
  else Result:=TStabProc(fProcStack[fProcStack.Count-1]);
end;

function TStabsReader.PushProc(const Name: AnsiString; Addr: PtrInt):TStabProc;
begin
  Result:=TStabProc.Create;
  Result.Name:=Name;
  Result.Addr:=Addr;
  fProcStack.Add(Result);
end;

procedure TStabsReader.PopProc; 
begin
  if fProcStack.Count>0 then begin
    TStabProc(fProcStack[fProcStack.Count-1]).Free;
    fProcStack.Delete(fProcStack.Count-1);
  end;
end;

procedure TStabsReader.DoReadStabs(AType, Misc: Byte; Desc: Word; Value: LongWord; const AStr: AnsiString);
begin
  case fLastType of
    N_SO:
      if (AType<>N_SO) and Assigned(fCallback) then
        fCallback.StartFile(fSourceFileName, fSourceAddr);
  end;

  case AType of
    N_SO:
      HandleSourceFile(AType, Misc, Desc, Value, AStr);
    N_LSYM:
      HandleLSym(AType, Misc, Desc, Value, AStr);
    N_FUN:
      HandleFunc(AType, Misc, Desc, Value, AStr);
    N_SLINE:
      HandleLine(AType, Misc, Desc, Value, AStr);
    N_RSYM, N_PSYM:
      HandleRPSym(AType, Misc, Desc, Value, AStr);
    N_LCSYM:
      HandleVariable(AType, Misc, Desc, Value, AStr);
    N_EXT, N_TYPE, N_EXTTYPE, N_PEXTTYPE:
      HandleAsmSym(AType, Misc, Desc, Value, AStr);
  end;

  fLastType:=AType;
end;

procedure TStabsReader.HandleSourceFile(AType, Misc: Byte; Desc: Word; Value: TStabAddr; const AStr: AnsiString);
var
  fileaddr  : LongWord;
  filename  : AnsiString;
begin
  if fSourceAddr=Value then
    fSourceFileName:=fSourceFileName+AStr
  else begin
    fSourceFileName:=AStr;
    fSourceAddr:=Value;
  end;
end;

function isRangeCommonType(ATypeNum, ARangeTypEnum: Integer;
  const LowRange, HighRange: AnsiString; var CommonType: TStabType): Boolean;
const
  LowMaxInt32  = '-2147483648';
  HighMaxInt32 = '2147483647';
begin
  Result:=ATypeNum=ARangeTypeNum;
  if Result then begin
    if (LowRange = LowMaxInt32) and (HighRange = HighMaxInt32) then CommonType:=stInt32
    else Result:=False;
  end;
end;

type
  TStabAttribis = record
    BitSize : Integer;
  end;

procedure CheckValueForAttr(const TypeVal: AnsiString; var CheckedVal: AnsiString; var Attr: TStabAttribis);
var
  i : Integer;
  s : AnsiString;
begin
  FillChar(Attr, SizeOf(Attr), 0);
  i:=1;
  while NextValAttr(TypeVal, i, s) do begin
    if s<>'' then
      case s[1] of
        's': GetBitSizeAttr(s, attr.BitSize);
      end;
  end;
  if i=1 then CheckedVal:=TypeVal
  else CheckedVal:=Copy(TypeVal, i, length(TypeVal)-i+1);
end;

const
  NumChars    = ['0'..'9'];
  SymChars    = ['+','-'];
  SymNumChars = SymChars+NumChars;

function GetNextNumber(const s: String; var index: Integer; var numStr: string): Boolean;
var
  j : Integer;
begin
  Result:=(index>=1) and (index<=length(s)) and (s[index] in SymNumChars);
  if not Result then Exit;
  j:=index;
  if s[j] in SymChars then inc(j);
  Result:=(j<=length(s)) and (s[j] in NumChars);
  if not Result then Exit;
  while (j<=length(s)) and (s[j] in NumChars) do inc(j);
  numStr:=copy(s, index, j-index);
  index:=j;
end;

function TStabsReader.StabTypeDecl(const TypeDec,TypeVal:AnsiString; Num: Integer): TStabTypeDescr;
var
  typenum   : Integer;
  lowr      : AnsiString;
  highr     : AnsiString;
  st        : TStabType;
  stabt     : TStabTypeDescr;
  typedesc  : AnsiString;
  rngval    : AnsiString;
  idx       : Integer;
  isPacked  : Boolean;
  nm, vl    : AnsiString;
  v         : AnsiString;
  LongVal   : LongWord;
  j, err    : Integer;
  attribs   : TStabAttribis;

  recSz     : Integer;
  recElName : String;
  recElsz   : Integer;
  recElOfs  : Integer;
  recElOwn  : Boolean;
  recElType : TStabTypeDescr;
  recElem   : TRecUnionElement;
begin
  v:=TypeVal;
  CheckValueForAttr(TypeVal, v, attribs);
  stabt:=nil;
  if v='' then begin
    stabt:=GetType(num);
  end else if isSymTypeRange(v) then begin
    ParseSymTypeSubRangeVal(v, typenum, lowr, highr);
    if isRangeCommonType(num, typenum, lowr, highr, st) then begin
      stabt:=AddType(num, st)
    end else begin
      stabt:=AddType(num, stRange);
      stabt.Related:=GetType(typenum);
    end;
    stabt.LowRange:=lowr;
    stabt.HighRange:=highr;
  end else if isSymTypeArray(v) then begin
    GetArrayIndexTypeRange(v, rngval, idx, isPacked);
    stabt:=AddType(num, stArray);
    stabt.ArrPacked:=isPacked;
    if ParseSymTypeSubRangeVal(rngval, typenum, lowr, highr) then begin
      stabt.ArrIndType:=TStabTypeDescr.Create(stRange);
      stabt.LowRange:=Lowr;
      stabt.HighRange:=highr;
      stabt.Related:=GetType(typenum);
    end else
      {todo: non ranged indexes};
  end else if isSymTypeEnum(v) then begin
    idx:=2;
    stabt:=AddType(num, stEnum);
    while NextEnumVal(v, idx, nm, vl) do begin
      Val(vl, LongVal, err);
      stabt.AddEnumElement(nm, LongVal);
    end;
  end else if isSymTypeStruct(v) then begin
    stabt:=AddType(num, stRecord);
    ParseStructSize(v, recsz, idx);

    while NextStructElem(v, idx, recElName, idx) do begin
      recElType:=nil;
      if (v[idx] in ['0'..'9'])  then begin
        GetNextNumber(v, idx, nm);
        Val(nm, j, err);
        if err=0 then begin
          recElType:=GetType(j);
          recElOwn:=not Assigned(recElType);
        end;
        //inc(idx, length(nm)+1);
      end else {todo: parsing declared-in types};

      if not Assigned(recElType) then begin
        recElOwn:=True;
        recElType:=TStabTypeDescr.Create(stVoid);
      end;
      inc(idx);
      if not NextStructElemPos(v, idx, recElOfs, recElSz, idx) then Break;
      recElem:=stabt.AddRecUnionElement;
      recElem.Name:=recElName;
      recElem.SetType(recElType, recElOwn);
      recElem.BitOffset:=recElOfs;
      recElem.BitSize:=recElSz;
    end;

  end else begin
    if ParseSymTypeVal(v, typenum, typedesc) then begin
      if typenum=num then
        stabt:=AddType(num, stVoid)
      else if typedesc=SymType_Pointer then begin
        stabt:=AddType(num, stPointer);
        stabt.Related:=GetType(typeNum);
      end
    end;
  end;
  Result:=stabt;
end;

procedure TStabsReader.HandleLSym(AType, Misc: Byte; LineNum: Word; Value: TStabAddr; const AStr: AnsiString);
var
  name, desc, v : AnsiString;
  num       : Integer;
  typedescr : TStabTypeDescr;
begin
  ParseStabStr( AStr, name, desc, num, v );
  if desc = '' then Exit;

  case desc[1] of
    Sym_TypeName, Sym_StructType:
    begin
      typedescr:=StabTypeDecl(desc, v, num);
      if Assigned(typedescr) then begin
        typedescr.DeclLine:=LineNum;
        typedescr.Name:=name;
        if typedescr.Name<>'' then
          fCallback.DeclareType(typedescr);
      end;
    end;
  end;

end;

procedure TStabsReader.HandleFunc(AType, Misc: Byte; Desc: Word; Value: TStabAddr; const AStr: AnsiString);
var
  funsym  : TStabSym;
  Params  : array of TStabProcParams;
  i, j    : integer;
  funnm   : AnsiString;
begin
  SetLength(Params, 0);
  StabFuncStr(AStr, funnm);
  if funnm<>'' then begin
    PushProc(funnm, Value );
    if Assigned(fCallback) then
      fCallback.StartProc(funnm, Params, j, funsym.n_desc, funsym.n_value );
  end else begin
    if ASsigned(fCallback) then
      fCallback.EndProc( CurrentProcName);
    PopProc;
  end;

end;

procedure TStabsReader.HandleLine(AType, Misc: Byte; Desc: Word; Value: TStabAddr; const AStr: AnsiString);
var
  v : LongWord;
begin
  if Assigned(fCallback) then begin
    v := Value;
    if not AbsoluteLineNumbesAddress then
      v := CurrentProcAddr + v;
    fCallback.CodeLine(Desc, v);
  end;
end;

procedure TStabsReader.HandleAsmSym(AType, Misc: Byte; Desc: Word; Value: TStabAddr; const AStr: AnsiString);
begin
  if Assigned(fCallback) then
    fCallBack.AsmSymbol(AStr, Value);
end;

procedure TStabsReader.HandleRPSym(AType,Misc:Byte;Desc:Word;Value:TStabAddr; const AStr:AnsiString);
var
  varname : AnsiString;
  vardesc : AnsiString;
  varType : Integer;

  isArg   : Boolean; // is procedure argument
begin
  StabVarStr(AStr, varname, vardesc, vartype);

  if Assigned(CurrentProc) then begin
    isArg := (AType=N_RSYM) and isProcArgument(vardesc);

    if isArg then
      CurrentProc.AddParam.Name:=varname
    else
      CurrentProc.AddLocal.Name:=varname;
  end;

//  writeln('name: ', varname);
//  writeln('type: ', vartype);
{  i := index;
  j := 0;
  for i := index to StabsCnt - 1 do begin
    if (Stabs^[i].n_type = N_PSYM) then begin
      if j = length(Params) then begin
        if j = 0 then SetLength(Params, 4)
        else SetLength(Params, j * 4);
      end;
      StabFuncStr( StabStr(Stabs^[index].n_strx), Params[j].Name);
      inc(j);
    end else
      Break;
  end;}
end;

procedure TStabsReader.HandleVariable(AType, Misc: Byte; Desc: Word; Value: TStabAddr; const AStr: AnsiString);
var
  global  : Boolean;
  varname : AnsiString;
  vardesc : AnsiString;
  vartype : Integer;
begin
  global := AType = N_LCSYM;
  StabVarStr( AStr, varname, vardesc, vartype );
  if global then
    fCallback.DeclareGlobalVar(varname, Value )
  else
    fCallback.DeclareLocalVar(varname, vlRegister, Value );
end;

constructor TStabsReader.Create; 
begin
  fProcStack:=TFPList.Create;
  fTypes:=TFPList.Create;
end;

destructor TStabsReader.Destroy;
var
  i : Integer;
begin
  for i:=0 to fProcStack.Count-1 do TObject(fProcStack[i]).Free;
  fProcStack.Clear;
  fProcStack.Free;
  for i:=0 to fTypes.Count-1 do TObject(fTypes[i]).Free;
  fTypes.Free;
  inherited Destroy;
end;

procedure TStabsReader.ReadStabs(const StabsBuf: array of Byte; StabsLen: Integer;  
  const StabStrBuf: array of byte; StabStrLen: Integer; Callback: TStabsCallback); 
begin
  fCallback:=Callback;

  fSourceFileName:=''; // N_SO
  fSourceAddr:=0;

  ReadStabSyms(StabsBuf, StabStrBuf, StabsLen div sizeof(TStabSym) , StabStrLen, @DoReadStabs);
end;


function TStabsReader.AddType(ATypeNum: Integer; AStabType: TStabType): TStabTypeDescr;
begin
  Result:=TStabTypeDescr.Create(AStabType);
  if fTypes.Count<=ATypeNum then
    fTypes.Count:=ATypeNum+1;
  if Assigned(fTypes[ATypeNum]) then TObject(fTypes[ATypeNum]).Free;
  fTypes[ATypeNum]:=Result;
end;

function TStabsReader.GetType(ATypeNum: Integer): TStabTypeDescr;
begin
  if (ATypeNum>=0) and (ATypeNum<fTypes.Count) then
    Result:=TStabTypeDescr(fTypes[ATypeNum])
  else
    Result:=nil;
end;

{ TStabTypeDescr }

constructor TStabTypeDescr.Create(AType:TStabType);
begin
  inherited Create;
  fBaseType:=AType;
end;

destructor TStabTypeDescr.Destroy;
var
  i : Integer;
begin
  case fBaseType of
    stRecord:
      for i:=0 to Count-1 do Elements[i].Free;
  end;

  inherited Destroy;
end;

function TStabTypeDescr.AddRecUnionElement:TRecUnionElement;
begin
  if (fBaseType<>stRecord) then begin
    Result:=nil;
    Exit;
  end;
  if Count=length(Elements) then begin
    if Count=0 then SetLength(Elements, 4)
    else SetLength(Elements, Count*2);
  end;
  Result:=TRecUnionElement.Create;
  Elements[Count]:=Result;
  inc(Count);
end;

function TStabTypeDescr.AddEnumElement(const EnumName:AnsiString; IntValue:LongWord): Boolean;
begin
  Result:=fBaseType=stEnum;
  if not Result then Exit;
  if Count=length(Enum) then begin
    if Count=0 then SetLength(Enum, 4)
    else SetLength(Enum, Count*2);
  end;
  Enum[Count].EnumName:=EnumName;
  Enum[Count].IntValue:=IntValue;
  inc(Count);
end;

{ TRecUnionElement }

destructor TRecUnionElement.Destroy;
begin
  if fOwnType then ElemType.Free;
  inherited Destroy;
end;

procedure TRecUnionElement.SetType(AType:TStabTypeDescr; AOwnType: Boolean);
begin
  if fOwnType then ElemType.Free;
  writeln('setting a type');
  fOwnType:=AOwnType;
  ElemType:=AType;
end;

end.

