unit stabsProc; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, stabs;
  
type
  TStabSymArray = array [Word] of TStabSym;
  PStabSymArray = ^TStabSymArray;
  
  TStabProcParams = record
    Name    : String;    
    SrcLine : LongWord;
  end;

  TVarLocation = (vlStack, vlRegister);

  TStabTypeDescr = record
  end;
  
  TStabsCallback = class(TObject)
  public
    procedure DeclareType(const TypeName: AnsiString); virtual; abstract;
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

    fLastType   : Byte;
    
    function CurrentProcName: AnsiString;
    function CurrentProcAddr: PtrInt;
    function CurrentProc: TStabProc;
    function PushProc(const Name: AnsiString; Addr: PtrInt): TStabProc;
    procedure PopProc;

    procedure DoReadStabs(AType, Misc: Byte; Desc: Word; Value: TStabAddr; const AStr: String);
    procedure HandleSourceFile(AType, Misc: Byte; Desc: Word; Value: TStabAddr; const AStr: String);
    procedure HandleLSym(AType, Misc: Byte; ADesc: Word; Value: TStabAddr; const AStr: String);
    procedure HandleFunc(AType, Misc: Byte; Desc: Word; Value: TStabAddr; const AStr: String);
    procedure HandleLine(AType, Misc: Byte; Desc: Word; Value: TStabAddr; const AStr: String);
    procedure HandleAsmSym(AType, Misc: Byte; Desc: Word; Value: TStabAddr; const AStr: String);
    procedure HandleRPSym(AType, Misc: Byte; Desc: Word; Value: TStabAddr; const AStr: String);
    procedure HandleVariable(AType, Misc: Byte; Desc: Word; Value: TStabAddr; const AStr: String);
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

procedure TStabsReader.DoReadStabs(AType, Misc: Byte; Desc: Word; Value: LongWord; const AStr: String);
begin
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

  case fLastType of
    N_SO:
      if (AType<>N_SO) and Assigned(fCallback) then begin
        writeln('fSourcefile = ',fSourceFileName);
        fCallback.StartFile(fSourceFileName, fSourceAddr);
      end;
  end;

  fLastType:=AType;
end;

procedure TStabsReader.HandleSourceFile(AType, Misc: Byte; Desc: Word; Value: TStabAddr; const AStr: String);
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


procedure TStabsReader.HandleLSym(AType, Misc: Byte; ADesc: Word; Value: TStabAddr; const AStr: String);
var
  name, desc, v : string;
  num   : Integer;
begin
  ParseStabStr( AStr, name, desc, num, v );
  if desc = '' then Exit;

  case desc[1] of
    Sym_TypeName, Sym_StructType:
      fCallback.DeclareType(name);
  end;

end;

procedure TStabsReader.HandleFunc(AType, Misc: Byte; Desc: Word; Value: TStabAddr; const AStr: String);
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

procedure TStabsReader.HandleLine(AType, Misc: Byte; Desc: Word; Value: TStabAddr; const AStr: String);
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

procedure TStabsReader.HandleAsmSym(AType, Misc: Byte; Desc: Word; Value: TStabAddr; const AStr: String);
begin
  if Assigned(fCallback) then
    fCallBack.AsmSymbol(AStr, Value);
end;

procedure TStabsReader.HandleRPSym(AType,Misc:Byte;Desc:Word;Value:TStabAddr; const AStr:String);
var
  varname : string;
  vardesc : string;
  varType : Integer;

  isArg   : Boolean; // is procedure argument
begin
  writeln('AStr = ', AStr);

  StabVarStr(AStr, varname, vardesc, vartype);

  if Assigned(CurrentProc) then begin
    isArg := (AType=N_RSYM) and isRSymProcArgument(vardesc);

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

procedure TStabsReader.HandleVariable(AType, Misc: Byte; Desc: Word; Value: TStabAddr; const AStr: String);
var
  global  : Boolean;
  varname : String;
  vardesc : String;
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
  fProcStack := TFPList.Create;
end;

destructor TStabsReader.Destroy;
var
  i : Integer;
begin
  for i:=0 to fProcStack.Count-1 do TObject(fProcStack[i]).Free;
  fProcStack.Clear;
  fProcStack.Free;
  inherited Destroy;  
end;

procedure TStabsReader.ReadStabs(const StabsBuf: array of Byte; StabsLen: Integer;  
  const StabStrBuf: array of byte; StabStrLen: Integer; Callback: TStabsCallback); 
begin
  fCallback:=Callback;

  fSourceFileName:=''; // N_SO
  fSourceAddr:=0;

  ReadStabSyms(StabsBuf, StabStrBuf, StabsLen, StabStrLen, @DoReadStabs);
end;


end.

