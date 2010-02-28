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

  TVarLocation = (vl_OnStack, vl_Register);

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

  TStabsReader = class(TObject)
  private
    Stabs     : PStabSymArray;
    StabsCnt  : Integer;
    StrSyms   : PByteArray;
    StrLen    : Integer;
    fCallback   : TStabsCallback;
    
    fProcStack  : TStringList;

    fUnnamed  : TStringList;
    
    function CurrentProcName: AnsiString;
    function CurrentProcAddr: PtrInt;
    procedure PushProc(const Name: AnsiString; Addr: PtrInt);
    procedure PopProc;

    function StabStr(strx: integer): AnsiString;
    
    procedure DoReadStabs;
    procedure HandleSourceFile(var index: Integer);
    procedure HandleLSym(var index: Integer);
    procedure HandleFunc(var index: Integer);
    procedure HandleLine(var index: Integer);
    procedure HandleAsmSym(var index: Integer);
    procedure HandleVariable(var index: Integer);
    
  public
    AbsoluteLineNumbesAddress  : Boolean;
    
    constructor Create;
    destructor Destroy; override;
    
    procedure ReadStabs(const StabsBuf : array of Byte; StabsLen: Integer; 
      const StabStrBuf: array of byte; StabStrLen: Integer; Callback: TStabsCallback);
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
  if fProcStack.Count > 0 then Result := fProcStack[fProcStack.Count-1]
  else Result := '';
end;

function TStabsReader.CurrentProcAddr: PtrInt;
begin
  if fProcStack.Count > 0 then Result := PtrInt(fProcStack.Objects[fProcStack.Count-1])
  else Result := 0;
end;

procedure TStabsReader.PushProc(const Name: AnsiString; Addr: PtrInt); 
begin
  fProcStack.AddObject(Name, TObject(Addr));
end;

procedure TStabsReader.PopProc; 
begin
  fProcStack.Delete(fProcStack.Count-1);
end;

function TStabsReader.StabStr(strx: integer): AnsiString; 
begin
  if strx = 0 then Result := ''
  else Result := PChar(@StrSyms^[strx]);
end;

procedure TStabsReader.DoReadStabs; 
var
  i     : Integer;
begin
  i := 0;  
  while i < StabsCnt do  begin
    case Stabs^[i].n_type of
      N_SO:   
        HandleSourceFile(i);
      N_LSYM: 
        HandleLSym(i);
      N_FUN:  
        HandleFunc(i);
      N_SLINE:
        HandleLine(i);
      N_LCSYM:
        HandleVariable(i);
      N_EXT, N_TYPE, N_EXTTYPE, N_PEXTTYPE:
        HandleAsmSym(i);
    else
      inc(i);    
    end;
  end;
end;

procedure TStabsReader.HandleSourceFile(var index: Integer); 
var
  fileaddr  : LongWord;
  filename  : AnsiString;
begin
  fileaddr := Stabs^[Index].n_value;
  filename := '';
  while (Stabs^[Index].n_type = N_SO) and (Stabs^[Index].n_value = fileaddr) do begin
    filename := filename + StabStr(Stabs^[Index].n_strx);
    inc(index);
  end;
  
  if Assigned(fCallback) then fCallback.StartFile(filename, fileaddr);
end;


procedure TStabsReader.HandleLSym(var index: Integer);
var
  name, desc, v : string;
  num   : Integer;
begin
  if not Assigned(fCallback) then begin
    inc(index);
    Exit;
  end;
  ParseStabStr( StabStr(Stabs^[Index].n_strx), name, desc, num, v );
  inc(index);
  if desc = '' then Exit;

  case desc[1] of
    Sym_TypeName, Sym_StructType:
      fCallback.DeclareType(name);
  end;

end;

procedure TStabsReader.HandleFunc(var index: Integer); 
var
  funsym  : TStabSym;
  Params  : array of TStabProcParams;
  i, j    : integer;
  funnm   : AnsiString;
begin
  SetLength(Params, 0);
  funsym := Stabs^[index];
  StabFuncStr( StabStr(Stabs^[index].n_strx), funnm);

  inc(index);
  
  if funnm = '' then begin
    if Assigned(fCallback) then fCallback.EndProc( CurrentProcName );
    PopProc;
    Exit;
  end;
  
  i := index;
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
  end;

  PushProc(funnm, Stabs^[i].n_value );
  if Assigned(fCallback) then 
    fCallback.StartProc(funnm, Params, j, funsym.n_desc, funsym.n_value );
end;

procedure TStabsReader.HandleLine(var index: Integer); 
var
  v : Integer;
begin
  if Assigned(fCallback) then begin
    v := Stabs^[index].n_value;
    if not AbsoluteLineNumbesAddress then v := CurrentProcAddr + v;
    fCallback.CodeLine( Stabs^[index].n_desc, v);
  end;
  inc(index);
end;

procedure TStabsReader.HandleAsmSym(var index: Integer);
begin
  if Assigned(fCallback) then
    fCallBack.AsmSymbol(StabStr( Stabs^[index].n_strx ), Stabs^[index].n_value );
  inc(index);
end;

procedure TStabsReader.HandleVariable(var index: Integer);
var
  global  :  Boolean;
  varname, vartype: String;
begin
  if not Assigned(fCallback) then begin
    inc(index);
    exit;
  end;

  global := Stabs^[index].n_type = N_LCSYM;
  StabVarStr(StabStr( Stabs^[index].n_strx), varname, vartype );

  //writeln('new var = ', varname, ' global = ', global);
  
  if global then
    fCallback.DeclareGlobalVar(varname, Stabs^[index].n_value )
  else
    fCallback.DeclareLocalVar(varname, vl_Register, Stabs^[index].n_value );
  inc(index);
end;

constructor TStabsReader.Create; 
begin
  fProcStack := TStringList.Create;
end;

destructor TStabsReader.Destroy;  
begin
  fProcStack.Free;
  inherited Destroy;  
end;

procedure HandlePSym(var index: Integer);
begin
  inc(index);
end;
    

procedure TStabsReader.ReadStabs(const StabsBuf: array of Byte; StabsLen: Integer;  
  const StabStrBuf: array of byte; StabStrLen: Integer; Callback: TStabsCallback); 
begin
  Stabs := @StabsBuf;
  StabsCnt := StabsLen div sizeof(TStabSym);
  StrSyms := @StabStrBuf[0];
  StrLen := StabStrLen;
  fCallback := Callback;
  DoReadStabs;
end;


end.

