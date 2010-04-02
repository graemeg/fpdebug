unit dbgInfoStabs;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, dbgTypes, dbgInfoTypes,
  stabs, stabsProc;

var
  DebugDumpStabs  : Boolean = False;
  DebugParseStabs : Boolean = True;

type
  { TDbgStabsInfo }
  TDbgStabsInfo = class(TDbgInfoReader)
  private
    fSource         : TDbgDataSource;
    fInfo           : TDbgInfo;
    StabsRead       : Boolean;
    fReadDebugMode  : Boolean;
  protected
    procedure DoReadSymbols;
    function AllocCallback: TStabsCallback;
  public
    constructor Create(ASource: TDbgDataSource); override;

    class function isPresent(ASource: TDbgDataSource): Boolean; override;
    function ReadDebugInfo(ASource: TDbgDataSource; AInfo: TDbgInfo): Boolean; override;
    procedure dump_symbols;
  end;
  
function StabsTypeToStr(_type: Byte): string;

implementation

type
  
  { TStabsToDebugInfo }

  TStabsToDebugInfo = class(TStabsCallback)
  private 
    fFileSym      : TDbgSymFile;
    DebugInfo     : TDbgInfo;
    fCurrentFunc  : TDbgSymFunc;
  public
    constructor Create(ADebugInfo: TDbgInfo);

    function StabMemPosToRegName(MemPos: Integer): AnsiString; // todo: StabMemPosToRegName

    procedure DeclareType(TypeDescr: TStabTypeDescr); override;
    procedure StartFile(const FileName: AnsiString; FirstAddr: LongWord); override;
    procedure AddVar(const Name: AnsiString; OfType: TStabTypeDescr;
      Visiblity: TStabVarVisibility; MemLocation: TStabVarLocation;
      MemPos: Integer); override;

    procedure CodeLine(LineNum, Addr: LongWord); override;
    
    procedure StartProc(const Name: AnsiString; LineNum: Integer;
      EntryAddr: LongWord; isGlobal: Boolean; const NestedTo: String;
      ReturnType: TStabTypeDescr); override;
    procedure EndProc(const Name: AnsiString); override;
    
    procedure AsmSymbol(const SymName: AnsiString; Addr: LongWord); override;
  end;

  { TStabsCallbackLogging }

  TStabsCallbackLogging = class(TStabsCallback)
  public
    procedure DeclareType(AType: TStabTypeDescr); override;
    procedure StartFile(const FileName: AnsiString; FirstAddr: LongWord); override;

    procedure CodeLine(LineNum, Addr: LongWord); override;

    procedure StartProc(const Name: AnsiString; LineNum: Integer;
      EntryAddr: LongWord; isGlobal: Boolean; const NestedTo: String;
      RetType: TStabTypeDescr); override;

    procedure AddVar(const Name: AnsiString; OfType: TStabTypeDescr;
      Visiblity: TStabVarVisibility; MemLocation: TStabVarLocation;
      MemPos: Integer); override;

    procedure EndProc(const Name: AnsiString); override;

    procedure AsmSymbol(const SymName: AnsiString; Addr: LongWord); override;
  end;

{ TStabsCallbackLogging }

procedure TStabsCallbackLogging.DeclareType(AType: TStabTypeDescr);
begin
  writeln('Type: ', AType.Name, ' : ', AType.BaseType);
end;

procedure TStabsCallbackLogging.StartFile(const FileName:AnsiString;FirstAddr:
  LongWord);
begin
  writeln('File: ', FileName);
end;

procedure AddVar(const Name: AnsiString; OfType: TStabTypeDescr;
  Visiblity: TStabVarVisibility; MemLocation: TStabVarLocation;
  MemPos: Integer);
begin
  writeln('var: ', Name,': ', OfType.Name, ' ', Visiblity, ' ',MemLocation, ' ', MemPos);
end;

procedure TStabsCallbackLogging.CodeLine(LineNum,Addr:LongWord);
begin
  writeln('#', LineNum, '; Addr: $',HexStr(Addr,8));
end;

procedure TStabsCallbackLogging.StartProc(const Name:AnsiString; LineNum:Integer;
  EntryAddr: LongWord; isGlobal: Boolean;
  const NestedTo: String; RetType: TStabTypeDescr);
begin
  Write('Proc: ', Name, ' : ', RetType.BaseType);
  if NestedTo<>'' then Write(' nested to: ', NestedTo);
  Writeln;
end;

procedure TStabsCallbackLogging.AddVar(const Name: AnsiString;
  OfType: TStabTypeDescr; Visiblity: TStabVarVisibility;
  MemLocation: TStabVarLocation; MemPos: Integer);
begin
  writeln('Var: ', Name, ' ', OFType.Name,' ', Visiblity, ' ',MemLocation, ' ',MemPos);
end;

procedure TStabsCallbackLogging.EndProc(const Name:AnsiString);
begin
  //WritelN('End proc: ', Name);
end;

procedure TStabsCallbackLogging.AsmSymbol(const SymName:AnsiString;Addr:LongWord);
begin
  WritelN('Asm sym: ', SymName, ' ', HexStr(Addr, sizeof(Addr)*2));
end;

{ TStabsToDebugInfo }

constructor TStabsToDebugInfo.Create(ADebugInfo: TDbgInfo);
begin
  inherited Create;
  DebugInfo := ADebugInfo;
end;

function TStabsToDebugInfo.StabMemPosToRegName(MemPos:Integer):AnsiString;
begin
  Result:=''; //todo!
end;

function isSimpleType(AStabType: TStabType): Boolean;
const
  SimpleTypes  = [
    stInt8,  stInt16,  stInt32,  stInt64,
    stUInt8, stUInt16, stUInt32, stUInt64,
    stBool8, stBool16, stBool32,
    stChar, stWideChar,
    stSingle, stDouble, stExtended, stShortReal, stReal ];
begin
  Result:=AStabType in SimpleTypes;
end;

function StabToDbgSimple(AStabType: TStabType): TDbgSimpleType;
begin
  case AStabType of
    stInt8      : Result:=dstSInt8;
    stInt16     : Result:=dstSInt16;
    stInt32     : Result:=dstSInt32;
    stInt64     : Result:=dstSInt64;
    stUInt8     : Result:=dstUInt8;
    stUInt16    : Result:=dstUInt16;
    stUInt32    : Result:=dstUInt32;
    stUInt64    : Result:=dstUInt64;
    stBool8     : Result:=dstBool8;
    stBool16    : Result:=dstBool16;
    stBool32    : Result:=dstBool32;
    stChar      : Result:=dstChar8;
    stWideChar  : Result:=dstChar16;
    stSingle    : Result:=dstFloat32;
    stDouble    : Result:=dstFloat64;
    stExtended  : Result:=dstFloat64;
    stShortReal : Result:=dstFloat48; //todo: find the size of short real
    stReal      : Result:=dstFloat48;
  else
    Result:=dstUInt32; // sizeof(PtrUInt)
  end;
end;

procedure TStabsToDebugInfo.DeclareType(TypeDescr: TStabTypeDescr);
var
  parent  : TDbgSymbol;
  simple  : TDbgSymSimpleType;
begin
  if isSimpleType(TypeDescr.BaseType) then begin
    {if TypeDescr.DeclLine=0 then parent:=RootSymbol
    else //todo: find the proper parent file }
    if TypeDescr.Name='' then Exit;//todo: remove type name check!
    parent:=RootSymbol;
    simple:=DebugInfo.AddSymbol(TypeDescr.Name, parent, TDbgSymSimpleType)as TDbgSymSimpleType;
    simple.Simple:=StabToDbgSimple(TypeDescr.BaseType);

  end else begin

  end;
end;

procedure TStabsToDebugInfo.StartFile(const FileName: AnsiString; FirstAddr: LongWord);
begin
  fFileSym := DebugInfo.AddFile(FileName);
end;

function StabToDbgLocation(sl: TStabVarLocation): TDbgDataLocation;
begin
  if sl=svlStack then
    Result:=ddlFrameRel
  else
    Result:=ddlRegister
end;

procedure TStabsToDebugInfo.AddVar(const Name: AnsiString;
  OfType: TStabTypeDescr; Visiblity: TStabVarVisibility;
  MemLocation: TStabVarLocation; MemPos: Integer);
var
  parent  : TDbgSymbol;
  vartype : TDbgSymType;
  varinfo : TDbgSymVar;
begin
  case Visiblity of
    svvGlobal:
      parent:=fFileSym;
    svvParam, svvParamByRef, svvLocalFromParam:
      parent:=fCurrentFunc;
    svvLocal:
      if Assigned(fCurrentFunc) then parent:=fCurrentFunc
      else parent:=fFileSym;
  else
    parent:=nil;
  end;

  //todo:
  if Assigned(OfType) then
    vartype:=DebugInfo.FindSymbol(OfType.Name, RootSymbol, TDbgSymType) as TDbgSymType
  else
    vartype:=nil; //variable of no type? error is stabs (or stabs parsing)?

  varinfo:=DebugInfo.AddSymbol(Name, parent, TDbgSymVar) as TDbgSymVar;
  varinfo.vartype:=vartype;
  varinfo.DataPos.Location:=StabToDbgLocation(MemLocation);
  varinfo.DataPos.Addr:=MemPos;
  if varinfo.DataPos.Location=ddlRegister then
    varinfo.DataPos.RegName:=StabMemPosToRegName(MemPos);
end;

procedure TStabsToDebugInfo.CodeLine(LineNum, Addr: LongWord);
begin
  if Assigned(fFileSym) then
    fFileSym.AddLineInfo(Addr, LineNum);
end;

procedure TStabsToDebugInfo.StartProc(const Name: AnsiString; LineNum: Integer;
  EntryAddr: LongWord; isGlobal: Boolean; const NestedTo: String;
  ReturnType: TStabTypeDescr);
var
  proc  : TDbgSymFunc;
begin
  proc := DebugInfo.AddSymbolFunc(Name, fFileSym);
  if not Assigned(proc) then Exit;
  proc.EntryPoint := EntryAddr;
  fCurrentFunc:=proc;
end;

procedure TStabsToDebugInfo.EndProc(const Name: AnsiString);
begin
  fCurrentFunc:=nil;
end;

procedure TStabsToDebugInfo.AsmSymbol(const SymName: AnsiString; Addr: LongWord);
begin
end;

{ TDbgStabsInfo }

procedure TDbgStabsInfo.DoReadSymbols;
var
  sz        : Int64;
  buf       : array of byte;
  strsz     : Int64;
  strbuf    : array of byte;

  i         : Integer;
  SymCount  : Integer;
  Symbols   : PStabSymArray;
  s         : AnsiString;
  
  callback  : TStabsCallback;
  
  function SymStr(strx: integer): AnsiString;
  begin
    if strx = 0 then Result := ''
    else Result := PChar(@strbuf[strx]);
  end;
  
begin
  if not fSource.GetSectionInfo(_stab, sz) then begin
    //writelN('no stab symbols!');
    Exit;
  end;
  SetLength(buf, sz);
  fSource.GetSectionData(_stab, 0, sz, PByteArray(@buf[0])^);
  
  if fSource.GetSectionInfo(_stabstr, strsz) then begin
    SetLength(strbuf, strsz);
    fSource.GetSectionData(_stabstr, 0, strsz, strbuf);
  end else begin
    strsz := 0;
    strbuf := nil
  end;

  if fReadDebugMode then begin
    SymCount := sz div sizeof(TStabSym);

    if DebugDumpStabs then begin
      Symbols := PStabSymArray(@buf[0]);
      for i := 0 to SymCount - 1 do
      with Symbols^[i] do begin
        s := SymStr(n_strx);
        writeln(
          StabsTypeToStr(n_type):8,': ',
          'other = ', n_other,'; ',
          'desc = ',  n_desc, '; ',
          'value = ', Integer(n_value),'; ', HexStr(n_value, 8), '; ',
          s);
      end;
    end;
    writeln('Total symbols = ', SymCount);
  end;

  callback:=AllocCallback;
  if Assigned(callback) then begin
    stabsProc.ReadStabs(buf, sz, strbuf, strsz, callback);
    callback.Free;
  end;
end;

function TDbgStabsInfo.AllocCallback:TStabsCallback;
begin
  if fReadDebugMode then Result:=TStabsCallbackLogging.Create
  else Result:=TStabsToDebugInfo.Create(fInfo);
end;

class function TDbgStabsInfo.isPresent(ASource: TDbgDataSource): Boolean;
var
  sz : Int64;
begin
  Result := ASource.GetSectionInfo(_stab, sz);
end;


function TDbgStabsInfo.ReadDebugInfo(ASource: TDbgDataSource; AInfo: TDbgInfo): Boolean;
begin
  try
    fInfo := AInfo;
    DoReadSymbols;
    Result := true;
  except
    Result := false;
  end;
end;

constructor TDbgStabsInfo.Create(ASource: TDbgDataSource);
begin
  inherited Create(ASource);
  fSource := ASource;
end;

procedure TDbgStabsInfo.dump_symbols;
begin
  fReadDebugMode:=True;
  DoReadSymbols;
end;

function StabsTypeToStr(_type: Byte): string;
begin
  case _type of
    N_GSYM:   Result := 'N_GSYM';
    N_FNAME:  Result := 'N_FNAME';
    N_FUN:    Result := 'N_FUN';
    N_STSYM:  Result := 'N_STSYM';
    N_LCSYM:  Result := 'N_LCSYM';
    N_MAIN:   Result := 'N_MAIN';
    N_ROSYM:  Result := 'N_ROSYM';
    N_PC:     Result := 'N_PC';
    N_NSYMS:  Result := 'N_NSYMS';
    N_NOMAP:  Result := 'N_NOMAP';
    N_OBJ:    Result := 'N_OBJ';
    N_OPT:    Result := 'N_OPT';
    N_RSYM:   Result := 'N_RSYM';
    N_M2C:    Result := 'N_M2C';
    N_SLINE:  Result := 'N_SLINE';
    N_DSLINE: Result := 'N_DSLINE';
    N_BSLINE: Result := 'N_BSLINE';
    //N_BROWS: Result := 'BROWS';
    N_DEFD:   Result := 'N_DEFD';
    N_FLINE:  Result := 'N_FLINE';
    N_EHDECL: Result := 'N_EHDECL';
    //N_MOD2: Result := 'MOD2';
    N_CATCH:  Result := 'N_CATCH';
    N_SSYM:   Result := 'N_SSYM';
    N_ENDM:   Result := 'N_ENDM';
    N_SO:     Result := 'N_SO';
    N_LSYM:   Result := 'N_LSYM';
    N_BINCL:  Result := 'N_BINCL';
    N_SOL:    Result := 'N_SOL';
    N_PSYM:   Result := 'N_PSYM';
    N_EINCL:  Result := 'N_EINCL';
    N_ENTRY:  Result := 'N_ENTRY';
    N_LBRAC:  Result := 'N_LBRAC';
    N_EXCL:   Result := 'N_EXCL';
    N_SCOPE:  Result := 'N_SCOPE';
    N_RBRAC:  Result := 'N_RBRAC';
    N_BCOMM:  Result := 'N_BCOMM';
    N_ECOMM:  Result := 'N_ECOMM';
    N_ECOML:  Result := 'N_ECOML';
    N_WITH:   Result := 'N_WITH';
    N_NBTEXT: Result := 'N_NBTEXT';
    N_NBDATA: Result := 'N_NBDATA';
    N_NBBSS:  Result := 'N_NBBSS';
    N_NBSTS:  Result := 'N_NBSTS';
    N_NBLCS:  Result := 'N_NBLCS';
  else
    Result := IntToHex(_type, 2);
  end;
end;

initialization
  RegisterDebugInfo(TDbgStabsInfo);

end.

