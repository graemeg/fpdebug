unit dbgInfoStabs;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, contnrs, dbgTypes, dbgInfoTypes, stabs, stabsProc, AVL_Tree;

type

  { TDbgStabsInfo }

  TDbgStabsInfo = class(TDbgInfoReader)
  private
    fSource     : TDbgDataSource;
    StabsRead   : Boolean;
    
    SymTable    : TFPObjectHashTable;
    SymList     : TFPObjectList;
    SrcFiles    : TFPObjectHashTable;
    FilesList   : TFPObjectList;

    fInfo       : TDbgInfo;
  protected
    procedure ReadSymbols(debugdump: Boolean = false);
  public
    constructor Create(ASource: TDbgDataSource); override;
    destructor Destroy; override;

    class function isPresent(ASource: TDbgDataSource): Boolean; override;
    function ReadDebugInfo(ASource: TDbgDataSource; AInfo: TDbgInfo): Boolean; override;
    procedure dump_symbols;
  end;
  
function StabsTypeToStr(_type: Byte): string;

implementation

type
  
  { TDbgInfoCallback }

  TDbgInfoCallback = class(TStabsCallback)
  private 
    fOwner        : TDbgStabsInfo;
    fFileSym      : TDbgFileInfo;
    fDebugInfo    : TDbgInfo;
    fCurrentFunc  : TDbgSymbolFunc;
  public
    constructor Create(AOwner: TDbgStabsInfo; ADebugInfo: TDbgInfo);
    
    procedure DeclareType(const TypeName: AnsiString); override;
    procedure StartFile(const FileName: AnsiString; FirstAddr: LongWord); override;
    procedure DeclareLocalVar(const Name: AnsiString; Location: TVarLocation; Addr: LongWord); override;
    procedure DeclareGlobalVar(const Name: AnsiString; Addr: LongWord); override;

    procedure CodeLine(LineNum, Addr: LongWord); override;
    
    procedure StartProc(const Name: AnsiString; const StabParams : array of TStabProcParams; ParamsCount: Integer; LineNum: Integer; Addr: LongWord); override;
    procedure EndProc(const Name: AnsiString); override;
    
    procedure AsmSymbol(const SymName: AnsiString; Addr: LongWord); override;
  end;

{ TDbgInfoCallback }

constructor TDbgInfoCallback.Create(AOwner: TDbgStabsInfo; ADebugInfo: TDbgInfo);
begin
  inherited Create;
  fOwner := AOwner;
  fDebugInfo := ADebugInfo;
end;

procedure TDbgInfoCallback.DeclareType(const TypeName: AnsiString);  
begin
end;

procedure TDbgInfoCallback.StartFile(const FileName: AnsiString; FirstAddr: LongWord);  
begin
  fFileSym := fDebugInfo.AddFile(FileName);
end;

procedure TDbgInfoCallback.DeclareLocalVar(const Name: AnsiString; Location: TVarLocation; Addr: LongWord);
begin
end;

procedure TDbgInfoCallback.DeclareGlobalVar(const Name: AnsiString; Addr: LongWord);
begin

end;

procedure TDbgInfoCallback.CodeLine(LineNum, Addr: LongWord);  
begin
  if Assigned(fFileSym) then
    fFileSym.AddLineInfo(Addr, LineNum);
end;

procedure TDbgInfoCallback.StartProc(const Name: AnsiString;  
  const StabParams: array of TStabProcParams; ParamsCount: Integer;  
  LineNum: Integer; Addr: LongWord);  
var
  proc  : TDbgSymbolFunc;
begin
  proc := TDbgSymbolFunc(fOwner.fInfo.AddSymbol(Name, fFileSym, TDbgSymbolFunc));
  if not Assigned(proc) then Exit;
  proc.EntryPoint := Addr;
  fCurrentFunc:=proc;
end;

procedure TDbgInfoCallback.EndProc(const Name: AnsiString);  
begin
  fCurrentFunc:=nil;
end;

procedure TDbgInfoCallback.AsmSymbol(const SymName: AnsiString; Addr: LongWord);  
begin
end;


{ TDbgStabsInfo }

procedure TDbgStabsInfo.ReadSymbols(debugdump: Boolean);
var
  sz        : Int64;
  buf       : array of byte;
  strsz     : Int64;
  strbuf    : array of byte;

  i         : Integer;
  SymCount  : Integer;
  Symbols   : PStabSymArray;
  s         : AnsiString;
  
  callback  : TDbgInfoCallback;
  
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

  if debugdump then begin
    SymCount := sz div sizeof(TStabSym);
    Symbols := PStabSymArray(@buf[0]);
    for i := 0 to SymCount - 1 do
    with Symbols^[i] do begin
      s := SymStr(n_strx);
      writeln(Format('%s; other = %d; desc = %d; value = %d, %s; str = %s', [StabsTypeToStr(n_type),
        n_other, n_desc, n_value,  IntToHex(n_value, 8), s]));
    end;
    writeln('Total symbols = ', SymCount);
  end else begin
    callback := TDbgInfoCallback.Create(Self, fInfo);
    stabsProc.ReadStabs(buf, sz, strbuf, strsz, callback);
    callback.Free;
  end;
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
    ReadSymbols;
    Result := true;
  except
    Result := false;
  end;
end;

constructor TDbgStabsInfo.Create(ASource: TDbgDataSource);
begin
  inherited Create(ASource);
  fSource := ASource;
  SymTable := TFPObjectHashTable.Create(true);
  SrcFiles := TFPObjectHashTable.Create(true);
  SymList := TFPObjectList.Create(false);  
  FilesList := TFPObjectList.Create(false);
end;

destructor TDbgStabsInfo.Destroy;
begin
  FilesList.Free;
  SrcFiles.Free;
  SymTable.Free;
  SymList.Free;
  inherited Destroy;
end;

procedure TDbgStabsInfo.dump_symbols;
begin
  ReadSymbols(true);
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

