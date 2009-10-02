unit dbgInfoStabs;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, dbgTypes, dbgInfoTypes, stabs, contnrs;

type
  TStabSymbolType = (sst_Func, sst_Variable);

  TFileInfo = class(TObject)
    filename  : AnsiString;
    firstAddr : LongWord;
  end;

  TStabSymbol = class(TObject)
    name    : AnsiString;
    addr    : LongWord;
    symtype : TStabSymbolType;
    parent  : TStabSymbol;

    srcfile : TFileInfo;
    line    : LongWord;
    column  : LongWord;
  end;

  TFuncStabSymbol = class(TStabSymbol)
    callconv    : TCallConv;
  end;

  { TDbgStabsInfo }

  TDbgStabsInfo = class(TDbgInfo)
  private
    fSource     : TDbgDataSource;
    
    fStream     : TStream;
    fOwnSource  : Boolean;

    Symbols     : array of TStabSym;
    SymCount    : Integer;
    Strs        : array of byte;

    SymTable    : TFPObjectHashTable;
    SymFiles    : TFPObjectList;
  protected
    procedure ReadSymbols;
    procedure FillSymTable;
    function AllocStabSymbol(const sym: TStabSym): TStabSymbol;
    function SymStr(strx: integer): AnsiString;
  public
    constructor Create(ASource: TDbgDataSource); override;
    destructor Destroy; override;

    class function isPresent(ASource: TDbgDataSource): Boolean; override;
    function GetDebugData(const DataName: AnsiString; DataAddr: TDbgPtr; OutData: TDbgDataList): Boolean; override;
    function GetAddrByName(const SymbolName: AnsiString; var Addr: TDbgPtr): Boolean; override;
    procedure GetNames(Names: TStrings); override;
    procedure dump_symbols;
  end;
  
function StabsTypeToStr(_type: Byte): string;

implementation

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

{ TDbgStabsInfo }

procedure TDbgStabsInfo.ReadSymbols;
var
  sz    : Int64;
begin
  if not fSource.GetSectionInfo(_stab, sz) then begin
    writelN('no stab symbols!');
    Exit;
  end;
  SymCount := sz div sizeof(TStabSym);
  SetLength(Symbols, SymCount);

  fSource.GetSectionData(_stab, 0, sz, PByteArray(@symbols[0])^);

  if fSource.GetSectionInfo(_stabstr, sz) then begin
    SetLength(strs, sz);
    fSource.GetSectionData(_stabstr, 0, sz, strs);
  end;
end;

procedure TDbgStabsInfo.FillSymTable;
var
  i   : integer;
  sym : TStabSymbol;
  sh  : ShortString;
  nm  : AnsiString;
  currentFile : TFileInfo;
begin
  currentFile := nil;

  for i := 0 to SymCount - 1 do begin
    case Symbols[i].n_value of
      N_SO: begin
        nm := SymStr(Symbols[i].n_strx);
        if nm = '' then
          currentFile := nil
        else begin
          if Assigned(currentFile) and (currentFile.firstAddr = Symbols[i].n_value) then
            currentFile.filename := currentFile.filename + nm
          else begin
            currentFile := TFileInfo.Create;
            currentFile.firstAddr := Symbols[i].n_value;
            currentFile.filename := nm;
            SymFiles.Add(currentFile);
          end;
        end;
      end;
    else
      sym := AllocStabSymbol( Symbols[i] );
      sh := sym.name;
      sym.srcfile := currentFile;

      if not Assigned(SymTable.Items[sh]) then SymTable.Add(sh, sym);
    end;
  end;
end;

function TDbgStabsInfo.AllocStabSymbol(const sym: TStabSym): TStabSymbol;
var
  s   : TStabSymbol;
  nm  : AnsiString;
begin
  nm := SymStr(sym.n_strx);

  case sym.n_type of
    N_FUN:
      if nm <> '' then begin
        s := TStabSymbol.Create;
        s.name := nm;
        s.symtype := sst_Func;
        s.addr := sym.n_value;
      end;
  else
    Result := nil;
  end;
end;

function TDbgStabsInfo.SymStr(strx: integer): AnsiString;
begin
  if strx = 0 then Result := ''
  else Result := PChar(@strs[strx]);
end;

class function TDbgStabsInfo.isPresent(ASource: TDbgDataSource): Boolean;
var
  sz : Int64;
begin
  Result := ASource.GetSectionInfo(_stab, sz);
end;

constructor TDbgStabsInfo.Create(ASource: TDbgDataSource);
begin
  inherited Create(ASource);
  fSource := ASource;
  SymTable := TFPObjectHashTable.Create(true);
  SymFiles := TFPObjectList.Create(true);
end;

destructor TDbgStabsInfo.Destroy;
begin
  SymFiles.Free;
  SymTable.Free;
  inherited Destroy;
end;

function TDbgStabsInfo.GetDebugData(const DataName: AnsiString; DataAddr: TDbgPtr; OutData: TDbgDataList): Boolean;
begin
  Result := false;
end;

function TDbgStabsInfo.GetAddrByName(const SymbolName: AnsiString; var Addr: TDbgPtr): Boolean;
var
  i   : integer;
  sh  : TStabSymbol;
begin
  sh := TStabSymbol(SymTable.Items[SymbolName]);
  Result := Assigned(sh);
  if Result then Addr := sh.addr;
end;

procedure TDbgStabsInfo.GetNames(Names: TStrings);
begin
 //todo:
end;

procedure TDbgStabsInfo.dump_symbols;
var
  i  : Integer;
  s  : AnsiString;
begin
  if SymCount = 0 then ReadSymbols;

  for i := 0 to SymCount - 1 do
    with Symbols[i] do begin
      s := SymStr(n_strx);
      writeln(Format('%s; other = %d; desc = %d; value = %d, %s; str = %s', [StabsTypeToStr(n_type),
        n_other, n_desc, n_value,  IntToHex(n_value, 8), s]));
    end;
  writeln('Total symbols = ', SymCount);
end;

initialization
  RegisterDebugInfo(TDbgStabsInfo);

end.

