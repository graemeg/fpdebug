unit dbgInfoTypes; 

{$mode objfpc}{$H+}

interface

uses
  contnrs, SysUtils, Classes, dbgTypes, AVL_Tree;

type
  { TDbgDataSource }

  TDbgDataSource = class(TObject) // executable parser
  public
    class function isValid(ASource: TStream): Boolean; virtual; abstract;
    class function UserName: AnsiString; virtual; abstract;
    constructor Create(ASource: TStream; OwnSource: Boolean); virtual; 

    function GetSectionInfo(const SectionName: AnsiString; var Size: int64): Boolean; virtual; abstract;
    function GetSectionData(const SectionName: AnsiString; Offset, Size: Int64; var Buf: array of byte): Int64; virtual; abstract;
  end;
  TDbgDataSourceClass = class of TDbgDataSource;

  TDbgInfo = class;

  { TDbgInfoReader }

  TDbgInfoReader = class(TObject)
  public
    class function isPresent(ASource: TDbgDataSource): Boolean; virtual; abstract;

    constructor Create(ASource: TDbgDataSource); virtual; 

    function ReadDebugInfo(ASource: TDbgDataSource; Info: TDbgInfo): Boolean; virtual; abstract;
  end;
  TDbgInfoReaderClass = class of TDbgInfoReader;

  { TDbgFileSymbol }

  { TDbgSymbol }

  TDbgSymbol = class(TObject)
  private
    fName   : AnsiString;
    fParent : TDbgSymbol;
    subList : TFPObjectList;
    subHash : TFPObjectHashTable;
  protected
    function GetCount: Integer;
    function GetSymbol(index: Integer): TDbgSymbol;
  public
    constructor Create(const AName: AnsiString; AParentSym: TDbgSymbol); virtual;
    destructor Destroy; override;
    property Name: AnsiString read fName;
    property Parent: TDbgSymbol read fParent;
    property Count: integer read GetCount;
    property Child[i: integer]: TDbgSymbol read GetSymbol; default;
  end;
  TDbgSymbolClass = class of TDbgSymbol;

  { TDbgFileInfo }

  TDbgLineInfo = class(TObject)
    LineNum : Integer;
    Addr    : TDbgPtr;
  end;

  TDbgFileInfo = class(TDbgSymbol)
  private
    AddrToLines : TAVLTree;
    LinesToAddr : TAVLTree;
  public
    FileName  : WideString;
    constructor Create(const AName: AnsiString; AParentSym: TDbgSymbol); override;
    destructor Destroy; override;
    procedure AddLineInfo(const Addr: TDbgPtr; const LineNum: Integer);
    function FindLineByAddr(const Addr: TDbgPtr; var LineNum: Integer): Boolean;
    function FindAddrByLine(const LineNum: Integer; var Addr: TDbgPtr): Boolean;
  end;

  TDbgSymbolFunc = class(TDbgSymbol)
  public
    EntryPoint : TDbgPtr;
  end;

  TVariableType = (vt_Local, vt_Global, vt_Param, vt_ParamByRef);

  TDbgVariable = class(TDbgSymbol)
  public
    vartype : TVariableType;
  end;

  { TDbgInfo }

  TDbgInfo = class(TObject)
  private
    fGlobalList : TFPObjectList;
    fGlobalHash : TFPObjectHashTable;

  protected
    function GetFileSymbolName(const FileName: WideString): AnsiString;
    function FindInGlobal(const SymName: AnsiString): TDbgSymbol;
    function AddGlobalSymbol(const SymName: AnsiString; symClass: TDbgSymbolClass): TDbgSymbol;
  public
    Reader  : TDbgInfoReader;
    constructor Create;
    destructor Destroy; override;

    function AddFile(const FileName: WideString): TDbgFileInfo; virtual;
    function FindFile(const FileName: WideString): TDbgFileInfo; virtual;

    function AddSymbol(const SymbolName: AnsiString; ParentSymbol: TDbgSymbol;
      SymbolClass: TDbgSymbolClass): TDbgSymbol; virtual; overload;
    function AddFileSymbol(const SymbolName: AnsiString; const FileName: WideString;
      const SymbolClass: TDbgSymbolClass): TDbgSymbol;

    function FindSymbol(const SymbolName: AnsiString; Parent: TDbgSymbol; SearchInFiles: Boolean = true): TDbgSymbol;
    function FindInFile(const SymbolName: AnsiString; const FileName: WideString): TDbgSymbol;
  end;

function GetDataSource(const FileName: string): TDbgDataSource; overload;
function GetDataSource(ASource: TStream; OwnSource: Boolean): TDbgDataSource; overload;
procedure RegisterDataSource(DataSource: TDbgDataSourceClass); 

procedure GetDebugInfos(Source: TDbgDataSource; List: TFPList);
procedure RegisterDebugInfo(DebugInfo: TDbgInfoReaderClass);

implementation

var
  srcclasses  : TFPList;
  infoclasses : TFPList;
  
procedure GetDebugInfos(Source: TDbgDataSource; List: TFPList);
var
  i         : Integer;
  readerclass : TDbgInfoReaderClass;
  reader    : TDbgInfoReader;
  info      : TDbgInfo;
begin
  for i := 0 to infoclasses.Count - 1 do begin
    readerclass := TDbgInfoReaderClass(infoclasses[i]);
    if readerclass.isPresent(Source) then begin
      reader := readerclass.Create(Source);
      info := TDbgInfo.Create;
      info.Reader := reader;
      reader.ReadDebugInfo(Source, info);
      List.Add(info);
    end;
  end;
end;
  
function GetDataSource(const FileName: string): TDbgDataSource;
var
  fs  : TFileStream;
begin
  try
    fs := TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
    Result := GetDataSource(fs, true);
  except
    Result := nil;
  end;
end;  

function GetDataSource(ASource: TStream; OwnSource: Boolean): TDbgDataSource;
var
  i   : Integer;
  cls : TDbgDataSourceClass;
  p : Int64;
begin
  Result := nil;
  if not Assigned(ASource) then Exit;
  
  p := ASource.Position;
  for i := 0 to srcclasses.Count - 1 do begin
    cls :=  TDbgDataSourceClass(srcclasses[i]);
    try
      ASource.Position := P;
      if cls.isValid(ASource) then begin 
        ASource.Position := p;
        Result := cls.Create(ASource, OwnSource);
        Exit;
      end else
        ;
        
    except
      on e: exception do begin
        //writeln('exception! WHY? ', e.Message);
      end;
    end;
  end;
  Result := nil;
end;

procedure RegisterDataSource( DataSource: TDbgDataSourceClass); 
begin
  if Assigned(DataSource) and (srcclasses.IndexOf(DataSource) < 0) then  
    srcclasses.Add(DataSource)
end;

procedure RegisterDebugInfo(DebugInfo: TDbgInfoReaderClass);
begin
  infoclasses.Add(DebugInfo);
end;

procedure InitDebugInfoLists;
begin
  srcclasses := TFPList.Create;
  infoclasses := TFPList.Create;
end;

procedure ReleaseDebugInfoLists;
begin 
  srcclasses.Free;
  infoclasses.Free;
end;

{ TDbgDataSource }

constructor TDbgDataSource.Create(ASource: TStream; OwnSource: Boolean); 
begin
  inherited Create;
end;

{ TDbgInfoReader }

constructor TDbgInfoReader.Create(ASource: TDbgDataSource);
begin
  inherited Create;
end;

{ TDbgInfo }

function TDbgInfo.GetFileSymbolName(const FileName: WideString): AnsiString;
begin
  Result := '#file:'+UTF8Encode(FileName);
end;

function TDbgInfo.FindInGlobal(const SymName: AnsiString): TDbgSymbol;
begin
  Result := TDbgSymbol(fGlobalHash.Items[SymName]);
end;


constructor TDbgInfo.Create;
begin
  inherited Create;
  fGlobalList := TFPObjectList.Create(true);
  fGlobalHash := TFPObjectHashTable.Create(false);
end;

destructor TDbgInfo.Destroy;
begin
  fGlobalList.Free;
  fGlobalHash.Free;
  inherited Destroy;
end;

function TDbgInfo.AddFile(const FileName: WideString): TDbgFileInfo;
begin
  Result := FindFile(FileName);
  if Assigned(Result) then Exit;

  Result := AddGlobalSymbol( GetFileSymbolName(FileName), TDbgFileInfo ) as TDbgFileInfo;
  Result.FileName := FileName;
end;

function TDbgInfo.FindFile(const FileName: WideString): TDbgFileInfo;
var
  res     : TDbgSymbol;
begin
  res := FindInGlobal(FileName);
  if Assigned(res) and (res is TDbgFileInfo) then
    Result := TDbgFileInfo(res)
  else
    Result := nil;
end;

function TDbgInfo.AddSymbol(const SymbolName: AnsiString;
  ParentSymbol: TDbgSymbol; SymbolClass: TDbgSymbolClass): TDbgSymbol;
begin
  if Assigned(PArentSymbol) then writeln(' ', ParentSymbol.ClassName) else writeln;

  if SymbolClass.InheritsFrom(TDbgFileInfo) then begin
    Result := nil;
    Exit;
  end;

  if Assigned(ParentSymbol) then
    Result := SymbolClass.Create(SymbolName, ParentSymbol)
  else
    Result := AddGlobalSymbol(SymbolName, SymbolClass)
end;

function TDbgInfo.AddGlobalSymbol(const SymName: AnsiString;
  symClass: TDbgSymbolClass): TDbgSymbol;
begin
  Result := symClass.Create(SymName, nil);
  fGlobalHash.Add(SymName, Result);
  fGlobalList.Add(Result);
end;

function TDbgInfo.AddFileSymbol(const SymbolName: AnsiString; const FileName: WideString;
  const SymbolClass: TDbgSymbolClass): TDbgSymbol;
begin
  Result := AddSymbol(SymbolName, AddFile(FileName), SymbolClass);
end;

function TDbgInfo.FindSymbol(const SymbolName: AnsiString; Parent: TDbgSymbol; SearchInFiles: Boolean): TDbgSymbol;
var
  i   : integer;
  sym : TDbgSymbol;
begin
  if not Assigned(Parent) then begin
    Result := FindInGlobal(SymbolName);
    if Assigned(Result) or not SearchInFiles then Exit;

    for i := 0 to fGlobalList.Count - 1 do begin
      sym := TDbgSymbol(fGlobalList[i]);
      if sym is TDbgFileInfo then begin
        Result := FindSymbol(SymbolName, sym, false);
        if Assigned(Result) then Exit;
      end;
    end;

  end else
    Result := TDbgSymbol(Parent.subHash.Items[SymbolName]);
end;

function TDbgInfo.FindInFile(const SymbolName: AnsiString; const FileName: WideString): TDbgSymbol;
var
  parent  : TDbgSymbol;
begin
  parent := FindInGlobal(GetFileSymbolName(FileName));
  if not Assigned(parent) then
    Result := nil
  else
    Result := FindSymbol(SymbolName, parent);
end;

{ TDbgSymbol }

function TDbgSymbol.GetCount: Integer;
begin
  Result := subList.Count;
end;

function TDbgSymbol.GetSymbol(index: Integer): TDbgSymbol;
begin
  Result := TDbgSymbol(subList[index]);
end;

constructor TDbgSymbol.Create(const AName: AnsiString; AParentSym: TDbgSymbol);
begin
  inherited Create;
  fName := AName;
  fParent := AParentSym;
  subList := TFPObjectList.Create(true);
  subHash := TFPObjectHashTable.Create(false);

  if Assigned(fParent) then begin
    fParent.subHash.Items[AName]:=Self;
    fPArent.subList.Add(Self);
  end;
end;

destructor TDbgSymbol.Destroy;
begin
  subHash.Free;
  subList.Free;
  inherited Destroy;
end;

{ TDbgFileInfo }

function OnLinesCompareAddr(Item1, Item2: Pointer): Integer;
begin
  if TDbgLineInfo(Item1).Addr < TDbgLineInfo(Item2).Addr then Result := -1
  else if TDbgLineInfo(Item1).Addr = TDbgLineInfo(Item2).Addr then Result := 0
  else Result := 1;
end;

function OnLinesCompareLine(Item1, Item2: Pointer): Integer;
begin
  if TDbgLineInfo(Item1).LineNum < TDbgLineInfo(Item2).LineNum then Result := -1
  else if TDbgLineInfo(Item1).LineNum = TDbgLineInfo(Item2).LineNum then Result := 0
  else Result := 1;
end;

constructor TDbgFileInfo.Create(const AName: AnsiString; AParentSym: TDbgSymbol);
begin
  AddrToLines := TAVLTree.Create(@OnLinesCompareAddr);
  LinesToAddr := TAVLTree.Create(@OnLinesCompareLine);
  inherited Create(AName, AParentSym);
end;

destructor TDbgFileInfo.Destroy;
begin
  AddrToLines.Free;
  LinesToAddr.Free;
  inherited Destroy;
end;

procedure TDbgFileInfo.AddLineInfo(const Addr: TDbgPtr; const LineNum: Integer);
var
  info  : TDbgLineInfo;
begin
  info := TDbgLineInfo.Create;
  info.Addr:=Addr;
  info.LineNum:=LineNum;
  AddrToLines.Add(info);
  LinesToAddr.Add(info);
end;

function CompareWithAddr(Item1, Item2: Pointer): Integer;
begin
  if PDbgPtr(Item1)^ < TDbgLineInfo(Item2).Addr then Result := -1
  else if PDbgPtr(Item1)^ = TDbgLineInfo(Item2).Addr then Result := 0
  else Result := 1;
end;

function TDbgFileInfo.FindLineByAddr(const Addr: TDbgPtr; var LineNum: Integer): Boolean;
var
  node  : TAVLTreeNode;
begin
  node := AddrToLines.FindKey(@Addr, @CompareWithAddr);
  Result := Assigned(node) and Assigned(node.Data);
  if Result then Linenum := TDbgLineINfo(node.Data).LineNum;
end;

function CompareWithLine(Item1, Item2: Pointer): Integer;
begin
  if PInteger(Item1)^ < TDbgLineInfo(Item2).LineNum then Result := -1
  else if PInteger(Item1)^ = TDbgLineInfo(Item2).LineNum then Result := 0
  else Result := 1;
end;

function TDbgFileInfo.FindAddrByLine(const LineNum: Integer; var Addr: TDbgPtr): Boolean;
var
  node  : TAVLTreeNode;
begin
  node := AddrToLines.FindKey(@LineNum, @CompareWithLine);
  Result := Assigned(node) and Assigned(node.Data);
  if Result then Addr := TDbgLineINfo(node.Data).Addr;
end;

initialization
  InitDebugInfoLists;

finalization
  ReleaseDebugInfoLists;


end.

