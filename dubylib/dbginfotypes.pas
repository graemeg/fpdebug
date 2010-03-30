unit dbgInfoTypes; 

{$ifdef fpc}{$mode delphi}{$H+}{$endif}

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

  TDbgSymbolVisiblity = (svGlobal, svExport, svLocal);

  { TDbgSymbol }

  TDbgSymbol = class(TObject)
  private
    fName   : AnsiString;
    fParent : TDbgSymbol;
    subList : TFPObjectList;
    subHash : TFPObjectHashTable;
    fVisibility : TDbgSymbolVisiblity;
  protected
    function GetCount: Integer;
    function GetSymbol(index: Integer): TDbgSymbol;
  public
    constructor Create(const AName: AnsiString; AParentSym: TDbgSymbol); virtual;
    destructor Destroy; override;
    property Visibility: TDbgSymbolVisiblity read fVisibility write fVisibility;
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

  { TDbgFileInfo }

  TDbgFileInfo = class(TDbgSymbol)
  private
    AddrToLines : TAVLTree;
    LinesToAddr : TAVLTree;
  public
    FileName  : WideString;
    constructor Create(const AName: AnsiString; AParentSym: TDbgSymbol); override;
    destructor Destroy; override;
    procedure AddLineInfo(const Addr: TDbgPtr; const LineNum: Integer);
    function FindLineByAddr(const Addr: TDbgPtr; var LineNum: Integer; Strict: Boolean = false): Boolean;
    function FindAddrByLine(const LineNum: Integer; var Addr: TDbgPtr): Boolean;
    function GetLinesInfoCount: Integer;
  end;

  { TDbgSymbolFunc }

  TDbgSymbolFunc = class(TDbgSymbol)
  public
    EntryPoint : TDbgPtr;
  end;

  { TDbgSymbolTypeDescr }

  TDbgSymbolTypeDescr = class(TDbgSymbol)
  end;

  TDbgSimpleType = (dstInteger, dstUnsigned, dstSingle,
    dstDouble, dstReal, dstPointer, dstChar, dstWideChar);

  TDbgSymbolSimpleDescr = class(TDbgSymbolTypeDescr)
    SymType : TDbgSimpleType;
  end;

  { TDbgSymbolVar }

  TDbgSymbolVar = class(TDbgSymbol)
  public
    addr    : TDbgPtr;
    vartype : TDbgSymbolTypeDescr;
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
    
    procedure EnumSourceFiles(str: TStrings);
  end;

function GetDataSource(const FileName: string): TDbgDataSource; overload;
function GetDataSource(ASource: TStream; OwnSource: Boolean): TDbgDataSource; overload;
procedure RegisterDataSource(DataSource: TDbgDataSourceClass); 

procedure GetDebugInfos(Source: TDbgDataSource; List: TFPList);
procedure RegisterDebugInfo(DebugInfo: TDbgInfoReaderClass);

function FindSourceFileName(info: TDbgInfo; const ShortName: AnsiString; IgnoreCase: Boolean=True): AnsiString;
function FindLineAddr(info: TDbgInfo; const FullFileName: AnsiString; LineNum: Integer; var addr: TDbgPtr): Boolean;

implementation

var
  SrcClasses  : TFPList;
  InfoClasses : TFPList;
  
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
  res := FindInGlobal( GetFileSymbolName(FileName));
  if Assigned(res) and (res is TDbgFileInfo) then
    Result := TDbgFileInfo(res)
  else
    Result := nil;
end;

function TDbgInfo.AddSymbol(const SymbolName: AnsiString;
  ParentSymbol: TDbgSymbol; SymbolClass: TDbgSymbolClass): TDbgSymbol;
begin
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

procedure TDbgInfo.EnumSourceFiles(str: TStrings);
var
  i : integer;
begin
  for i := 0 to fGlobalList.Count - 1 do
    if TDbgSymbol(fGlobalList[i]) is TDbgFileInfo then 
      str.Add( TDbgFileInfo(fGlobalList[i]).FileName);
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

function TDbgFileInfo.FindLineByAddr(const Addr: TDbgPtr; var LineNum: Integer; Strict: Boolean = false): Boolean;
var
  node  : TAVLTreeNode;
  cnt   : integer;
  i     : Integer;
  a     : TDbgPtr;
begin
  if Strict then cnt := 0 else cnt := 8;
  for i := 0 to 7 do begin
    a := Addr - i;
    node := AddrToLines.FindKey(@a, @CompareWithAddr);
    Result := Assigned(node) and Assigned(node.Data);
    if Result then begin
      Linenum := TDbgLineInfo(node.Data).LineNum;
      Exit;
    end;
  end;
  Result := false;
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

function TDbgFileInfo.GetLinesInfoCount:Integer;
begin
  Result:=AddrToLines.Count;
end;

function isStrTail(const TailStr, Str: AnsiSTring): Boolean;
var
  tail :string;
begin
  tail:=Copy(Str, length(Str)-length(TailStr)+1, length(TailStr));
  Result:=tail=TailStr;
end;

function FixSlashes(const s: AnsiString): String;
var
  i : Integer;
begin
  Result:=s;
  for i:=1 to length(Result) do
    if Result[i] in ['/','\'] then
      Result[i]:=DirectorySeparator;
end;

function FindSourceFileName(info: TDbgInfo; const ShortName: AnsiString; IgnoreCase: Boolean): AnsiString;
var
  i   : Integer;
  st  : TStringList;
  chk : AnsiString;
  fn  : AnsiString;
begin
  Result:='';
  if not Assigned(info) or (ShortName='') then Exit;

  if IgnoreCase then chk:=AnsiLowerCase(ShortName)
  else chk:=ShortName;

  st := TStringList.Create;
  try
    info.EnumSourceFiles(st);
    for i:=0 to st.Count-1 do begin
      fn:=st[i];
      if IgnoreCase then fn:=AnsiLowerCase(fn);
      fn:=FixSlashes(fn);
      if (length(fn)>=length(chk)) and ((ExtractFileName(fn)=chk) or isStrTail(ShortName, fn)) then begin
        Result:=st[i];
        Exit;
      end;
    end;
  finally
    st.Free;
  end;
end;

function FindLineAddr(info: TDbgInfo; const FullFileName: AnsiString; LineNum: Integer; var addr: TDbgPtr): Boolean;
var
  dfile : TDbgFileInfo;
begin
  Result:=False;
  if (FullFileName='') then Exit;

  dfile:=info.FindFile(FullFileName);
  Result:=Assigned(dfile);
  if not Result then Exit;

 Result:= dfile.FindAddrByLine(LineNum, addr);
end;

initialization
  InitDebugInfoLists;

finalization
  ReleaseDebugInfoLists;


end.

