unit dbgInfoTypes; 

{$ifdef fpc}{$mode delphi}{$H+}{$endif}

interface

uses
  contnrs, SysUtils, Classes, dbgTypes, AVL_Tree, dbgInfoAccess;

const
  RootSymbol = nil;

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

  TDbgDataLocation = (ddlAbsolute, ddlFrameRel, ddlRegister);

  //todo: REMOVE! replace by dbgInfoAccess
  TDbgDataPos = record
    Location : TDbgDataLocation;
    Addr     : TDbgPtr;
    RegName  : AnsiString; //register name,
  end;

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
  TDbgSymClass = class of TDbgSymbol;

  { TDbgSymType }

  TDbgSymType = class(TDbgSymbol)
  public
    function GetVarSize: LongWord; virtual; abstract;
    function isRefType: Boolean; virtual; // true for AnsiString, FPC Classes etc.
    function DerefOfs: PtrInt; virtual;
    function isIndexAccess: Boolean; virtual; // true for arrays + array-like types (PChar, Classes with default value)
    function GetItemOffset(AIndex: PtrInt; var Offset: PtrUInt): Boolean; virtual;
  end;
  TDbgSymTypeClass = class of TDbgSymType;


  TDbgSymAlias = class(TDbgSymbol)
  public
    Original : TDbgSymbol;
  end;

  { TDbgFileInfo }

  TDbgLineInfo = class(TObject)
    LineNum : Integer;
    Addr    : TDbgPtr;
  end;

  { TDbgSymSourceFile }

  TDbgSymFile = class(TDbgSymbol)
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

  { TDbgSymFunc }

  TDbgSymFunc = class(TDbgSymbol)
  public
    EntryPoint  : TDbgPtr;
    ReturnType  : TDbgSymType;
  end;

  { TDbgSymbolTypeDescr }

  TDbgSimpleType = (
    dstSInt8,   dstSInt16,  dstSInt32, dstSInt64,
    dstUInt8,   dstUInt16,  dstUInt32, dstUInt64,
    dstFloat32, dstFloat48, dstFloat64,
    dstBool8,   dstBool16,  dstBool32,
    dstChar8,   dstChar16
  );

  { TDbgSymSimpleType }

  TDbgSymSimpleType = class(TDbgSymType)
    Simple  : TDbgSimpleType;
    function GetVarSize: LongWord; override;
  end;

  TDbgSymPointerType = class(TDbgSymType)
    RefType : TDbgSymType;
  end;

  { TDbgSymVar }

  TDbgSymVar = class(TDbgSymbol)
  public
    VarType : TDbgSymType;
    DataPos : TDbgDataPos;
  end;

  { TDbgInfo }

  TDbgInfo = class(TObject)
  private
    fGlobalList : TFPObjectList;
    fGlobalHash : TFPObjectHashTable;

  protected
    function GetFileSymbolName(const FileName: WideString): AnsiString;
    function FindInGlobal(const SymName: AnsiString; SymClass: TDbgSymClass=nil): TDbgSymbol;
    procedure AddSymbolToHash(Symbol: TDbgSymbol);

    function GetSymList(const SymName: AnsiString): TFPList;

  public
    //Reader  : TDbgInfoReader;
    Root    : TDbgSymbol;
    constructor Create;
    destructor Destroy; override;

    function AddFile(const FileName: WideString): TDbgSymFile; virtual;
    function FindFile(const FileName: WideString): TDbgSymFile; virtual;

    function AddSymbol(const SymbolName: AnsiString; ParentSymbol: TDbgSymbol;
      SymbolClass: TDbgSymClass): TDbgSymbol; virtual; overload;
    function AddSymbolVar(const VarName: AnsiString; ParentSymbol: TDbgSymbol): TDbgSymVar;
    function AddSymbolFunc(const FuncName: AnsiString; ParentSymbol: TDbgSymbol): TDbgSymFunc;

    function AddFileSymbol(const SymbolName: AnsiString; const FileName: WideString;
      const SymbolClass: TDbgSymClass): TDbgSymbol;

    function FindSymbol(const SymbolName: AnsiString; Parent: TDbgSymbol; SymbolClass: TDbgSymClass = nil; SearchInFiles: Boolean = true): TDbgSymbol;
    function FindInFile(const SymbolName: AnsiString; const FileName: WideString): TDbgSymbol;
    
    procedure EnumSourceFiles(str: TStrings);
  end;

function GetDataSource(const FileName: string): TDbgDataSource; overload;
function GetDataSource(ASource: TStream; OwnSource: Boolean): TDbgDataSource; overload;
procedure RegisterDataSource(DataSource: TDbgDataSourceClass); 

procedure GetDebugInfos(Source: TDbgDataSource; List: TFPList); deprecated; //todo: remove, use LoadDebugInfoFromFile

procedure RegisterDebugInfo(DebugInfo: TDbgInfoReaderClass);

function FindSourceFileName(info: TDbgInfo; const ShortName: AnsiString; IgnoreCase: Boolean=True): AnsiString;
function FindLineAddr(info: TDbgInfo; const FullFileName: AnsiString; LineNum: Integer; var addr: TDbgPtr): Boolean;

function AddAliasToSymbol(info: TDbgInfo; const AliasName: String; AliasParent, AOriginalSymbol: TDbgSymbol): TDbgSymAlias;
function AddGlobalSimpleType(info: TDbgInfo; const TypeName: String; AType : TDbgSimpleType): TDbgSymSimpleType;

procedure LoadDebugInfoFromFile(info: TDbgInfo; const FileName: UnicodeString);

implementation

var
  SrcClasses  : TFPList;
  InfoClasses : TFPList;

procedure LoadDebugInfoFromFile(info: TDbgInfo; const FileName: UnicodeString);
var
  source      : TDbgDataSource;
  i           : Integer;
  readerclass : TDbgInfoReaderClass;
  reader      : TDbgInfoReader;

begin
  source := GetDataSource(FileName);
  if not Assigned(source) then
   { writeln('[LoadDebugInfo] cannot find reader for the file: ', FileName)};

  for i := 0 to InfoClasses.Count - 1 do begin
    readerclass := TDbgInfoReaderClass(InfoClasses[i]);
    if readerclass.isPresent(Source) then begin
      reader := ReaderClass.Create(Source);
      try
        reader.ReadDebugInfo(Source, info);
      finally
        reader.Free;
      end;
    end;
  end;
  source.Free;
end;

function AddAliasToSymbol(info: TDbgInfo; const AliasName: String; AliasParent, AOriginalSymbol: TDbgSymbol): TDbgSymAlias;
begin
  Result := info.AddSymbol(AliasName, AliasParent, TDbgSymAlias) as TDbgSymAlias;
  Result.Original:=AOriginalSymbol;
end;

function AddGlobalSimpleType(info: TDbgInfo; const TypeName: String; AType : TDbgSimpleType): TDbgSymSimpleType;
begin
  Result:=info.AddSymbol( TypeName, nil, TDbgSymSimpleType) as TDbgSymSimpleType;
  Result.Simple:=AType;
end;
  
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

function TDbgInfo.FindInGlobal(const SymName: AnsiString; SymClass: TDbgSymClass): TDbgSymbol;
var
  list  : TFPList;
  i     : Integer;
begin
  Result:=nil;
  list:=GetSymList(SymName);
  if SymClass=nil then begin
    if list.Count>0 then
      Result:=TDbgSymbol(list[0]);
  end else
    for i:=0 to list.Count-1 do
      if TObject(list[i]) is SymClass then begin
        Result:=TDbgSymbol(list[i]);
        Exit;
      end;
end;

constructor TDbgInfo.Create;
begin
  inherited Create;
  fGlobalList := TFPObjectList.Create(true);
  fGlobalHash := TFPObjectHashTable.Create(false);
  Root:=TDbgSymbol.Create('', nil);
end;

destructor TDbgInfo.Destroy;
begin
  Root.Free;
  fGlobalList.Free;
  fGlobalHash.Free;
  inherited Destroy;
end;

function TDbgInfo.AddFile(const FileName: WideString): TDbgSymFile;
begin
  Result := FindFile(FileName);
  if Assigned(Result) then Exit;

  Result := AddSymbol( GetFileSymbolName(FileName), RootSymbol, TDbgSymFile ) as TDbgSymFile;
  Result.FileName := FileName;
end;

function TDbgInfo.FindFile(const FileName: WideString): TDbgSymFile;
var
  res     : TDbgSymbol;
begin
  res := FindInGlobal( GetFileSymbolName(FileName));
  if Assigned(res) and (res is TDbgSymFile) then
    Result := TDbgSymFile(res)
  else
    Result := nil;
end;

function TDbgInfo.AddSymbol(const SymbolName: AnsiString;
  ParentSymbol: TDbgSymbol; SymbolClass: TDbgSymClass): TDbgSymbol;
begin
  if ParentSymbol=nil then ParentSymbol:=Root;

  Result := SymbolClass.Create(SymbolName, ParentSymbol);
  AddSymbolToHash(Result);
end;

function TDbgInfo.AddSymbolVar(const VarName:AnsiSTring;ParentSymbol:TDbgSymbol):TDbgSymVar;
begin
  Result:=AddSymbol(VarName, ParentSymbol, TDbgSymVar) as TDbgSymVar;
end;

function TDbgInfo.AddSymbolFunc(const FuncName: AnsiString; ParentSymbol: TDbgSymbol): TDbgSymFunc;
begin
  Result:=AddSymbol(FuncName, ParentSymbol, TDbgSymFunc)  as TDbgSymFunc;
end;

procedure TDbgInfo.AddSymbolToHash(Symbol: TDbgSymbol);
var
  List : TFPList;
begin
  List:=GetSymList(Symbol.Name);
  List.Add(Symbol);
  fGlobalList.Add(Symbol);
end;

function TDbgInfo.GetSymList(const SymName:AnsiString):TFPList;
begin
  Result:=TFPList(fGlobalHash.Items[SymName]);
  if not Assigned(Result) then begin
    Result:=TFPList.Create;
    fGlobalHash.Items[SymName]:=Result;
  end;
end;

function TDbgInfo.AddFileSymbol(const SymbolName: AnsiString; const FileName: WideString;
  const SymbolClass: TDbgSymClass): TDbgSymbol;
begin
  Result := AddSymbol(SymbolName, AddFile(FileName), SymbolClass);
end;

function TDbgInfo.FindSymbol(const SymbolName: AnsiString; Parent: TDbgSymbol;
  SymbolClass: TDbgSymClass; SearchInFiles: Boolean): TDbgSymbol;
var
  i   : integer;
  sym : TDbgSymbol;
begin
  if not Assigned(Parent) then begin
    Result := FindInGlobal(SymbolName);
    if Assigned(Result) or not SearchInFiles then Exit;

    for i := 0 to fGlobalList.Count - 1 do begin
      sym := TDbgSymbol(fGlobalList[i]);
      if sym is TDbgSymFile then begin
        Result := FindSymbol(SymbolName, sym, nil, false);
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
    if TDbgSymbol(fGlobalList[i]) is TDbgSymFile then
      str.Add( TDbgSymFile(fGlobalList[i]).FileName);
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
  subList := TFPObjectList.Create(False);
  subHash := TFPObjectHashTable.Create(false);

  if Assigned(fParent) then begin
    fParent.subHash.Items[AName]:=Self;
    fParent.subList.Add(Self);
  end;
end;

destructor TDbgSymbol.Destroy;
begin
  subHash.Free;
  subList.Free;
  inherited Destroy;
end;

{ TDbgSymFile }

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

constructor TDbgSymFile.Create(const AName: AnsiString; AParentSym: TDbgSymbol);
begin
  AddrToLines := TAVLTree.Create(@OnLinesCompareAddr);
  LinesToAddr := TAVLTree.Create(@OnLinesCompareLine);
  inherited Create(AName, AParentSym);
end;

destructor TDbgSymFile.Destroy;
begin
  AddrToLines.Free;
  LinesToAddr.Free;
  inherited Destroy;
end;

procedure TDbgSymFile.AddLineInfo(const Addr: TDbgPtr; const LineNum: Integer);
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

function TDbgSymFile.FindLineByAddr(const Addr: TDbgPtr; var LineNum: Integer; Strict: Boolean = false): Boolean;
var
  node  : TAVLTreeNode;
  i     : Integer;
  a     : TDbgPtr;
begin
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

function TDbgSymFile.FindAddrByLine(const LineNum: Integer; var Addr: TDbgPtr): Boolean;
var
  node  : TAVLTreeNode;
begin
  node := AddrToLines.FindKey(@LineNum, @CompareWithLine);
  Result := Assigned(node) and Assigned(node.Data);
  if Result then Addr := TDbgLineINfo(node.Data).Addr;
end;

function TDbgSymFile.GetLinesInfoCount:Integer;
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
      writeln('fn =  ', fn,' "', ExtractFileName(fn),'"');
      writeln('chk = ', chk);
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
  dfile : TDbgSymFile;
begin
  Result:=False;
  if (FullFileName='') then Exit;

  dfile:=info.FindFile(FullFileName);
  Result:=Assigned(dfile);
  if not Result then Exit;

 Result:= dfile.FindAddrByLine(LineNum, addr);
end;

{ TDbgSymSimpleType }

function TDbgSymSimpleType.GetVarSize: LongWord;
const
  SizeOfSimpleType : array [TDbgSimpleType] of Integer = (
    1, 2, 4, 8, // dstSInt8,   dstSInt16,  dstSInt32, dstSInt64,
    1, 2, 4, 8, // dstUInt8,   dstUInt16,  dstUInt32, dstUInt64,
    4, 6, 8,    // dstFloat32, dstFloat48, dstFloat64,
    1, 2, 4,    // dstBool8,   dstBool16,  dstBool32,
    1, 2        // dstChar8,   dstChar16
  );
begin
  Result:=SizeOfSimpleType[Simple];
end;

{ TDbgSymType }

function TDbgSymType.isRefType:Boolean;
begin
  Result:=False;
end;

function TDbgSymType.DerefOfs:PtrInt;
begin
  Result:=0;
end;

function TDbgSymType.isIndexAccess:Boolean;
begin
  Result:=False;
end;

function TDbgSymType.GetItemOffset(AIndex:PtrInt; var Offset: PtrUInt): Boolean;
begin
  Offset:=0;
  Result:=False;
end;

initialization
  InitDebugInfoLists;

finalization
  ReleaseDebugInfoLists;


end.

