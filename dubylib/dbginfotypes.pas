unit dbgInfoTypes; 

{$mode objfpc}{$H+}

interface

uses
  contnrs, SysUtils, Classes, dbgTypes; 

type
  { TDbgDataSource }

  TDbgDataSource = class(TObject) // executable parser
  public
    class function isValid(ASource: TStream): Boolean; virtual; abstract;
    class function UserName: AnsiString; virtual; abstract;
    constructor Create(ASource: TStream; OwnSource: Boolean); virtual; 

  {  function SectionsCount: Integer; virtual; abstract;
    function GetSection(index: Integer; var Name: AnsiString; var Size: Int64): Boolean; virtual; abstract; //todo: remove?
    function GetSectionData(index: Integer; outStream: TStream): Boolean; virtual; abstract; //todo: remove?}

    function GetSectionInfo(const SectionName: AnsiString; var Size: int64): Boolean; virtual; abstract;
    function GetSectionData(const SectionName: AnsiString; Offset, Size: Int64; var Buf: array of byte): Integer; virtual; abstract;
  end;
  TDbgDataSourceClass = class of TDbgDataSource;
  
  { TDbgInfo }

  TDbgInfo = class(TObject)
  public
    class function isPresent(ASource: TDbgDataSource): Boolean; virtual; abstract;
    constructor Create(ASource: TDbgDataSource); virtual; 
    function GetDebugData(const DataName: string; DataAddr: TDbgPtr; OutData: TDbgDataList): Boolean; virtual; abstract;
    function GetAddrByName(const SymbolName: String; var Addr: TDbgPtr): Boolean; virtual; abstract;
    procedure GetNames(strings: TStrings); virtual; abstract;
  end;
  TDbgInfoClass = class of TDbgInfo;

type
  TCallConv = (cc_fastcall, cc_cdecl, cc_stdcall); //todo:

const
  dinfo_LineInfo   = 'cmn.lineinfo';  // DataName  - requests line number of the give address
  dinfo_FileName   = 'filename';      //    - name of the file
  dinfo_Number     = 'line';          //    - line number
  dinfo_Colon      = 'colon';         //    - colon number

  dinfo_GlobalSymAddr = 'cmn.gsymboladdr';  // Global Symbol address - requests addr of the symbol, specified by name
  dinfo_Addr          = 'addr';             //    - returned address (always 64-bit?)

function GetDataSource(const FileName: string): TDbgDataSource; overload;
function GetDataSource(ASource: TStream; OwnSource: Boolean): TDbgDataSource; overload;

procedure RegisterDataSource(DataSource: TDbgDataSourceClass); 
procedure RegisterDebugInfo(DebugInfo: TDbgInfoClass );

implementation

var
  sources  : TFPList;
  readers  : TFPObjectList;
  
  
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
  //writeln('-- sources count ', sources.Count);
  for i := 0 to sources.Count - 1 do begin
    cls :=  TDbgDataSourceClass(sources[i]);
    try
      ASource.Position := P;
      if cls.isValid(ASource) then begin 
        //writelN('-- is valid source!');
        ASource.Position := p;
        //writelN('-- creating! cls = ', cls.ClassName);
        Result := cls.Create(ASource, OwnSource);
        //writeln('result = ', PtrUInt(Result));
        Exit;
      end else
        ;
        //writeln('-- failed to detect for ', cls.ClassName);
        
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
  if Assigned(DataSource) and (sources.IndexOf(DataSource) < 0) then  begin
    //writeln('-- added new data source ', DataSource.ClassName);
    sources.Add(DataSource)
  end;
end;

procedure RegisterDebugInfo(DebugInfo: TDbgInfoClass); 
begin

end;

procedure InitDebugInfoLists;
begin
  sources := TFPList.Create;
  readers := TFPObjectList.Create(true);
end;

procedure ReleaseDebugInfoLists;
begin 
  sources.Free;
  readers.Free;
end;

{ TDbgDataSource }

constructor TDbgDataSource.Create(ASource: TStream; OwnSource: Boolean); 
begin
  inherited Create;
end;

{ TDbgInfo }

constructor TDbgInfo.Create(ASource: TDbgDataSource); 
begin
  inherited Create;
end;

initialization
  InitDebugInfoLists;

finalization
  ReleaseDebugInfoLists;


end.

