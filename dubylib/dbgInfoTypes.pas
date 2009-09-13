unit dbgInfoTypes; 

{$ifdef fpc}{$mode delphi}{$H+}{$endif}

interface

uses
  contnrs, SysUtils, Classes, dbgTypes; 

type
  { TDbgDataSource }

  TDbgDataSource = class(TObject) // executable parser
  public
    class function isValid(ASource: TStream): Boolean; virtual; abstract;
    constructor Create(ASource: TStream; OwnSource: Boolean); virtual; 

    function SectionsCount: Integer; virtual; abstract;
    function GetSectionInfo(SourceIndex: Integer; var SourceName: AnsiString; var Offset, Size: Int64): Boolean; virtual; abstract;    
    function GetSource: TStream; virtual; abstract;
  end;
  TDbgDataSourceClass = class of TDbgDataSource;
  
  { TDbgInfo }

  TDbgInfo = class(TObject)
  public
    class function isPresent(ASource: TDbgDataSource): Boolean; virtual; abstract;
    constructor Create(ASource: TDbgDataSource); virtual; 
    function GetLineInfo(const Addr: TDbgPtr; var FileName: String; var LineNum, ColumnNum: Integer): Boolean; virtual; abstract;
  end;
  TDbgInfoClass = class of TDbgInfo;
  
 
function GetDataSource(ASource: TStream; OwnSource: Boolean): TDbgDataSource;
function GetDebugInfo(InfoStream: TStream; OwnInfoStream: Boolean): TDbgInfo; 

procedure RegisterDataSource( DataSource: TDbgDataSourceClass); 
procedure RegisterDebugInfo( DebugInfo: TDbgInfoClass ); 
  
implementation

{$ifndef fpc}
type
  TFPList = TList;
  TFPObjectList = TObjectList;
{$endif}


var
  sources  : TFPList;
  readers  : TFPObjectList;

function GetDataSource(ASource: TStream; OwnSource: Boolean): TDbgDataSource;
var
  i   : Integer;
  cls : TDbgDataSourceClass;
  p : Int64;
begin
  Result := nil;
  if not Assigned(ASource) then Exit;
  
  p := ASource.Position;
  writeln('-- sources count ', sources.Count);
  for i := 0 to sources.Count - 1 do begin
    cls :=  TDbgDataSourceClass(sources[i]);
    try
      ASource.Position := P;
      if cls.isValid(ASource) then begin 
        writelN('-- is valid source!');
        ASource.Position := p;
        writelN('-- creating! cls = ', cls.ClassName);
        Result := cls.Create(ASource, OwnSource);
        writeln('result = ', PtrUInt(Result));
        Exit;
      end else
        writeln('-- failed to detect for ', cls.ClassName);
        
    except
      on e: exception do begin
        writeln('exception! WHY? ', e.Message);
      end;
    end;
  end;
  Result := nil;
end;

function GetDebugInfo(InfoStream: TStream; OwnInfoStream: Boolean): TDbgInfo; 
var
  i   : Integer;
  p   : Int64;
//  reader : TDbgInfoReader;
begin
  Result := nil;
{  if not Assigned(InfoStream) then Exit;  

  p := InfoStream.Position;  
  for i := 0 to readers.Count - 1 do begin
    InfoStream.Position := p;
    reader := TDbgInfoReader(readers[i]);      
    try
      Result := reader.CreateDebugInfo(InfoStream, OwnInfoStream);
      if Assigned(Result) then Exit;
    except
    end;
  end;}
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

