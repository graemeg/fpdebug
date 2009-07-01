unit dbgInfoTypes; 

{$mode objfpc}{$H+}

interface

uses
  contnrs, Classes, dbgTypes; 

type
  // STABS or DWARF 
  TDbgInfo = class(TObject)
  public
    //todo:
    function GetLineInfo(const Addr: TDbgPtr; var FileName: String; var LineNum, ColumnNum: Integer): Boolean; virtual; abstract;
  end;
  
  TDbgInfoReader = class(TObject)
  public
    function CreateDebugInfo(DebugData: TStream; OwnInfoStream: Boolean): TDbgInfo; virtual; abstract;
  end;
  
  { TDbgDataSource }

  TDbgDataSource = class(TObject) // executable parser
  public
    constructor Create(ASource: TStream; OwnSource: Boolean); virtual; 
    class function isValid(ASource: TStream): Boolean; virtual; abstract;
    function DataCount(Source: TStream): Integer; virtual; abstract;
    function GetDataStream(Source: TStream; SourceIndex: Integer; var Offset, Size: Int64): Boolean; virtual; abstract;    
  end;
  TDbgDataSourceClass = class of TDbgDataSource;
  
function GetDataSource(ASource: TStream; OwnSource: Boolean): TDbgDataSource;
function GetDebugInfo(InfoStream: TStream; OwnInfoStream: Boolean): TDbgInfo; 

procedure RegisterDataSource( DataSource: TDbgDataSourceClass); 
procedure RegisterReader( DebugInfoReader: TDbgInfoReader ); 
  
implementation

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
  for i := 0 to sources.Count - 1 do begin
    cls :=  TDbgDataSourceClass(sources[i]);
    try
      ASource.Position := P;
      if cls.isValid(ASource) then begin 
        ASource.Position := p;
        Result := cls.Create(ASource, OwnSource);
        Exit;
      end;
    except
    end;
  end;
  Result := nil;
end;

function GetDebugInfo(InfoStream: TStream; OwnInfoStream: Boolean): TDbgInfo; 
var
  i   : Integer;
  p   : Int64;
  reader : TDbgInfoReader;
begin
  Result := nil;
  if not Assigned(InfoStream) then Exit;  

  p := InfoStream.Position;  
  for i := 0 to readers.Count - 1 do begin
    InfoStream.Position := p;
    reader := TDbgInfoReader(readers[i]);      
    try
      Result := reader.CreateDebugInfo(InfoStream, OwnInfoStream);
      if Assigned(Result) then Exit;
    except
    end;
  end;
end;

procedure RegisterDataSource( DataSource: TDbgDataSourceClass); 
begin
  if Assigned(DataSource) and (sources.IndexOf(DataSource) < 0) then 
    sources.Add(DataSource)
end;

procedure RegisterReader( DebugInfoReader: TDbgInfoReader ); 
begin
  if Assigned(DebugInfoReader) and (readers.IndexOf(DebugInfoReader) < 0) then 
    readers.Add(DebugInfoReader);
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

end;

initialization
  InitDebugInfoLists;

finalization
  ReleaseDebugInfoLists;


end.

