unit machoDbgSource;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, macho, machofile, dbgInfoTypes;

type

  { TDbgMachoDataSource }

  TDbgMachoDataSource = class(TDbgDataSource)
  private
    fSource     : TStream;
    fOwnSource  : Boolean;
    fFile       : TMachoFile;
  protected
    procedure ReadFile;
  public
    class function isValid(ASource: TStream): Boolean; override;
    class function UserName: AnsiString; override;
  public
    constructor Create(ASource: TStream; OwnSource: Boolean); override;
    destructor Destroy; override;

    function SectionsCount: Integer; override;
    function GetSection(index: Integer; var Name: AnsiString; var Size: Int64): Boolean; override;
    function GetSectionData(index: Integer; outStream: TStream): Boolean; override;
  end;


implementation

function isValidMachoStream(stream: TStream): Boolean;
var
  header  : mach_header;
begin
  try
    Result := Assigned(stream);
    if not Result then Exit;
    Result := stream.Read(header, sizeof(header)) = sizeof(header);
    if not Result then Exit;
    Result := (header.magic = MH_CIGAM) or (header.magic = MH_MAGIC);
  except
    Result := false;
  end;
end;

{ TDbgMachoDataSource }

class function TDbgMachoDataSource.isValid(ASource: TStream): Boolean;
begin
  Result := isValidMachoStream(ASource);
end;

class function TDbgMachoDataSource.UserName: AnsiString;
begin
  Result:='mach-o file';
end;

procedure TDbgMachoDataSource.ReadFile;
begin
  if Assigned(fFile) then fFile.Free;
  fFile:=TMachOFile.Create;
  fFile.LoadFromStream(fSource);
end;

constructor TDbgMachoDataSource.Create(ASource: TStream; OwnSource: Boolean);
begin
  fSource := ASource;
  fOwnSource := OwnSource;
  inherited Create(ASource, OwnSource);
end;

destructor TDbgMachoDataSource.Destroy;
begin
  if Assigned(fFile) then fFile.Free;
  if fOwnSource then fSource.Free;
  inherited Destroy;
end;

function TDbgMachoDataSource.SectionsCount: Integer;
begin
  if not Assigned(fFile) then ReadFile;
  Result := fFile.Sections.Count;
end;

function FixName(const macsectionname: String): String;
begin
  if Copy(macsectionName, 1, 2) = '__' then
    Result:= '.'+Copy(macsectionName, 3, length(macsectionName)-2)
  else
    Result := macsectionname;
end;

function TDbgMachoDataSource.GetSection(Index: Integer; var Name: AnsiString; var Size: Int64): Boolean;
begin
  if not Assigned(fFile) then ReadFile;
  Result := (Index >= 0) and (Index < fFile.Sections.Count);
  if not Result then Exit;

  with TMachoSection(fFile.sections[index]) do
    if is32 then begin
      Name := FixName(sec32.sectname);
      Size := sec32.size;
    end else begin
      Name := FixName(sec64.sectname);
      Size := sec64.size;
    end;
end;

function TDbgMachoDataSource.GetSectionData(index: Integer; outStream: TStream): Boolean;
var
  ofs : Int64;
  sz  : Int64;
begin
  if not Assigned(outStream) then begin
    Result := false;
    Exit;
  end;

  if not Assigned(fFile) then ReadFile;
  Result := (Index >= 0) and (Index < fFile.Sections.Count);
  if not Result then Exit;

  with TMachOsection(fFile.sections[index]) do begin
    if is32 then begin
      ofs := sec32.offset;
      sz := sec32.size;
    end else begin
      ofs := sec64.offset;
      sz := sec64.size;
    end;
  end;
  if ofs > 0 then begin
    fSource.Position:=ofs;
    outStream.CopyFrom(fSource, sz);
  end;
end;

initialization
  RegisterDataSource ( TDbgMachoDataSource );

end.

