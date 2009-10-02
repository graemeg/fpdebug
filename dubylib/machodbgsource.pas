unit machoDbgSource;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, macho, machofile, dbgInfoTypes, stabs;

type

  { TDbgMachoDataSource }

  TDbgMachoDataSource = class(TDbgDataSource)
  private
    fSource     : TStream;
    fOwnSource  : Boolean;
    fFile       : TMachoFile;
    isStabs     : Boolean;
    StabsCmd    : symtab_command;
    fileRead    : Boolean;
  protected
    procedure ReadFile;
    function GetStabSectionInfo(StabStr: Boolean; var SectionOffset, SectionSize: Int64): Boolean;
    function GetSectionIndex(const SectionName: AnsiString): Integer;
  public
    class function isValid(ASource: TStream): Boolean; override;
    class function UserName: AnsiString; override;
  public
    constructor Create(ASource: TStream; OwnSource: Boolean); override;
    destructor Destroy; override;

{    function SectionsCount: Integer;
    function GetSection(index: Integer; var Name: AnsiString; var Size: Int64): Boolean;
    function GetSectionData(index: Integer; outStream: TStream): Boolean;}

    function GetSectionInfo(const SectionName: AnsiString; var Size: int64): Boolean; override;
    function GetSectionData(const SectionName: AnsiString; Offset, Size: Int64; var Buf: array of byte): Integer; override;
  end;

implementation

const
  //StabSectoinName
  _stab    = '.stab';
  _stabstr = '.stabstr';


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

function FixMachoName(const macsectionname: String): String;
begin
  if Copy(macsectionName, 1, 2) = '__' then
    Result:= '.'+Copy(macsectionName, 3, length(macsectionName)-2)
  else
    Result := macsectionname;
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
var
  i : Integer;
begin
  if Assigned(fFile) then fFile.Free;
  fFile:=TMachOFile.Create;
  fFile.LoadFromStream(fSource);
  for i := 0 to fFile.header.ncmds - 1 do begin
    isStabs := fFile.commands[i]^.cmd = LC_SYMTAB;
    if isStabs then begin
      StabsCmd := psymtab_command(fFile.commands[i])^;
      Break;
    end;
  end;
  fileRead := true;
end;

function TDbgMachoDataSource.GetStabSectionInfo(StabStr: Boolean; var SectionOffset, SectionSize: Int64): Boolean;
begin
  Result := isStabs;
  if not Result then Exit;
  if StabStr then begin
    SectionOffset := StabsCmd.stroff;
    SectionSize := StabsCmd.strsize;
  end else begin
    SectionOffset := StabsCmd.symoff;
    SectionSize := Int64(StabsCmd.nsyms * sizeof(TStabSym));
  end;
end;

function TDbgMachoDataSource.GetSectionIndex(const SectionName: AnsiString): Integer;
var
  i     : Integer;
  Name  : AnsiString;
begin
  //todo: hash-table
  for i := 0 to fFile.Sections.Count - 1 do begin
    with TMachoSection(fFile.sections[i]) do
      if is32
        then Name := FixMachoName(sec32.sectname)
        else Name := FixMachoName(sec64.sectname);
    if Name = SectionName then begin
      Result := i;
      Exit;
    end;
  end;
  Result := -1;
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

{function TDbgMachoDataSource.SectionsCount: Integer;
begin
  if not Assigned(fFile) then ReadFile;
  Result := fFile.Sections.Count;
  if isStabs then inc(Result, 2);
end;

function TDbgMachoDataSource.GetSection(Index: Integer; var Name: AnsiString; var Size: Int64): Boolean;
var
  cnt   : Integer;
  sstr  : Boolean;
const
  StabSectionName : array [Boolean] of AnsiString = (_stab, _stabstr);
begin
  if not Assigned(fFile) then ReadFile;
  cnt := fFile.Sections.Count;
  if isStabs then inc(cnt, 2);
  Result := (Index >= 0) and (Index < cnt);
  if not Result then Exit;

  if Index < fFile.Sections.Count then begin
    with TMachoSection(fFile.sections[index]) do
      if is32 then begin
        Name := FixMachoName(sec32.sectname);
        Size := sec32.size;
      end else begin
        Name := FixMachoName(sec64.sectname);
        Size := sec64.size;
      end;
  end else begin
    sstr := Index = cnt - 1;
    Name := StabSectionName[sstr];
    Result := GetStabSectionInfo(sstr, Size);
  end;
end;

function TDbgMachoDataSource.GetSectionData(index: Integer; outStream: TStream): Boolean;
var
  ofs : Int64;
  sz  : Int64;
begin
  //todo: method will be removed
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
end;}

function TDbgMachoDataSource.GetSectionInfo(const SectionName: AnsiString; var Size: int64): Boolean;
var
  idx     : integer;
  stabstr : Boolean;
  dummy   : int64;
begin
  if not fileRead then ReadFile;

  stabstr := (SectionName = _stabstr);
  if stabstr or (SectionName = _stab) then
    Result := GetStabSectionInfo(stabstr, dummy, Size)
  else begin
    idx := GetSectionIndex(SectionName);
    Result := idx >= 0;
    if not Result then Exit;

    with TMachOsection(fFile.sections[idx]) do
      if is32
        then Size := sec32.size
        else Size := sec64.size;
  end;
end;

function TDbgMachoDataSource.GetSectionData(const SectionName: AnsiString; Offset, Size: Int64; var Buf: array of byte): Integer;
var
  idx     : Integer;
  sofs    : int64;
  ssize   : int64;
  stabstr : Boolean;
  sz      : Int64;
  s       : TMachOsection;
begin
  if not fileRead then ReadFile;

  Result := 0;
  stabstr := SectionName = _stabstr;
  if stabstr or (SectionName = _stab)  then begin
    if not GetStabSectionInfo(stabstr, sofs, ssize) then
      Exit;
  end else begin
    idx := GetSectionIndex(SectionName);

    s := TMachOsection(fFile.sections[idx]);
    if not s.is32 then begin
      ssize := s.sec32.size;
      sofs := s.sec32.offset;
    end else begin
      sofs := s.sec64.offset;
      ssize := s.sec64.size;
    end;
  end;

  sz := ssize - Offset;
  if sz < 0 then Exit;

  fSource.Position := sofs + Offset;
  Result := fSource.Read(Buf[0], sz);
end;

initialization
  RegisterDataSource ( TDbgMachoDataSource );

end.

