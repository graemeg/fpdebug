unit elfDbgSource;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  dbgTypes, dbgInfoTypes,
  elf;  // these files are part of


type
  TElfSection = packed record
    name      : AnsiString;
    FileOfs   : QWord;
    Address   : QWord;
    Size      : QWord;
  end;

  { TElfFile }

  TElfFile = class(TObject)
  protected
    function Load32BitFile(Stream: TStream): Boolean;
    procedure AddSection(const name: AnsiString; FileOffset, Address, Size: Qword);
  public
    sections  : array of TElfSection;
    seccount  : Integer;
    function LoadFromFile(Stream: TStream): Boolean;
    function FindSection(const Name: String): Integer;
  end;

  { TElfDbgSource }

  TElfDbgSource = class(TDbgDataSource) // executable parser
  private
    fSource     : TStream;
    fOwnSource  : Boolean;
    fElfFile    : TElfFile;
  public
    class function isValid(ASource: TStream): Boolean; override;
    class function UserName: AnsiString; override;
    constructor Create(ASource: TStream; OwnSource: Boolean); override;
    destructor Destroy; override;

    function GetSectionInfo(const SectionName: AnsiString; var Size: int64): Boolean; override;
    function GetSectionData(const SectionName: AnsiString; Offset, Size: Int64; var Buf: array of byte): Int64; override;
  end;

implementation

{ TElfFile }

function TElfFile.Load32BitFile(Stream: TStream): Boolean;
var
  hdr   : Elf32_Ehdr;
  sect  : array of Elf32_shdr;
  i, j  : integer;
  nm    : string;
  sz    : LongWord;
  strs  : array of byte;
begin
  Result := Stream.Read(hdr, sizeof(hdr)) = sizeof(hdr);
  if not Result then Exit;

  SetLength(sect, hdr.e_shnum);
  Stream.Position := hdr.e_shoff;


  sz := hdr.e_shetsize * hdr.e_shnum;
  if sz > length(sect)*sizeof(Elf32_shdr) then
    sz := length(sect)*sizeof(Elf32_shdr);
  Stream.Read(sect[0], sz);

  i := sect[hdr.e_shstrndx].sh_offset;
  j := sect[hdr.e_shstrndx].sh_size;
  SetLength(strs, j);
  Stream.Position:=i;
  Stream.Read(strs[0], j);

  for i := 0 to hdr.e_shnum - 1 do
    with sect[i] do begin
      nm := PChar( @strs[sh_name] );
      AddSection(nm, sh_offset, sh_addr, sh_size );
    end;

end;

procedure TElfFile.AddSection(const name: AnsiString; FileOffset, Address,
  Size: Qword);
begin
  if seccount=Length(sections) then begin
    if seccount = 0 then SetLength(sections, 4)
    else SetLength(sections, seccount*2);
  end;
  sections[seccount].Address:= Address;
  sections[seccount].name:=name;
  sections[seccount].FileOfs:=FileOffset;
  sections[seccount].Size:=Size;
  inc(seccount);
end;

function TElfFile.LoadFromFile(Stream: TStream): Boolean;
var
  ident : array [0..EINDENT-1] of byte;
  p     : Int64;
begin
  try
    p := Stream.Position;
    Result := Stream.Read(ident, sizeof(ident)) = sizeof(ident);
    if not Result then Exit;

    Result := (ident[EI_MAG0] = $7f) and
              (ident[EI_MAG1] = byte('E')) and
              (ident[EI_MAG2] = byte('L')) and
              (ident[EI_MAG3] = byte('F'));
    if not Result then Exit;

    Result := ident[EI_CLASS] = ELFCLASS32;
    if not Result then Exit; //todo: 64-bit

    Stream.Position := p;
    Result := Load32BitFile(Stream);

  except
    Result := false;
  end;
end;

function TElfFile.FindSection(const Name: String): Integer;
var
  i : Integer;
begin
  Result := -1;
  for i := 0 to seccount - 1 do
    if sections[i].name = Name then begin
      Result := i;
      Exit;
    end;
end;

{ TElfDbgSource }

class function TElfDbgSource.isValid(ASource: TStream): Boolean;
var
  buf : array [0..3] of byte;
begin
  try
    Result := Assigned(ASource) and (ASource.size > sizeof(Elf32_EHdr));
    if not Result then Exit;

    ASource.Read(buf, sizeof(buf));
    Result := (buf[EI_MAG0] = $7f) and (buf[EI_MAG1] = byte('E')) and
              (buf[EI_MAG2] = byte('L')) and (buf[EI_MAG3] = byte('F'));
  except
    Result := false;
  end;
end;

class function TElfDbgSource.UserName: AnsiString;
begin
  Result := 'ELF executable';
end;

constructor TElfDbgSource.Create(ASource: TStream; OwnSource: Boolean);
begin
  inherited Create(ASource, OwnSource);
  fSource := ASource;
  fOwnSource := OwnSource;
  fElfFile := TElfFile.Create;
  fElfFile.LoadFromFile(ASource);
end;

destructor TElfDbgSource.Destroy;
begin
  if fOwnSource then fSource.Free;
  fElfFile.Free;
  inherited Destroy;
end;

function TElfDbgSource.GetSectionInfo(const SectionName: AnsiString;
  var Size: int64): Boolean;
var
  i : Integer;
begin
  i := fElfFile.FindSection(SectionName);
  Result := i >= 0;
  if not Result then Exit;

  Size := fElfFile.sections[i].Size;
end;

function TElfDbgSource.GetSectionData(const SectionName: AnsiString; Offset,
  Size: Int64; var Buf: array of byte): Int64;
var
  i   : Integer;
  sz  : Int64;
begin
  Result := 0;
  i := fElfFile.FindSection(SectionName);
  if i < 0 then Exit;

  fSource.Position := fElfFile.sections[i].FileOfs + Offset;

  if Offset + Size > fElfFile.sections[i].Size  then
    sz := fElfFile.sections[i].Size - Offset
  else
    sz := Size;
  if sz > 0 then Result := fSource.Read(Buf[0], sz)
  else Result := 0;
end;

initialization
  RegisterDataSource ( TElfDbgSource );

end.

