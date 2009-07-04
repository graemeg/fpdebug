unit PESource; 

{
 This unit contains the types needed for reading PE images.
 At some time this may go to be part of the rtl ?

 ---------------------------------------------------------------------------

 @created(Thu May 4th WET 2006)
 @lastmod($Date: 2009-01-16 03:26:10 +0300 (Пт, 16 янв 2009) $)
 @author(Marc Weustink <marc@@dommelstein.nl>)

 @modified by dmitry boyarintsev (july 2009: 
   + removed Windows unit dependancy. added SectionCount and 
   + added Sections access by Index
}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, dbgInfoTypes, DbgPETypes; 
  
type
  TDbgImageSection = record
    RawData: Pointer;
    Size: QWord;
    VirtualAdress: QWord;
  end;
  PDbgImageSection = ^TDbgImageSection;


  { TDbgImageLoader }

  TDbgImageLoader = class(TObject)
  private
    FImage64Bit: Boolean;
    FImageBase: QWord;
    FSections: TStringList;
    function GetSection(const AName: String): PDbgImageSection;
  protected
    procedure Add(const AName: String; ARawData: Pointer; ASize: QWord; AVirtualAdress: QWord);
    procedure SetImageBase(ABase: QWord);
    procedure SetImage64Bit(AValue: Boolean);
    procedure LoadSections; virtual; abstract;
    procedure UnloadSections; virtual; abstract;
    function GetSectionsCount: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    function GetSectionByIndex(i: Integer):  PDbgImageSection;
    property ImageBase: QWord read FImageBase;
    Property Image64Bit: Boolean read FImage64Bit;
    property Section[const AName: String]: PDbgImageSection read GetSection;
    property SectionsCount: Integer read GetSectionsCount;
  end;
  
  { TDbgPEImageLoader }

  TDbgPEImageLoader = class(TDbgImageLoader)
  private
  protected
    function  LoadData(out AModuleBase: Pointer; out AHeaders: PImageNtHeaders): Boolean; virtual; abstract;
    procedure LoadSections; override;
    procedure UnloadData; virtual; abstract;
    procedure UnloadSections; override;
  public
  end;
  
  { TDbgWinPEImageLoader }

  TDbgWinPEImageLoader = class(TDbgPEImageLoader)
  private
    FStream   : TStream;
    data      : array of byte;
    FModulePtr: Pointer;
    procedure DoCleanup;
  protected
    function  LoadData(out AModuleBase: Pointer; out AHeaders: PImageNtHeaders): Boolean; override;
    procedure UnloadData; override;
  public
    constructor Create(ASource: TStream);
  end;

  
  { TPEFileSource }

  TPEFileSource = class(TDbgDataSource)
  private
    fLoader     : TDbgWinPEImageLoader;
    fStream     : TStream;
    fOwnStream  : Boolean;
  public
    class function isValid(ASource: TStream): Boolean; override;
    
    constructor Create(ASource: TStream; OwnSource: Boolean); override;
    destructor Destroy; override;
    
    function SectionsCount: Integer; override;
    function GetSectionInfo(Index: Integer; var Name: AnsiString; var Offset, Size: Int64): Boolean; override;
    function GetSource: TStream; override;
  end;

implementation

function isValidPEStream(ASource: TStream): Boolean;
var
  DosHeader: TImageDosHeader;
begin
  try
    Result := false;
    if ASource.Read(DosHeader, sizeof(DosHeader)) <> sizeof(DosHeader) then 
      Exit;
    if (DosHeader.e_magic <> IMAGE_DOS_SIGNATURE) or (DosHeader.e_lfanew = 0) then 
      Exit;
    Result := true;
  except
    Result := false;
  end;
end;

constructor TPEFileSource.Create(ASource: TStream; OwnSource: Boolean);  
begin
  fLoader := TDbgWinPEImageLoader.Create(ASource);
  fLoader.LoadSections;
  fStream := ASource;
  fOwnStream := OwnSource;
  inherited Create(ASource, OwnSource);  
end;

destructor TPEFileSource.Destroy;  
begin
  fLoader.Free;
  if fOwnStream then fStream.Free;
  inherited Destroy;  
end;

class function TPEFileSource.isValid(ASource: TStream): Boolean;  
begin
  Result := isValidPEStream(ASource);
end;

function TPEFileSource.SectionsCount: Integer; 
begin
  Result := fLoader.SectionsCount;
end;

function TPEFileSource.GetSectionInfo(Index: Integer; var Name: AnsiString; var Offset, Size: Int64): Boolean; 
var
  sc : PDbgImageSection;
begin
  sc := fLoader.GetSectionByIndex(Index);
  Result := Assigned(sc);
  if not Result then Exit;
  Offset := sc^.VirtualAdress;
  Size := sc^.Size;
end;

function TPEFileSource.GetSource: TStream;  
begin
  Result:=fStream;
end;

{ TDbgImageLoader }

procedure TDbgImageLoader.Add(const AName: String; ARawData: Pointer; ASize: QWord; AVirtualAdress: QWord);
var
  p: PDbgImageSection;
  idx: integer;
begin
  idx := FSections.AddObject(AName, nil);
  New(p);
  P^.RawData := ARawData;
  p^.Size := ASize;
  p^.VirtualAdress := AVirtualAdress;
  FSections.Objects[idx] := TObject(p);
end;

constructor TDbgImageLoader.Create;
begin
  inherited Create;
  FSections := TStringList.Create;
  FSections.Sorted := True;
  //FSections.Duplicates := dupError;
  FSections.CaseSensitive := False;
  LoadSections;
end;

destructor TDbgImageLoader.Destroy;
var
  n: integer;
begin
  UnloadSections;
  for n := 0 to FSections.Count - 1 do
    Dispose(PDbgImageSection(FSections.Objects[n]));
  FSections.Clear;
  inherited Destroy;
  FreeAndNil(FSections);
end;

function TDbgImageLoader.GetSectionByIndex(i: Integer): PDbgImageSection; 
begin
  Result := PDbgImageSection(FSections.Objects[i]);
end;

function TDbgImageLoader.GetSection(const AName: String): PDbgImageSection;
var
  idx: integer;
begin
  idx := FSections.IndexOf(AName);
  if idx = -1
  then Result := nil
  else Result := PDbgImageSection(FSections.Objects[idx]);
end;

procedure TDbgImageLoader.SetImage64Bit(AValue: Boolean);
begin
  FImage64Bit := AValue;
end;

function TDbgImageLoader.GetSectionsCount: Integer; 
begin
  Result := FSections.Count;
end;

procedure TDbgImageLoader.SetImageBase(ABase: QWord);
begin
  FImageBase := ABase;
end;

{ TDbgPEImageLoader }

procedure TDbgPEImageLoader.LoadSections;
var
  ModulePtr: Pointer;
  NtHeaders: PImageNtHeaders;
  NtHeaders32: PImageNtHeaders32 absolute NtHeaders;
  NtHeaders64: PImageNtHeaders64 absolute NtHeaders;
  SectionHeader: PImageSectionHeader;
  n: Integer;
  p: Pointer;
  SectionName: array[0..IMAGE_SIZEOF_SHORT_NAME] of Char;
begin
  if not LoadData(ModulePtr, NtHeaders) then Exit;
  
  if NtHeaders^.Signature <> IMAGE_NT_SIGNATURE
  then begin
    //WriteLn('Invalid NT header: ', IntToHex(NtHeaders^.Signature, 8));
    Exit;
  end;

  SetImage64Bit(NtHeaders^.OptionalHeader.Magic = IMAGE_NT_OPTIONAL_HDR64_MAGIC);

  if Image64Bit
  then SetImageBase(NtHeaders64^.OptionalHeader.ImageBase)
  else SetImageBase(NtHeaders32^.OptionalHeader.ImageBase);

  for n := 0 to NtHeaders^.FileHeader.NumberOfSections - 1 do
  begin
    SectionHeader := Pointer(@NtHeaders^.OptionalHeader) + NtHeaders^.FileHeader.SizeOfOptionalHeader + SizeOf(TImageSectionHeader) * n;
    // make a null terminated name
    Move(SectionHeader^.Name, SectionName, IMAGE_SIZEOF_SHORT_NAME);
    SectionName[IMAGE_SIZEOF_SHORT_NAME] := #0;
    if (SectionName[0] = '/') and (SectionName[1] in ['0'..'9'])
    then begin
      // long name
      p := ModulePtr + NTHeaders^.FileHeader.PointerToSymbolTable + NTHeaders^.FileHeader.NumberOfSymbols * IMAGE_SIZEOF_SYMBOL + StrToIntDef(PChar(@SectionName[1]), 0);
      Add(PChar(p), ModulePtr + SectionHeader^.PointerToRawData, SectionHeader^.Misc.VirtualSize,  SectionHeader^.VirtualAddress);
    end
    else begin
      // short name
      Add(SectionName, ModulePtr + SectionHeader^.PointerToRawData, SectionHeader^.Misc.VirtualSize,  SectionHeader^.VirtualAddress);
    end
  end;
end;

procedure TDbgPEImageLoader.UnloadSections;
begin
  UnloadData;
end;

{ TDbgWinPEImageLoader }

constructor TDbgWinPEImageLoader.Create(ASource: TStream);
begin
  fStream := ASource;
  inherited Create;
end;

procedure TDbgWinPEImageLoader.DoCleanup;
begin
  FStream.Free;
  SetLength(data, 0);
end;

function TDbgWinPEImageLoader.LoadData(out AModuleBase: Pointer; out AHeaders: PImageNtHeaders): Boolean;
var
  DosHeader: PImageDosHeader;
begin
  Result := False;

  try
    SetLength(data, FStream.Size);
    FStream.Read(data[0], length(data));
    //FMapHandle := CreateFileMapping(FFileHandle, nil, PAGE_READONLY{ or SEC_IMAGE}, 0, 0, nil);
    {if FMapHandle = 0
    then begin
      WriteLn('Could not create module mapping');
      Exit;
    end;}

    //FModulePtr := MapViewOfFile(FMapHandle, FILE_MAP_READ, 0, 0, 0);
    FModulePtr := @data[0];
    if FModulePtr = nil
    then begin
      //WriteLn('Could not map view');
      Exit;
    end;

    DosHeader := FModulePtr;
    if (DosHeader^.e_magic <> IMAGE_DOS_SIGNATURE)
    or (DosHeader^.e_lfanew = 0)
    then begin
      //WriteLn('Invalid DOS header');
      Exit;
    end;
    
    AModuleBase := FModulePtr;
    AHeaders := FModulePtr + DosHeader^.e_lfanew;
    Result := True;
  finally
    if not Result
    then begin
      // something failed, do some cleanup
      DoCleanup;
    end;
  end;
end;

procedure TDbgWinPEImageLoader.UnloadData;
begin
  DoCleanup;
end;

initialization
  RegisterDataSource(TPEFileSource);

end.

