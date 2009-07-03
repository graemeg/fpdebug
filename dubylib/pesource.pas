unit PESource; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, dbgInfoTypes, DbgPETypes; 
  
type
  
  { TPEFileSource }

  TPEFileSource = class(TDbgDataSource)
  public
    class function isValid(ASource: TStream): Boolean; override;
    function DataCount(Source: TStream): Integer; override;
    function GetDataStream(Source: TStream; SourceIndex: Integer; var Offset, Size: Int64): Boolean; override;
  end;

implementation

function isValidPEStream(ASource: TStream): Boolean;
var
  DosHeader: TImageDosHeader;
begin
  try
    Result := false;
    if ASource.Read(DosHeader, sizeof(DosHeader)) <> sizeof(DosHeader) then Exit;
    if (DosHeader.e_magic <> IMAGE_DOS_SIGNATURE) or (DosHeader.e_lfanew = 0) then Exit;
  except
    Result := false;
  end;
end;


class function TPEFileSource.isValid(ASource: TStream): Boolean;  
begin
  Result := isValidPEStream(ASource);
end;

function TPEFileSource.DataCount(Source: TStream): Integer; 
begin
  Result := 0;
end;

function TPEFileSource.GetDataStream(Source: TStream; SourceIndex: Integer; var Offset, Size: Int64): Boolean; 
begin
  Result := false;
end;


end.

