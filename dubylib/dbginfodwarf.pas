unit dbgInfoDwarf;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, dbgInfoTypes,
  dbgTypes,
  dwarfTypes, dwarfConst;

type
  { TDbgDwarf3Info }

  TDbgDwarf3Info = class(TDbgInfo)
  private
    fSource : TDbgDataSource;
  public
    class function isPresent(ASource: TDbgDataSource): Boolean; override;
    constructor Create(ASource: TDbgDataSource); override;
    function GetDebugData(const DataName: string; DataAddr: TDbgPtr; OutData: TDbgDataList): Boolean; override;

    procedure dump_debug_info;
  end;

implementation

{ TDbgDwarf3Info }

class function TDbgDwarf3Info.isPresent(ASource: TDbgDataSource): Boolean;
var
  i   : integer;
  nm  : AnsiString;
  sz  : Int64;
begin
  Result:=false;
  if not Assigned(ASource) then Exit;

  for i := 0 to ASource.SectionsCount - 1 do begin
    ASource.GetSection(i, nm, sz);
    if nm = '.debug_info' then begin
      Result := true;
      Exit;
    end;
  end;
end;

constructor TDbgDwarf3Info.Create(ASource: TDbgDataSource);
begin
  fSource:=ASource;
  inherited Create(ASource);
end;

function TDbgDwarf3Info.GetDebugData(const DataName: string; DataAddr: TDbgPtr; OutData: TDbgDataList): Boolean;
begin
  Result := false;
end;

procedure TDbgDwarf3Info.dump_debug_info;
var
  mem : TMemoryStream;
  i   : integer;
  nm  : String;
  sz  : Int64;
  buf : PByteArray;
  cu32 : PDwarfCUHeader32;
  cu64 : PDwarfCUHeader64;
  c    : Integer;
begin
  for i := 0 to fSource.SectionsCount - 1 do begin
    fSource.GetSection(i, nm, sz);
    if nm = '.debug_info' then begin
      mem := TMemoryStream.Create;
      fSource.GetSectionData(i, mem);
      mem.Position:=0;
      Break;
    end;
  end;
  if not Assigned(mem) then Exit;

  buf := PByteArray(mem.Memory);
  i := 0;
  c := 1;
  writeln('Compilation Units:');
  while i < mem.Size do begin
    writeln('i = ', i);
    cu32 := @buf^[i];
    if cu32^.Length = DWARF_HEADER64_SIGNATURE then begin
      cu64 := PDwarfCUHeader64(cu32);
      writeln('v: ',cu64^.Version, '; addrsize: ', cu64^.AddressSize, '; ofs: ', cu64^.AbbrevOffset, '; len: ', cu64^.Length);
      inc(i, sizeof (TDwarfCUHeader64) + cu64^.Length);
    end else begin
      writeln('v: ',cu32^.Version, '; addrsize: ', cu32^.AddressSize, '; ofs: ', cu32^.AbbrevOffset, '; len: ', cu32^.Length);
      inc(i, sizeof(TDwarfCUHeader32) + cu32^.Length);
    end;
    inc(c);
  end;

  mem.Free;
end;

end.

