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

    procedure dump_debug_abbrev;
    procedure dump_debug_info;
  end;

implementation

{ TDbgDwarf3Info }

class function TDbgDwarf3Info.isPresent(ASource: TDbgDataSource): Boolean;
var
  sz  : Int64;
begin
  Result := Assigned(ASource) and (ASource.GetSectionInfo('.debug_info', sz)) and (sz > 0);
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

procedure TDbgDwarf3Info.dump_debug_abbrev;
begin
  
end;

procedure TDbgDwarf3Info.dump_debug_info;
var
  i     : integer;
  sz    : Int64;
  data  : array of byte;
  buf   : PByteArray;
  cu32  : PDwarfCUHeader32;
  cu64  : PDwarfCUHeader64;
  c     : Integer;
begin
  
  if not fSource.GetSectionInfo('.debug_info', sz) then Exit;
  SetLength(data, sz);
  fSource.GetSectionData('.debug_info', 0, sz, data);
  buf := @data[0];
  i := 0;
  c := 1;
  writeln('Compilation Units:');
  while i < sz do begin
    writeln('i = ', i);
    cu32 := @buf^[i];
    if cu32^.Length = DWARF_HEADER64_SIGNATURE then begin
      cu64 := PDwarfCUHeader64(cu32);
      writeln('v: ',cu64^.Version, '; addrsize: ', cu64^.AddressSize, '; ofs: ', cu64^.AbbrevOffset, '; len: ', cu64^.Length);
      inc(i, sizeof (TDwarfCUHeader64) + cu64^.Length - 12);
    end else begin
      writeln('v: ',cu32^.Version, '; addrsize: ', cu32^.AddressSize, '; ofs: ', cu32^.AbbrevOffset, '; len: ', cu32^.Length);
      inc(i, sizeof(TDwarfCUHeader32) + cu32^.Length - 4);
    end;
    inc(c);
  end;
end;

end.

