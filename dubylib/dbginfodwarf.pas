unit dbgInfoDwarf;

{$ifdef fpc}{$mode delphi}{$H+}{$endif}

interface

uses
  Classes, SysUtils, dbgInfoTypes,
  dbgTypes, dwarf,
  dwarfTypes, dwarfConst;

type

  { TDbgDwarf3Info }

  TDbgDwarf3Info = class(TDbgInfoReader)
  private
    fSource : TDbgDataSource;
  public
    class function isPresent(ASource: TDbgDataSource): Boolean; override;
    constructor Create(ASource: TDbgDataSource); override;

    function ReadDebugInfo(ASource: TDbgDataSource; Info: TDbgInfo): Boolean; override;

    procedure dump_debug_abbrev;
    procedure dump_debug_info;
    procedure dump_debug_info2;
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

function TDbgDwarf3Info.ReadDebugInfo(ASource: TDbgDataSource; Info: TDbgInfo): Boolean;
begin
  Result:=false;
end;


procedure TDbgDwarf3Info.dump_debug_abbrev;
begin
  writeln('dump_debug_abbrev undone!');
end;

procedure TDbgDwarf3Info.dump_debug_info;
var
  sz    : Int64;
  buf   : Pointer;
  dwarf : TDbgDwarf; 
  sc    : TDwarfSection;
  i     : Integer;
begin
  dwarf := TDbgDwarf.Create;
  
  if not fSource.GetSectionInfo('.debug_info', sz) then begin
    writeln('no .debug_info section');
    Exit;
  end;
  
  for sc:=low(sc) to High(sc) do begin
    if fSource.GetSectionInfo(DWARF_SECTION_NAME[sc], sz) then begin
      //todo: virtual address
      buf:=dwarf.GetSectionData(sc, sz, 0);
      sz:=fSource.GetSectionData(DWARF_SECTION_NAME[sc], 0, sz, PByteArray(buf)^);  
    end else
      dwarf.GetSectionData(sc, 0, 0);
  end;

  dwarf.LoadCompilationUnits;
  
  for i:=0 to dwarf.Count-1 do begin
    writeln('Filename: ', dwarf.CompilationUnits[i].FileName);
  end;
  
  dwarf.Free;
end;


procedure WriteEntry(Entry: TDwarfEntry; const Prefix: AnsisTring);
var
  s : AnsisTring;
begin
  if not Assigned(Entry) then Exit;

  write(Prefix, DwarfTagToString(Entry.Tag));
  if Entry.GetStr(DW_AT_name, s) then write(': ', s);
  writeln;

  if Assigned(Entry.Child) then WriteEntry(Entry.Child, Prefix+'  ');
  if Assigned(Entry.Next) then WriteEntry(Entry.Next, Prefix);
end;

procedure TDbgDwarf3Info.dump_debug_info2; 
var
  dwarf : TDwarfReader; 
  size  : Int64;
begin
  dwarf := TDwarfReader.Create;
  
  if not fSource.GetSectionInfo('.debug_info', size) then begin
    writeln('no .debug_info section');
    Exit;
  end;
  
  if fSource.GetSectionInfo('.debug_info', dwarf.InfoSize) then begin
    SetLength(dwarf.Info, size);
    fSource.GetSectionData('.debug_info', 0, dwarf.InfoSize, dwarf.Info);
  end;
  
  if fSource.GetSectionInfo('.debug_abbrev', dwarf.AbbrevsSize) then begin
    SetLength(dwarf.Abbrevs, dwarf.AbbrevsSize);
    fSource.GetSectionData('.debug_abbrev', 0, dwarf.AbbrevsSize, dwarf.Abbrevs);
  end;

  dwarf.ReadDwarf;
  WriteEntry(dwarf.FirstEntry, '');
  dwarf.Free;
end;

initialization
  RegisterDebugInfo(TDbgDwarf3Info);

end.

