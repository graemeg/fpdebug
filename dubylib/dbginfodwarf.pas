unit dbgInfoDwarf;

{$ifdef fpc}{$mode delphi}{$H+}{$endif}

interface

uses
  Classes, SysUtils, dbgInfoTypes,
  dbgTypes, dwarf,
  dwarfTypes, dwarfConst;

var
  DebugDwarf_LineInfo  : Boolean = False;
  DebugDwarf_Variables : Boolean = True;

type

  { TDbgDwarf3Info }

  TDbgDwarf3Info = class(TDbgInfoReader)
  private
    fSource   : TDbgDataSource;

    fDbgInfo  : TDbgInfo;
    lineinfo  : TLineInfoStateMachine;
    curfile   : TDbgSymFile;
    parent    : TDbgSymbol;
    procedure ReadCompileUnit(entry: TDwarfEntry; var sym: TDbgSymbol);
    procedure ReadSubPrograms(entry: TDwarfEntry; var sym: TDbgSymbol);
    procedure ReadDwarfEntry(entry: TDwarfEntry);

    procedure dump_lineinfo(entry: TDwarfEntry; ParseSiblings: Boolean=True);
    procedure dump_variables(entry: TDwarfEntry);
  public
    class function isPresent(ASource: TDbgDataSource): Boolean; override;
    constructor Create(ASource: TDbgDataSource); override;

    function ReadDebugInfo(ASource: TDbgDataSource; Info: TDbgInfo): Boolean; override;

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
var
  size    : Int64;
  reader  : TDwarfReader;
  entry   : TDwarfEntry;

const
  _debug_info   = '.debug_info';
  _debug_abbrev = '.debug_abbrev';
  _debug_line   = '.debug_line';

begin
  Result:=ASource.GetSectionInfo(_debug_info, size);
  if not Result then begin
    //writeln('no .debug_info');
    Exit;
  end;

  reader:=TDwarfReader.Create;
  try
    reader.InfoSize:=size;
    SetLength(reader.Info, size);
    if size>0 then ASource.GetSectionData(_debug_info, 0, size, reader.Info);

    if ASource.GetSectionInfo(_debug_abbrev, size) then begin
      reader.AbbrevsSize:=size;
      SetLength(reader.Abbrevs, size);
      if size>0 then
        ASource.GetSectionData(_debug_abbrev, 0, size, reader.Abbrevs);
    end;

    lineinfo:=TLineInfoStateMachine.Create;
    if ASource.GetSectionInfo(_debug_line, size) then begin
      SetLength(lineinfo.Data, size);
      ASource.GetSectionData(_debug_line, 0, size, lineinfo.Data);
    end;

    Result:=reader.ReadDwarf;
    if not Result then Exit;

    entry:=reader.FirstEntry;
    fDbgInfo:=Info;
    while Assigned(entry) do begin
      ReadDwarfEntry(entry);
      entry:=entry.Next;
    end;
  finally
    reader.Free;
  end;
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

procedure TDbgDwarf3Info.dump_lineinfo(entry:TDwarfEntry; ParseSiblings: Boolean);
var
  line    : TLineInfoStateMachine;
  size    : Int64;
  ofs     : Integer;
const
  debug_line = '.debug_line';

begin
  if not assigned(entry) then Exit;
  if not fSource.GetSectionInfo(debug_line, size) then begin
    writeln('line info is not found!');
    Exit;
  end;

  line:=TLineInfoStateMachine.Create;
  SetLength(line.Data, size);
  fSource.GetSectionData(debug_line, 0, size, line.Data);
  repeat
    if (entry.Tag=DW_TAG_compile_unit) and entry.GetInt32(DW_AT_stmt_list, ofs) then begin
      writeln('line info at $', HexStr(ofs, 8));
      line.Reset(ofs);
      while line.NextLine do begin
        writeln('line: ', line.Line,'  addr: ', HexStr(line.Address, sizeof(PtrUint)*2),' filename: ', line.FileNameId );
      end;
    end;
    entry:=entry.Next;
  until not Assigned(entry) or not ParseSiblings;

  line.Free;
end;

procedure TDbgDwarf3Info.dump_variables(entry:TDwarfEntry);
var
  dw  : LongWord;
begin
  if not Assigned(entry) then Exit;

  if entry.Tag=DW_TAG_variable then begin
    writeln('name      = ', DwarfName(entry));
    //todo:!
  end;
  dump_variables(entry.Child);
  dump_variables(entry.Next);
end;

procedure TDbgDwarf3Info.ReadCompileUnit(entry:TDwarfEntry; var sym: TDbgSymbol);
var
  ofs32 : Integer;
  ofs   : QWord;
  name  : AnsiString;
begin
  // reading special compiled unit attributes
  if entry.GetStr(DW_AT_name, name) then begin
    curfile:=fDbgInfo.AddFile(name);
  end else
    curfile:=nil;

  if not entry.GetInt32(DW_AT_stmt_list, ofs32) then Exit;
  ofs:=ofs32;
  lineinfo.Reset(ofs);

  //todo: dfile search by filename
  while lineinfo.NextLine do begin
    if Assigned(curfile) and (lineinfo.Address<>0) then
      curfile.AddLineInfo( lineinfo.Address, lineinfo.Line);
  end;
  sym:=curfile;
end;

procedure TDbgDwarf3Info.ReadSubPrograms(entry:TDwarfEntry; var sym: TDbgSymbol);
var
  f : TDbgSymFunc;
begin
  f:=fDbgInfo.AddSymbolFunc( DwarfName(entry),  parent);
  sym:=f;
end;

procedure TDbgDwarf3Info.ReadDwarfEntry(entry:TDwarfEntry);
var
  sub   : TDwarfEntry;
  loop  : Boolean;
  par   : TDbgSymbol;
  sym   : TDbgSymbol;
begin
  loop:=True;
  sym:=nil;
  case entry.Tag of
    DW_TAG_compile_unit: ReadCompileUnit(entry, sym);
    DW_TAG_subprogram: ReadSubPrograms(entry, sym);
  end;
  if loop then begin
    if Assigned(sym) then begin
      par:=parent;
      parent:=sym;
    end;

    sub:=entry.Child;
    while Assigned(sub) do begin
      ReadDwarfEntry(sub);
      sub:=sub.Next;
    end;
    if assigned(sym) then parent:=par;
  end;
end;

procedure TDbgDwarf3Info.dump_debug_info2; 
var
  dwarf   : TDwarfReader;
  size    : Int64;
begin
  dwarf := TDwarfReader.Create;

  if not fSource.GetSectionInfo('.debug_info', size) then begin
    writeln('no .debug_info section');
    Exit;
  end;
  
  if fSource.GetSectionInfo('.debug_info', dwarf.InfoSize) then begin
    SetLength(dwarf.Info, size);
    fSource.GetSectionData('.debug_info', 0, dwarf.InfoSize, dwarf.Info);
  end else
    writeln('no debug info');
  
  if fSource.GetSectionInfo('.debug_abbrev', dwarf.AbbrevsSize) then begin
    SetLength(dwarf.Abbrevs, dwarf.AbbrevsSize);
    fSource.GetSectionData('.debug_abbrev', 0, dwarf.AbbrevsSize, dwarf.Abbrevs);
  end else
    writeln('no debug abbrev');

  dwarf.ReadDwarf;
  WriteEntry(dwarf.FirstEntry, '');

  if DebugDwarf_LineInfo then dump_lineinfo(dwarf.FirstEntry);
  if DebugDwarf_Variables then dump_variables(dwarf.FirstEntry);

  dwarf.Free;
end;

initialization
  RegisterDebugInfo(TDbgDwarf3Info);

end.

