{
    fpDebug  -  A debugger for the Free Pascal Compiler.

    Copyright (c) 2012 by Graeme Geldenhuys.

    See the file LICENSE.txt, included in this distribution,
    for details about redistributing fpDebug.

    Description:
      .
}
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
    procedure ReadVar(entry: TDwarfEntry; var sym: TDbgSymbol);
    procedure ReadDwarfEntry(entry: TDwarfEntry);

    procedure dump_lineinfo(entry: TDwarfEntry; ParseSiblings: Boolean=True);
    procedure dump_variables(entry: TDwarfEntry);
  public
    class function isPresent(ASource: TDbgDataSource): Boolean; override;
    constructor Create; override;

    function ReadDebugInfo(ASource: TDbgDataSource; Info: TDbgInfo): Boolean; override;

    procedure dump_debug_info2;
  end;

implementation

const
  _debug_info   = '.debug_info';
  _debug_abbrev = '.debug_abbrev';
  _debug_line   = '.debug_line';

//todo: incomplete support!
//todo: replace TDbgDataPos with dbgInfoTypes
procedure RencodeLocation(AData: PByteArray; ASize: QWord; AddrSizeBytes: Integer; var Pos: TDbgDataPos);
var
  opcode  : Byte;
  i       : Integer;
begin
  i:=0;
  while i < ASize do
  begin
    opcode:=AData[i];
    inc(i);
    case opcode of
      DW_OP_addr: begin
        Pos.Addr:=0;
        Move(AData[i], Pos.Addr, AddrSizeBytes);
        Inc(i, AddrSizeBytes);
      end;
      DW_OP_deref:;
      DW_OP_const1u: Inc(i,1);
      DW_OP_const1s: Inc(i,1);
      DW_OP_const2u: Inc(i,2);
      DW_OP_const2s: Inc(i,2);
      DW_OP_const4u: Inc(i,4);
      DW_OP_const4s: Inc(i,4);
      DW_OP_const8u: Inc(i,8);
      DW_OP_const8s: Inc(i,8);
      DW_OP_constu:  ULEB128toOrdinal(AData^,i);
      DW_OP_consts:  SLEB128toOrdinal(AData^,i);
      DW_OP_dup:  ;
      DW_OP_drop: ;
      DW_OP_over: ;
      DW_OP_pick: inc(i);
      DW_OP_swap: ;
      DW_OP_rot:  ;
      DW_OP_xderef: ;
      DW_OP_abs:;
      DW_OP_and:;
      DW_OP_div:;
      DW_OP_minus:;
      DW_OP_mod:;
      DW_OP_mul:;
      DW_OP_neg:;
      DW_OP_not:;
      DW_OP_or:;
      DW_OP_plus:;
      DW_OP_plus_uconst: ULEB128toOrdinal(AData^, i);
      DW_OP_shl: ;
      DW_OP_shr: ;
      DW_OP_shra: ;
      DW_OP_xor: ;
      DW_OP_skip: Inc(i, 2);
      DW_OP_bra:  Inc(i, 2);
      DW_OP_eq: ;
      DW_OP_ge: ;
      DW_OP_gt: ;
      DW_OP_le: ;
      DW_OP_lt: ;
      DW_OP_ne: ;
      DW_OP_lit0..DW_OP_lit31: begin
        //Write('DW_OP_lit', AData[i] - DW_OP_lit0);
      end;
      DW_OP_reg0..DW_OP_reg31: begin
        //Write('DW_OP_reg', AData[i] - DW_OP_reg0);
      end;

      DW_OP_breg0..DW_OP_breg31: begin
        //Write('DW_OP_breg ', AData[i] - DW_OP_breg0);
        Pos.RegName:=IntToStr(AData[i] - DW_OP_breg0);
        Pos.Addr := SLEB128toOrdinal(AData^, i);

        //todo: this is really stupid hack!
        if Pos.RegName='5' then
          Pos.Location:=ddlFrameRel
        else
          Pos.Location:=ddlFrameRel;
      end;

      DW_OP_regx: begin
        ULEB128toOrdinal(AData^, i);
      end;
      DW_OP_fbreg: begin
        SLEB128toOrdinal(AData^, i);
      end;
      DW_OP_bregx: begin
        ULEB128toOrdinal(AData^,i);
        SLEB128toOrdinal(AData^,i);
      end;
      DW_OP_piece:
        ULEB128toOrdinal(AData^, i);
      DW_OP_deref_size: Inc(i);
      DW_OP_xderef_size: Inc(i);
      DW_OP_nop:;
      DW_OP_push_object_address:;
      DW_OP_call2: Inc(i, 2);
      DW_OP_call4: Inc(i, 4);
      DW_OP_call_ref: Inc(i, AddrSizeBytes);
      DW_OP_form_tls_address: ;
      DW_OP_call_frame_cfa:   ;
      DW_OP_bit_piece: begin
        ULEB128toOrdinal(AData^, i);
        ULEB128toOrdinal(AData^, i);
      end;
      DW_OP_lo_user..DW_OP_hi_user: ;
    else
      // DW_OP_unknown
    end;
  end;
end;

procedure DebugDecodeLocation(AData: PByteArray; ASize: QWord);
var
  v : Int64;
  i : Integer;
begin
  i:=0;
  while i < ASize do
  begin
    Write('  ');
    case AData[i] of
      DW_OP_addr: begin
        Write('DW_OP_addr '{, MakeAddressString(@AData[1])});
        Inc(i, 4);//todo!
      end;
      DW_OP_deref: begin
        Write('DW_OP_deref');
      end;
      DW_OP_const1u: begin
        Write('DW_OP_const1u ', AData[1]);
        Inc(i, 1);
      end;
      DW_OP_const1s: begin
        Write('DW_OP_const1s ', PShortInt(@AData[1])^);
        Inc(i, 1);
      end;
      DW_OP_const2u: begin
        Write('DW_OP_const2u ', PWord(@AData[1])^);
        Inc(i, 2);
      end;
      DW_OP_const2s: begin
        Write('DW_OP_const2s ', PSmallInt(@AData[1])^);
        Inc(i, 2);
      end;
      DW_OP_const4u: begin
        Write('DW_OP_const4u ', PLongWord(@AData[1])^);
        Inc(i, 4);
      end;
      DW_OP_const4s: begin
        Write('DW_OP_const4s ', PLongInt(@AData[1])^);
        Inc(i, 4);
      end;
      DW_OP_const8u: begin
        Write('DW_OP_const8u ', PQWord(@AData[1])^);
        Inc(i, 8);
      end;
      DW_OP_const8s: begin
        Write('DW_OP_const8s ', PInt64(@AData[1])^);
        Inc(i, 8);
      end;
      DW_OP_constu: begin
        Inc(i);
        Write('DW_OP_constu ', ULEB128toOrdinal(AData^,i));
        Dec(i);
      end;
      DW_OP_consts: begin
        Inc(i);
        Write('DW_OP_consts ', SLEB128toOrdinal(AData^,i));
        Dec(i);
      end;
      DW_OP_dup: begin
        Write('DW_OP_dup');
      end;
      DW_OP_drop: begin
        Write('DW_OP_drop');
      end;
      DW_OP_over: begin
        Write('DW_OP_over');
      end;
      DW_OP_pick: begin
        Write('DW_OP_pick ', AData[1]);
        Inc(i, 1);
      end;
      DW_OP_swap: begin
        Write('DW_OP_swap');
      end;
      DW_OP_rot: begin
        Write('DW_OP_rot');
      end;
      DW_OP_xderef: begin
        Write('DW_OP_xderef');
      end;
      DW_OP_abs: begin
        Write('DW_OP_abs');
      end;
      DW_OP_and: begin
        Write('DW_OP_and');
      end;
      DW_OP_div: begin
        Write('DW_OP_div');
      end;
      DW_OP_minus: begin
        Write('DW_OP_minus');
      end;
      DW_OP_mod: begin
        Write('DW_OP_mod');
      end;
      DW_OP_mul: begin
        Write('DW_OP_mul');
      end;
      DW_OP_neg: begin
        Write('DW_OP_neg');
      end;
      DW_OP_not: begin
        Write('DW_OP_not');
      end;
      DW_OP_or: begin
        Write('DW_OP_or');
      end;
      DW_OP_plus: begin
        Write('DW_OP_plus');
      end;
      DW_OP_plus_uconst: begin
        Inc(i);
        Write('DW_OP_plus_uconst ', ULEB128toOrdinal(AData^, i));
        Dec(AData);
      end;
      DW_OP_shl: begin
        Write('DW_OP_shl');
      end;
      DW_OP_shr: begin
        Write('DW_OP_shr');
      end;
      DW_OP_shra: begin
        Write('DW_OP_shra');
      end;
      DW_OP_xor: begin
        Write('DW_OP_xor');
      end;
      DW_OP_skip: begin
        Write('DW_OP_skip ', PSmallInt(@AData[1])^);
        Inc(i, 2);
      end;
      DW_OP_bra: begin
        Write('DW_OP_bra ', PSmallInt(@AData[1])^);
        Inc(i, 2);
      end;
      DW_OP_eq: begin
        Write('DW_OP_eq');
      end;
      DW_OP_ge: begin
        Write('DW_OP_ge');
      end;
      DW_OP_gt: begin
        Write('DW_OP_gt');
      end;
      DW_OP_le: begin
        Write('DW_OP_le');
      end;
      DW_OP_lt: begin
        Write('DW_OP_lt');
      end;
      DW_OP_ne: begin
        Write('DW_OP_ne');
      end;
      DW_OP_lit0..DW_OP_lit31: begin
        Write('DW_OP_lit', AData[i] - DW_OP_lit0);
      end;
      DW_OP_reg0..DW_OP_reg31: begin
        Write('DW_OP_reg', AData[i] - DW_OP_reg0);
      end;
      DW_OP_breg0..DW_OP_breg31: begin
        Write('DW_OP_breg ', AData[i] - DW_OP_breg0);
        Inc(i);
        v := SLEB128toOrdinal(AData^, i);
        Dec(AData);
        if v >= 0 then Write('+');
        Write(v);
      end;
      DW_OP_regx: begin
        Inc(i);
        Write('DW_OP_regx ', ULEB128toOrdinal(AData^, i));
        Dec(i);
      end;
      DW_OP_fbreg: begin
        Inc(i);
        Write('DW_OP_fbreg ', SLEB128toOrdinal(AData^, i));
        Dec(i);
      end;
      DW_OP_bregx: begin
        Inc(AData);
        Write('DW_OP_bregx ', ULEB128toOrdinal(AData^,i));
        v := SLEB128toOrdinal(AData^,i);
        Dec(i);
        if v >= 0
        then Write('+');
        Write(v);
      end;
      DW_OP_piece: begin
        Inc(i);
        Write('DW_OP_piece ', ULEB128toOrdinal(AData^, i));
        Dec(i);
      end;
      DW_OP_deref_size: begin
        Write('DW_OP_deref_size ', AData[1]);
        Inc(i);
      end;
      DW_OP_xderef_size: begin
        Write('DW_OP_xderef_size', AData[1]);
        Inc(i);
      end;
      DW_OP_nop: begin
        Write('DW_OP_nop');
      end;
      DW_OP_push_object_address: begin
        Write('DW_OP_push_object_address');
      end;
      DW_OP_call2: begin
        Write('DW_OP_call2 ', PWord(@AData[1])^);
        Inc(i, 2);
      end;
      DW_OP_call4: begin
        Write('DW_OP_call4 ', PLongWord(@AData[1])^);
        Inc(i, 4);
      end;
      DW_OP_call_ref: begin
        Write('DW_OP_call_ref '{, MakeAddressString(@AData[1])});
        Inc(i, 4);
      end;
      DW_OP_form_tls_address: begin
        Write('DW_OP_form_tls_address');
      end;
      DW_OP_call_frame_cfa: begin
        Write('DW_OP_call_frame_cfa');
      end;
      DW_OP_bit_piece: begin
        Inc(i);
        Write('DW_OP_bit_piece ', ULEB128toOrdinal(AData^, i), ' ', ULEB128toOrdinal(AData^, i));
        Dec(i);
      end;
      DW_OP_lo_user..DW_OP_hi_user: begin
        Write('DW_OP_user=', AData[i]);
      end;
    else
      Write('Unknown DW_OP_', AData[i]);
    end;
    Inc(i);
    WriteLn;
  end;
end;


{ TDbgDwarf3Info }

class function TDbgDwarf3Info.isPresent(ASource: TDbgDataSource): Boolean;
var
  sz  : Int64;
begin
  Result := Assigned(ASource) and (ASource.GetSectionInfo(_debug_info, sz)) and (sz > 0);
end;

constructor TDbgDwarf3Info.Create;
begin
  inherited Create;
end;

function TDbgDwarf3Info.ReadDebugInfo(ASource: TDbgDataSource; Info: TDbgInfo): Boolean;
var
  size    : Int64;
  reader  : TDwarfReader;
  entry   : TDwarfEntry;
begin
  fSource:=ASource;
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
  buf : array of byte;
begin
  if not Assigned(entry) then Exit;

  if entry.Tag in [DW_TAG_variable, DW_TAG_formal_parameter] then begin
    writeln('name      = ', DwarfName(entry));
    SetLength(buf, entry.GetAttrSize(DW_AT_location));
    if length(buf)>0 then begin
      writeln('  data size = ', length(buf));
      entry.GetAttrData(DW_AT_location, buf[0], length(buf));
      DebugDecodeLocation(@buf[0], length(buf));
    end;

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

procedure TDbgDwarf3Info.ReadVar(entry: TDwarfEntry; var sym: TDbgSymbol);
var
  nm  : AnsiString;
  v   : TDbgSymVar;
  buf : array of byte;
begin
  nm:=DwarfName(entry);
  v:=fDbgInfo.AddSymbol(nm, parent, TDbgSymVar) as TDbgSymVar;

  SetLength(buf, entry.GetAttrSize(DW_AT_location));
  if length(buf)>0 then begin
    entry.GetAttrData(DW_AT_location, buf[0], length(buf));
    //todo: use sizeof Target ptr instread of TDbgPtr
    RencodeLocation(@buf[0], length(buf), SizeOf(TDbgPtr), v.DataPos);
  end;
  sym:=v;
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
    DW_TAG_variable, DW_TAG_formal_parameter:
      ReadVar(entry, sym);
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

