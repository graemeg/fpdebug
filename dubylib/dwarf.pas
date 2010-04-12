{
 ---------------------------------------------------------------------------
 dwarf.pas  -   Dwarf symbol reader
 ---------------------------------------------------------------------------

 This unit contains helper classes for loading and resolving of DWARF debug
 symbols. It's based on Marc Weustink's dwarf reader unit part of FPDebugger

 ***************************************************************************
 *                                                                         *
 *   This source is free software; you can redistribute it and/or modify   *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This code is distributed in the hope that it will be useful, but      *
 *   WITHOUT ANY WARRANTY; without even the implied warranty of            *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
 *   General Public License for more details.                              *
 *                                                                         *
 *   A copy of the GNU General Public License is available on the World    *
 *   Wide Web at <http://www.gnu.org/copyleft/gpl.html>. You can also      *
 *   obtain it by writing to the Free Software Foundation,                 *
 *   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.        *
 *                                                                         *
 ***************************************************************************

 ---------------------------------------------------------------------------}

unit dwarf;

interface

uses
  dwarfConst, dwarfTypes;

type
  TDwarfEntry = class;
  
  TDwarfAttrValue = record
  case byte of
    0 : (i32 : Integer);
    1 : (i64 : Int64);
    3 : (u32 : LongWord);
    4 : (u64 : UInt64);
    5 : (b   : Boolean);    
    6 : (die : TDwarfEntry);
  end;
  
  TDwarfAttr = record
    Name     : Integer;
    Value    : TDwarfAttrValue; //todo: TDbgPtr
  end;
  
  TDwarfEntry = class(TObject) {Debugging Information Entry aka DIE}
  public
    Child     : TDwarfEntry;
    Next      : TDwarfEntry;  
    Parent    : TDwarfEntry;  
  
    Tag       : Integer;   
  end;
  
type
  { TAbbrevTable }
  TAbbrevTable = class(TObject)
  private
    FVerbose  : Boolean;
    DefFilled : array of Boolean;
  protected
    procedure DoReadAbbrev(const AData: array of byte; Offset, ADataSize: Integer);
  public
    FDefCount     : Integer;
    FDefinitions  : array of record
      Attribute   : Cardinal;
      Form        : Cardinal;
    end;
    Def   : array of TDwarfAbbrev;
    constructor Create(const Abbrev: array of byte; Offset,Size: Integer);  
    function GetDefintion(AbbrevNum: Integer; var Abbrev: TDwarfAbbrev): Boolean;
  end;
  
  { TDwarfReader }

  TDwarfReader = class(TObject)
  protected
    procedure ReadEntries(InfoOfs, InfoSize, AbbrevOfs: QWord; AddrSize: Byte);    
  public
    FirstEntry    : TDwarfEntry;
    
    Info          : array of byte;
    InfoSize      : Int64;
    
    Abbrevs       : array of byte;
    AbbrevsSize   : Int64;
    
    function ReadDwarf: Boolean;
  end;

implementation

type
  TDwarfHeader = record
    hdr32 :  TDwarfCUHeader32;
    hdr64 :  TDwarfCUHeader64;
  end;
  PDwarfHeader =  ^TDwarfHeader;


{ TDwarfReader }

procedure TDwarfReader.ReadEntries(InfoOfs, InfoSize, AbbrevOfs: QWord; AddrSize: Byte); 
var
  abbr  : TAbbrevTable;

  procedure SkipLEB(var p: Pointer);
  begin
    while (PByte(p)^ and $80) <> 0 do begin
      Inc(p);
    end;
    Inc(p);
  end;

  procedure SkipStr(var p: Pointer);
  begin
    while PByte(p)^ <> 0 do Inc(p);
    Inc(p);
  end;
  
  procedure ParseAttribs(const ADef: TDwarfAbbrev; ABuildList: Boolean; var ofs: Integer);
  var
    idx: Integer;
    Form: Cardinal;
    UValue: QWord;
    p: Pointer;
  begin
    for idx := ADef.Index to ADef.Index + ADef.Count - 1 do
    begin
      {if ABuildList
      then AList[idx - ADef.Index] := p;}

      Form := abbr.FDefinitions[idx].Form;
      while Form = DW_FORM_indirect do  
        Form := ULEB128toOrdinal(Info, ofs);
      
      case Form of
        DW_FORM_addr     : begin
          Inc(ofs, AddrSize);
        end;
        DW_FORM_block    : begin
          UValue := ULEB128toOrdinal(Info, ofs);
          Inc(ofs, UValue);
        end;
        DW_FORM_block1   : begin
          Inc(ofs, PByte(@Info[ofs])^ + 1);
        end;
        DW_FORM_block2   : begin
          Inc(ofs, PWord(@Info[ofs])^ + 2);
        end;
        DW_FORM_block4   : begin
          Inc(ofs, PLongWord(@Info[ofs])^ + 4);
        end;
        DW_FORM_data1    : begin
          Inc(ofs, 1);
        end;
        DW_FORM_data2    : begin
          Inc(ofs, 2);
        end;
        DW_FORM_data4    : begin
          Inc(ofs, 4);
        end;
        DW_FORM_data8    : begin
          Inc(ofs, 8);
        end;
        DW_FORM_sdata    : begin
          p:=@Info[ofs];//todo: change
          SkipLEB(p);
          inc(ofs, p-@Info[ofs]);          
        end;
        DW_FORM_udata    : begin
          p:=@Info[ofs];//todo: change
          SkipLEB(p);
          inc(ofs, p-@Info[ofs]);          
        end;
        DW_FORM_flag     : begin
          Inc(ofs, 1);
        end;
        DW_FORM_ref1     : begin
          Inc(ofs, 1);
        end;
        DW_FORM_ref2     : begin
          Inc(ofs, 2);
        end;
        DW_FORM_ref4     : begin
          Inc(ofs, 4);
        end;
        DW_FORM_ref8     : begin
          Inc(ofs, 8);
        end;
        DW_FORM_ref_udata: begin
          p:=@Info[ofs];//todo: change
          SkipLEB(p);
          inc(ofs, p-@Info[ofs]);          
        end;
        DW_FORM_ref_addr : begin
          Inc(ofs, AddrSize);
        end;
        DW_FORM_string   : begin
          p:=@Info[ofs];//todo: change
          SkipStr(p);
          inc(ofs, p-@Info[ofs]);          
        end;
        DW_FORM_strp     : begin
          Inc(ofs, AddrSize);
        end;
        DW_FORM_indirect : begin
        end;
      else
        Break;
      end;
    end;
  end;

var
  i       : Integer;
  Abbrev  : Cardinal;
  Def     : TDwarfAbbrev;
  MaxData   : QWord;
  BuildList : Boolean; // set once if we need to fill the list
  parent    : TDwarfEntry;
  prev      : TDwarfEntry;
  newdw     : TDwarfEntry;
  t   : TDwarfEntry;
begin
  abbr:=TAbbrevTable.Create(Abbrevs, AbbrevOfs, AbbrevsSize);  
  
  BuildList := False;
  parent:=nil; 
  prev:=nil;
  MaxData :=  InfoOfs + InfoSize;
  //Scope := AStartScope;
  //p := Scope.Entry;
  i:=InfoOfs;
  while (i <= MaxData) do begin
    Abbrev := ULEB128toOrdinal(Info, i);
    
    if Abbrev = 0 then begin  // children are complete
      prev:=parent;
      if Assigned(prev) then 
        parent:=prev.Parent
      else
        parent:=nil;
      Continue
    end else begin
      
      if not abbr.GetDefintion(Abbrev, Def)  then begin
        WriteLn('Error: Abbrev not found: ', Abbrev,' i=',i);
        Break;
      end;
      
      ParseAttribs(Def, BuildList, i);
  
      newdw:=TDwarfEntry.Create;
      if Assigned(prev) then prev.Next:=newdw;
      newdw.Parent:=newdw;
      if Assigned(Parent) and not Assigned(Parent.Child) then 
        Parent.child:=newdw;
      newdw.Tag:=Def.tag; 
      
      if not Assigned(Parent) then begin
        if not Assigned(FirstEntry) then 
          FirstEntry:=newdw
        else begin
          t:=FirstEntry;
          while Assigned(t.Next) do t:=t.Next;
          t.Next:=newdw;
        end;
      end;
      
      // Def.children can be set while no children are found
      // we cannot have a next without a defined child
      if Def.Children then begin
        parent:=newdw;
        prev:=nil;
      end else begin
        prev:=newdw;
      end;
    end;
  end;
end;
  
function TDwarfReader.ReadDwarf: Boolean; 
var
  header  : PDwarfHeader;
  i       : Integer;
  addrsz  : Byte;
  abbrofs : int64;
  len     : QWord;
begin
  Result:=InfoSize>0;
  if not Result then Exit;
  
  writeln('Info Size    = ', InfoSize);
  writeln('Abbrevs Size = ', AbbrevsSize);
  
  i:=0;
  while i<InfoSize do begin
    header:=@Info[i];
    if header^.hdr32.AbbrevOffset=$FFFFFFFF then begin
      addrsz:=header^.hdr64.AddressSize;
      abbrofs:=header^.hdr64.AbbrevOffset;
      len:=header^.hdr64.Length-(sizeof(header^.hdr64)-12);
      inc(i,sizeof(header^.hdr64)); // skipping 64-bit length
    end else begin
      addrsz:=header^.hdr32.AddressSize;
      abbrofs:=header^.hdr32.AbbrevOffset;
      len:=header^.hdr32.Length-(sizeof(header^.hdr32)-4);
      inc(i, sizeof(header^.hdr32));
    end;
    ReadEntries(i, len, abbrofs, addrsz);    
    inc(i, len); // skipping the data
  end;
end;

{ TAbbrevTable }

procedure TAbbrevTable.DoReadAbbrev(const AData: array of byte; Offset,  
  ADataSize: Integer); 

  procedure MakeRoom(AMinSize: Integer);
  var
    len: Integer;
  begin
    len := Length(FDefinitions);
    if len > AMinSize then Exit;
    if len > $4000
    then Inc(len, $4000)
    else begin
      if len=0 then len:=$40
      else len := len * 2;
    end;
    SetLength(FDefinitions, len);
  end;

var
  MaxData : Integer;
  abbrev  : LongWord;
  attrib  : LongWord;
  tagnum  : LongWord;
  form    : Cardinal;
  n       : Integer;
  i       : Integer;
  FAbbrevIndex  : Integer;
begin
  FAbbrevIndex:=0;

  abbrev := 0;
  // we don't know the number of abbrevs for this unit,
  // but we cannot go beyond the section limit, so use that as safetylimit
  // in case of corrupt data
  i:=Offset;
  MaxData:=ADataSize;
  while (i < MaxData) and (AData[i] <> 0) do begin
    abbrev := ULEB128toOrdinal(AData, i);
    tagnum := ULEB128toOrdinal(AData, i);
    
    while length(Def)<=abbrev do begin
      if length(Def)= 0 then begin
        SetLength(Def, 8);
        SetLength(DefFilled, 8);
      end else begin
        SetLength(Def, length(Def)*2);
        SetLength(DefFilled, length(Def)*2)
      end;
    end;

    if DefFilled[abbrev] then begin
      WriteLN('Duplicate abbrev=', abbrev, ' found. Ignoring....');
      while PWord(@AData[i])^ <> 0 do Inc(i, 2);
      Inc(i, 2);
      Continue;
    end;
    DefFilled[abbrev]:=True;
    

    Def[abbrev].tag := tagnum;
    if FVerbose then begin
      WriteLN('  abbrev:  ', abbrev);
      WriteLN('  tag:     ', Def[abbrev].tag, '=', DwarfTagToString(Def[abbrev].tag));
      WriteLN('  children:', AData[i], '=', DwarfChildrenToString(AData[i]));
    end;
    Def[abbrev].Children := AData[i] = DW_CHILDREN_yes;
    Inc(i);

    n := 0;
    Def[abbrev].Index := FAbbrevIndex;
    while PWord(@AData[i])^ <> 0 do
    begin
      attrib := ULEB128toOrdinal(AData, i);
      form := ULEB128toOrdinal(AData, i);

      MakeRoom(FAbbrevIndex + 1);
      FDefinitions[FAbbrevIndex].Attribute := attrib;
      FDefinitions[FAbbrevIndex].Form := form;
      Inc(FAbbrevIndex);
      if FVerbose then 
        WriteLN('   [', n:4, '] attrib: ', attrib, '=', DwarfAttributeToString(attrib), ', form: ', form, '=', DwarfAttributeFormToString(form));
      Inc(n);
    end;
    Def[abbrev].Count := n;
    inc(i, 2);
  end;
end;

constructor TAbbrevTable.Create(const Abbrev: array of byte; Offset,  
  Size: Integer); 
begin
  inherited Create;
  FVerbose:=False;
  DoReadAbbrev(Abbrev, Offset, Size);
end;

function TAbbrevTable.GetDefintion(AbbrevNum: Integer; var Abbrev: TDwarfAbbrev): Boolean;
begin
  Result:=(AbbrevNum>=0) and (AbbrevNum<length(DefFilled)) and DefFilled[AbbrevNum];
  if Result then Abbrev:=Def[AbbrevNum];
end;

end.
