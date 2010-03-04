program stabsreader;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, dbgInfoTypes, dbgInfoStabs, stabs,
  elfDbgSource,
  machoDbgSource, PESource, stabsproc;

function GetFileString(const Name: WideString; LineNum: Integer): String;
var
  st  : TStringList;
begin
  st := TStringList.Create;
  st.LoadFromFile(Name);
  dec(LineNum);
  if (LineNum >= 0) and (LineNum < st.Count) then
    Result := st[LineNum];
  st.Free;
end;  
  
procedure ReadStabsData(source : TDbgDataSource);
var
  stabs : TDbgStabsInfo;

  addr  : Integer;
  nm    : WideString;
  i     : LongWord;

  names : TStringList;
begin
  if not TDbgStabsInfo.isPresent(source) then begin
    writeln('stabs debug data is not present');
    Exit;
  end;
  writeln('stabs data found');
  writeln('reading stabs...');
  stabs := TDbgStabsInfo.Create(source);
  writeln('stabs read.');  

  {writeln('type addr. 0 - to exit');
  ReadLn(addr);
  while addr <> 0 do begin
    if stabs.GetLineByAddr(addr, nm, i) then begin
      writeln(GetFileString(nm, i));
    end else 
      writeln('not found');
    
    ReadLn(addr);
  end;}

{  names := TStringList.Create;
  stabs.GetNames(names, '');
  for i := 0 to names.Count - 1 do  writeln(names[i]);
  names.Free;
}

  stabs.dump_symbols;

  stabs.Free;
end;

procedure StabsParsing;
var
  ststr   : string;
  name    : string;
  mdstr   : string;
  mdnum   : Integer;
  value   : string;
  typeval : string;

  structSize : Integer;
  elemIndex  : Integer;

  bitofs     : Integer;
  bitsize    : Integer;
begin
  ststr := 'SHORTSTRING:Tt6=s256length:1,0,8;st:ar1;1;255;8,8,2040;;';
  ParseStabStr(ststr, name, mdstr, mdnum, value);
  writeln('str      = "', ststr,'"');
  writeln('name     = "', name,'"');
  writeln('modifier = "', mdstr,'"');
  writeln('mod num  = "', mdnum,'"');
  writeln('value    = "', value,'"');

  if isStructType(value, StructSize, elemIndex) then begin
    writeln('struct size = ', structSize);
    writeln('first index = ', elemIndex);
    writeln('length(value) = ', length(value));
    while GetStructElem(value, elemIndex, name, typeval, bitofs, bitsize, elemIndex) do
      writeln('  ', name, ' type = "', typeval, '"; ofs = ', bitofs, ' size = ', bitsize);
  end else
    writeln('not struct!');
  Halt(0);
end;

var
  dbgInfoSrc : TDbgDataSource;
begin
  //StabsParsing;

  if Paramcount < 1 then begin
    writeln('please specify stabs debug-info file name');
    Exit;
  end;
  dbgInfoSrc := GetDataSource(ParamStr(1));
  if not Assigned(dbgInfoSrc) then begin
    writeln('file '+ ParamStr(1)+ ' is of unknow format');
    Exit;
  end;
  writeln('File format: ', dbgInfoSrc.UserName);

  ReadStabsData(dbgInfoSrc);

  dbgInfoSrc.Free;
end.

