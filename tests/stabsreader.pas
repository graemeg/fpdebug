program stabsreader;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, dbgInfoTypes, dbgInfoStabs, stabs,
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
  
  stabs.dump_symbols;

  stabs.Free;
end;

var
  dbgInfoSrc : TDbgDataSource;
begin
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

