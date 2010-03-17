program stabsreader;

{$mode objfpc}{$H+}

uses
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

  stabs.dump_symbols;
  stabs.Free;
end;

var
  dbgInfoSrc : TDbgDataSource;

{$R *.res}

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
