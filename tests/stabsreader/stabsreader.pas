program stabsreader;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes,
  dbgInfoTypes, dbgInfoStabs, stabs,
  elfDbgSource,
  machoDbgSource, PESource, stabsproc;

procedure ReadStabsData(source : TDbgDataSource);
var
  stabs : TDbgStabsInfo;
  info  : TDbgInfo;
begin
  if not TDbgStabsInfo.isPresent(source) then begin
    writeln('stabs debug data is not present');
    Exit;
  end;
  writeln('stabs data found');
  writeln('reading stabs...');
  stabs := TDbgStabsInfo.Create;
  info := TDbgInfo.Create;
  try
    stabs.ReadDebugInfo(source, info);
  finally
    info.Free;
  end;

  writeln('stabs read.');  

  stabs.dump_symbols;
  stabs.Free;
end;

var
  dbgInfoSrc : TDbgDataSource;

{$R *.res}

procedure HandleParams;
var
  i : Integer;
  s : string;
begin
  for i:=1 to ParamCount do begin
    s:=AnsiLowerCase(ParamStr(i));
    if s='-str' then begin
      DebugDumpStabs:=True
    end else if s='-noparse' then begin
      DebugParseStabs:=False;
    end;
  end;

end;

var
  nm : string;
begin
  if Paramcount < 1 then begin
    writeln('please specify executable file name');
    writeln;
    writeln('  stabsreader [switches] executable_file_name');
    writeln;
    writeln;
    writeln('optional switches:');
    writeln('  -str     - writes out stab strings before parsed information.');
    writeln('  -noparse - doesn''t write parsed stabs information');
    Exit;
  end;

  HandleParams;

  nm:=ParamStr(ParamCount);

  dbgInfoSrc := GetDataSource(nm);
  if not Assigned(dbgInfoSrc) then begin
    writeln('file '+ nm + ' is of unknown format');
    Exit;
  end;
  writeln('File format: ', dbgInfoSrc.UserName);

  ReadStabsData(dbgInfoSrc);

  dbgInfoSrc.Free;
end.

