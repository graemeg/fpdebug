program stabsreader;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, dbgInfoTypes, dbgInfoStabs, stabs,
  machoDbgSource, PESource, stabsproc;

procedure ReadStabsData(source : TDbgDataSource);
var
  stabs : TDbgStabsInfo;
begin
  if not TDbgStabsInfo.isPresent(source) then begin
    writeln('stabs debug data is not present');
    Exit;
  end;
  stabs := TDbgStabsInfo.Create(source);

  writeln('stabs data found');
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

