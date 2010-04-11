program dwarfreader;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, dbgInfoTypes, dbgInfoDwarf,
  PESource, machoDbgSource;


procedure ReadDwarfData(source : TDbgDataSource);
var
  dwarf : TDbgDwarf3Info;
begin
  if not TDbgDwarf3Info.isPresent(source) then begin
    writeln('dwarf debug data is not present');
    Exit;
  end;
  dwarf := TDbgDwarf3Info.Create(source);


  writeln('dwarf data found');

  writeln('dumping: debug_info');
  dwarf.dump_debug_info2;

  dwarf.Free;
end;

var
  dbgInfoSrc : TDbgDataSource;

begin
  if Paramcount < 1 then begin
    writeln('please specify dwarf debug-info file name');
    Exit;
  end;
  dbgInfoSrc := GetDataSource(ParamStr(1));
  if not Assigned(dbgInfoSrc) then begin
    writeln('file '+ ParamStr(1)+ ' is of unknow format');
    Exit;
  end;
  writeln('File format: ', dbgInfoSrc.UserName);

  ReadDwarfData(dbgInfoSrc);

  dbgInfoSrc.Free;
end.

