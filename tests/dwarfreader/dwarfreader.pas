program dwarfreader;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  SysUtils, Classes, dbgInfoTypes, dbgInfoDwarf,
  // include file types we can read/support
  elfDbgSource,
  PESource,
  machoDbgSource;

procedure ReadDwarfData(source : TDbgDataSource);
var
  dwarf : TDbgDwarf3Info;
  info: TDbgInfo;
begin
  if not TDbgDwarf3Info.isPresent(source) then begin
    writeln('dwarf debug data is not present');
    Exit;
  end;
  dwarf := TDbgDwarf3Info.Create;
  info := TDbgInfo.Create;
  try
    dwarf.ReadDebugInfo(source, info);
  finally
    info.Free;
  end;


  writeln('dwarf data found');

  writeln('dumping: debug_info');
  dwarf.dump_debug_info2;

  dwarf.Free;
end;

function GetExternalDebugInfo(const FileName: String): string;
{$ifdef DARWIN}
const
  DwarfPath : String = '/Contents/Resources/DWARF/';
begin
  Result:=ChangeFileExt(FileName, '.dSYM')+DwarfPath+ExtractFileName(FileName);
end;
{$else}
begin
  Result:='';
end;
{$endif}


var
  dbgInfoSrc : TDbgDataSource;
  info : AnsiString;
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

  info:=GetExternalDebugInfo(ParamStr(1));
  if info<>'' then begin
    dbgInfoSrc := GetDataSource(info);
    if not Assigned(dbgInfoSrc) then begin
      writeln('file '+ info+ ' is of unknown format');
      Exit;
    end;
    writeln('File format: ', dbgInfoSrc.UserName);

    ReadDwarfData(dbgInfoSrc);
    dbgInfoSrc.Free;
  end;
end.

