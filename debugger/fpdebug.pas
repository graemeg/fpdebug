{
    fpDebug  -  A debugger for the Free Pascal Compiler.

    Copyright (c) 2012 by Graeme Geldenhuys.

    See the file LICENSE.txt, included in this distribution,
    for details about redistributing fpDebug.

    Description:
      .
}
program fpdebug;

{$mode objfpc}{$H+}
{$ifdef windows}
  {$apptype CONSOLE}
{$endif}

uses
  SysUtils,
  dbgTypes,

  dbgDataRead,
  dbgFPCDataRead,

  dbgInfoTypes,
  dbgInfoPascal,
  dbgInfoDwarf,  // dwarf debug information
  dbgInfoStabs,  // stabs debug information

  PESource,        // Win (PE), Linux/FreeBSD (elf), MacOSX (macho) executable files
  elfdbgsource,    // the implementation is cross-platform, so there's no need
  machoDbgSource,  // to cover them in {$ifdefs}

  // command line debugger
  commands,
  cmdloop,    // main loop and run commands
  cmdmemview, // view commands
  cmddbg,     // debug-info commands

  // turn the unit off, if you don't want to debug debug-info loading
  cmdDbgEx

  {$ifdef mswindows},winDbgTypes{$endif}          // windows debug API
  {$ifdef darwin},macDbgType{$endif} // macosx debug API
  {$ifdef linux},nixDbgTypes{$endif}              // linux debug API
  {$ifdef freebsd},nixDbgTypes{$endif}            // freebsd debug API

  {$ifdef CPUI386}  ,dbgi386{$endif} // i386 CPU routines
  {$ifdef CPUx86_64},dbgi386{$endif}  //  x64 CPU routines
  ,dbgMain
  ;

function FixFileName(const FileName: string): string;
begin
  Result:=FileName;
  if not FileExists(FileName) then
  begin
    Result:=FileName+'.exe';
    if not FileExists(Result) then
      Result:=FileName;
  end;
end;

procedure RunDebugger;
var
  dbg  : TDbgTarget;
  lDebugger : TDbgMain;
  cmd  : String;
begin
  cmd := FixFileName(ParamStr(1));
  if cmd = '' then
  begin
    writeln('executable is not specified');
    Exit;
  end
  else
    writeln('debugging process: ', cmd);

  if not FileExists(cmd) then
  begin
    writeln('file doesn''t exists or not available: <',cmd,'>');
    Exit;
  end;
  LoadDebugInfo(cmd);

  dbg := DebugProcessStart(cmd);
  lDebugger := TDbgMain.Create(dbg, 0, True);
  try
    if not Assigned(dbg) then
    begin
      writeln('cannot start debug process');
      Exit;
    end;
    RunLoop(lDebugger);
  finally
    lDebugger.Free;
  end;
end;

begin
  try
    RunDebugger;
  except
    on E: Exception do
      begin
        writeln('ERROR:');
        writeln(E.Message);
        writeln('');
      end;
  end;
end.

