
program dubyline;

{$ifdef fpc}
{$mode delphi}{$H+}
{$else}
{$apptype CONSOLE}
{$endif}

uses
  dbgTypes,

  dbgInfoTypes,
  dbgInfoDwarf,  // dwarf debug information
  dbgInfoStabs,  // stabs debug information
  
  dbgBreakPoints,  // break points manager
  
  PESource,        // Win (PE), Linux (elf), MacOSX (macho) executable files
  elfdbgsource,    // the implementation is cross-platform, so there's no need
  machoDbgSource,  // to cover them in {$ifdefs}
  
  // command line debugger
  commands,
  cmdloop,    // main loop and run commands
  memviewer,  // view commands
  cmddbg      // debug-info commands
  
  {$ifdef mswindows},winDbgTypes{$endif}          // windows debug API
  {$ifdef darwin},macDbgType, macDbgProc {$endif} // macosx debug API
  {$ifdef linux},nixDbgTypes{$endif}              // linux debug API
  
  {$ifdef CPUI386},dbgi386{$else} // i386 CPU routines
  {$error Target platform is not supported for debugging}
  {$endif}
  ;

 
procedure RunDebugger;
var
  dbg : TDbgProcess;
  cmd : String;
begin
  cmd := ParamStr(1);
  if cmd = '' then begin
    writeln('executable is not specified');
    Exit;
  end else
    writeln('debugging process: ', cmd);

  LoadDebugInfo(cmd);
    
  dbg := DebugProcessStart(cmd);
  if not Assigned(dbg) then begin
    writeln('cannot start debug process');
    Exit;
  end;
  
  try
    try
      //SetCommandLine(cmd);
      RunLoop(dbg);
    finally
      dbg.Free;
    end;
  except
    writeln('main loop exception');
  end;
end;

begin
  try
    RunDebugger;
  except
    writeln('exception while debugging');
  end;
end.

