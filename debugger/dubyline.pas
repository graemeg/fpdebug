program dubyline;

{$ifdef fpc}
{$mode delphi}{$H+}
{$else}
{$apptype CONSOLE}
{$endif}

uses
  dbgTypes,
  dbgInfoTypes,
  cmdloop,
  commands,
  memviewer,
  PESource,
  dbgInfoStabs
  {$ifdef darwin},macDbgType, macdbgproc {$endif}
  {$ifdef mswindows},winDbgTypes {$endif};

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
