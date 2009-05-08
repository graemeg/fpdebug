program fpdebugger;

{$mode objfpc}{$H+}

uses
  dbgTypes
  {$ifdef windows},winDbgTypes{$endif}
  {$ifdef linux},nixDbgTypes{$endif}
  {$ifdef darwin},macDbgType{$endif}
  ;

procedure RunDebugger;
var
  dbg : TDbgProcess;
  cmd : String;
  exe : String;
begin
  cmd := cmdline;
  exe := ParamStr(0);
  Delete(cmd, 1, length(exe));
  if cmd = '' then begin
    writeln('executable is not specified');
    Exit;
  end else
    writeln('debugging process: ', exe);

  dbg := DebugProcessStart(exe);
  if not Assigned(dbg) then begin
    writeln('cannot start debug process');
    Exit;
  end;
  try
    writeln('running');

    writeln('debug done');
  finally
    dbg.Free;
  end;
end;

begin
  try
    RunDebugger;
  except
    writeln('exception while debugging');
  end;
end.

