program fpdebugger;

{$mode objfpc}{$H+}

uses
  dbgTypes,
  cmdloop, commands
  {$ifdef windows},winDbgTypes{$endif}
  {$ifdef linux},nixDbgTypes{$endif}
  {$ifdef darwin},macDbgType{$endif}
  ;

procedure RunDebugger;
var
  dbg : TDbgProcess;
  evn : TDbgEvent;
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

