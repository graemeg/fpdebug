program fpdebugger;

{$mode objfpc}{$H+}

uses
  dbgTypes
  {$ifdef windows},winDbgTypes{$endif}
  {$ifdef linux},nixDbgTypes{$endif}
  {$ifdef darwin},macDbgType{$endif}
  , winDbgProc;

procedure RunDebugger;
var
  dbg : TDbgProcess;
  evn : TDbgEvent;
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
    while dbg.WaitNextEvent(evn) do begin
      case evn.Kind of
        dek_Other: write('other');
        dek_ProcessStart: write('process started');
        dek_ProcessTerminated: write('process terminated');
      end;
      readln;
    end;
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

