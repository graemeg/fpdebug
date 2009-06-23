program fpdebugger;

{$mode objfpc}{$H+}

uses
  dbgTypes
  {$ifdef windows},winDbgTypes, cmdloop, commands{$endif}
  {$ifdef linux},nixDbgTypes{$endif}
  {$ifdef darwin},macDbgType{$endif};

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
      {writeln('running');
      while dbg.WaitNextEvent(evn) do begin
        case evn.Kind of
          dek_Other: write('other');
          dek_BreakPoint: write('breakpoint');
          dek_ProcessStart: write('process started');
          dek_ProcessTerminated: write('process terminated');
        end;
        write(' ...press enter');
        readln;
      end;
      writeln('debug done');}
      RunLoop(dbg)
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

