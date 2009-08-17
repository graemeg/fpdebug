{$mode objfpc}

var
  p : PInteger;
begin 
  writeln('testing access violation');
  p := nil;
  try
    p^ := 5;
  except
    writeln('handled access violation');
  end;

  ExitCode := 55;
  writeln('exiting with error code = ', exitCode);
end.
