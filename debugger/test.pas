{$mode objfpc}

procedure DoBreak; assembler;
asm
  int 3;
end;


var
  p : PInteger;
begin
  writeln('testing break point'); 
  try
    DoBreak;
  except
    writelN('handled!'); 
  end;

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
