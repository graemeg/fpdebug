{$mode objfpc}

var
  s : string;

{$asmmode Intel}

procedure TestBreak; assembler;
asm
    int 3;
end;

procedure TestAV;
var 
  a, b : Integer;
  c : integer;
begin
  a := 4;
  b := a - 4;
  try
    writeln('next line fail with division by zero!');
    c := a div b;
    writeln(c);
  except
    writeln('division by zero');
  end;
end;

var
  p : PInteger;
begin 
 // writeln('testing break');
 //TestBreak;
  
  writeln('testing access violation');
  p := nil;
  try
    p^ := 5;
  except
    writeln('handled access violation');
  end;
  (*
  TestAV;
 // try
    raise(exception.create('hello world'));
 // except
 //   writelN('custom exception!');
 // end; 

  writeln('hello world');
  //readln(s);

  //writeln('hello ', s);
  *)
  ExitCode := 55;
  writeln('exiting with error code = ', exitCode);
end.
