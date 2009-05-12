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
begin
  a := 4;
  b := a - 4;
  try
    writeln(a div b);
  except
    writeln('division by zero');
  end;
end;

begin 
  //writeln('testing break');
  //TestBreak;

  writeln('testing access violation');
  TestAV;


  writeln('hello world');
  readln(s);

  writeln('hello ', s);
end.
