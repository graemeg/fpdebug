{$mode objfpc}

var
  s : string;

{$asmmode Intel}

procedure TestBreak; assembler;
asm
    int 3;
end;

begin 
  writeln('testing break');
  TestBreak;
  writeln('hello world');
  readln(s);
  writeln('hello ', s);
end.
