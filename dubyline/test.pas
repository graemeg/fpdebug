{$ifdef fpc}
{$mode delphi}
{$apptype console}
{$ASMMODE intel}
{$else}
{$apptype console}
{$endif}
procedure DoBreak; assembler;
asm
  int 3;
end;

procedure TestIntelBreak;
begin
  writeln('testing break point');
  try
    DoBreak;
  except
    writeln('handled!');
  end;
  writeln;
end;

procedure TestAccessViolation;
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
  writeln;
end;

procedure TestLoop(a, b, c: Integer);
var
  i : Integer;
  d : array of Integer;
type
  TMySubType = record
    a,z : integer;
    b   : array [1..10] of record b, c : Integer; end;
  end;
begin
  SetLength(d, 10);
  for i := 1 to 10 do 
    d[i-1]:=i+a;
  for i:=0 to 9 do 
    writeln(d[i]);
end;

type
  TMyType = record
    mt	: Integer;
    olo : PInteger;
  end;

begin
  //TestIntelBreak;
  //TestAccessViolation;
  TestLoop(10, 20, 30);
  ExitCode := 55;
  //writeln('exiting with error code = ', ExitCode);
  //readln;
end.
