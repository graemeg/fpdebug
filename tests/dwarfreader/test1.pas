// compile for stabs: fpc -g test1.pas
{$ASMMODE intel}
{$mode objfpc}

program p1;

uses
  test2;

type
  MyType = record
   a, b : integer;
  end;
var
  bb : byte;

procedure ProcCdecl(a: Integer; var b: byte; p: Pointer; s: single; const k: string); cdecl;
var
  bb : Integer;
  cc : Integer;
  buf : array [0..4095] of Integer;
begin
  buf[0] := 1000000;
  buf[4095] := 1000001;
  cc := a - 2;
  bb := b * cc;
  writeln('hello cdecl ', a, ' ', Integer(p), ' ',s, ' ', k);
  inc(b, bb+buf[0]+buf[4095]);
end;

procedure ProcFastcall(a: Integer; var b: byte; p: Pointer; s: single; const k: string); 
var
  bb : Integer;
  cc : Integer;
  buf : array [0..4080] of byte;
  dd  : Integer;
  t1 : Integer;
  t2 : Integer;
  t3 : Integer;
  t4 : Integer;
  t5 : Integer;
  t6 : Integer;
begin	
  cc := a - 2;
  bb := b * cc;
  writeln('hello fastcall ', a, ' ', Integer(p), ' ',s, ' ', k);
  inc(b);
  if b>0 then begin
    FillChar(buf, sizeof(buf), 0);
    t1:=buf[0];
    t2:=t1; 
    t3:=t2; 
    t4:=t3;
    t5:=t4;
    t6:=t1+t2+t3+t4+t5;
    writeln('t6 = ', t6);
  end;
end;

procedure ProcStdcall(a: Integer; var b: byte; p: Pointer; s: single; const k: string); stdcall;
var
  bb : Integer;
  cc : Integer;
begin
  cc := a - 2;
  bb := b * cc;
  writeln('hello stdcall ', a, ' ', Integer(p), ' ',s, ' ', k);
  inc(b);
end;

procedure ProcCdecl2(a: Integer; var b: byte; p: Pointer; s: single; const k: string); cdecl;
begin
  writeln('hello cdecl2 ', a, ' ', Integer(p), ' ',s, ' ', k);
  inc(b);
end;

procedure BreakPoint; assembler;
asm
  int 3;
end;

var
  my : MyType;

begin
  bb := 1;
  {BreakPoint;}
  ProcCdecl(1, bb, nil, 0, 'test');
  ProcFastCall(2, bb, nil, 0, 'test');
  ProcStdCall(3, bb, nil, 0, 'test');
  ProcCdecl2(8, bb, nil, 0, 'test');
  writeln(GetHi);
  my.a := bb;
  writeln(my.a);
end.