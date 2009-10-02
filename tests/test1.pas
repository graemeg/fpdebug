// compile for stabs: fpc -g test1.pas
program p1;

var
  bb : byte;

procedure ProcCdecl(a: Integer; var b: byte; p: Pointer; s: single; const k: string); cdecl;
begin
  writeln('hello cdecl ', a, ' ', Integer(p), ' ',s, ' ', k);
  inc(b);
end;

procedure ProcFastcall(a: Integer; var b: byte; p: Pointer; s: single; const k: string); 
begin
  writeln('hello fastcall ', a, ' ', Integer(p), ' ',s, ' ', k);
  inc(b);
end;

procedure ProcStdcall(a: Integer; var b: byte; p: Pointer; s: single; const k: string); stdcall;
begin
  writeln('hello stdcall ', a, ' ', Integer(p), ' ',s, ' ', k);
  inc(b);
end;

procedure ProcCdecl2(a: Integer; var b: byte; p: Pointer; s: single; const k: string); cdecl;
begin
  writeln('hello cdecl2 ', a, ' ', Integer(p), ' ',s, ' ', k);
  inc(b);
end;

begin
  bb := 1;	
  ProcCdecl(1, bb, nil, 0, 'test');
  ProcFastCall(2, bb, nil, 0, 'test');
  ProcStdCall(3, bb, nil, 0, 'test');
  ProcCdecl2(8, bb, nil, 0, 'test');
end.