// compile for stabs: fpc -g test1.pas
program p1;

procedure ProcCdecl(a: Integer; b: byte; p: Pointer; s: single; k: string); cdecl;
begin
  writeln('hello cdecl ', a, ' ', Integer(p), ' ',s, ' ', k);
end;

procedure ProcFastcall(a: Integer; b: byte; p: Pointer; s: single; k: string); 
begin
  writeln('hello fastcall ', a, ' ', Integer(p), ' ',s, ' ', k);
end;

procedure ProcStdcall(a: Integer; b: byte; p: Pointer; s: single; k: string); stdcall;
begin
  writeln('hello stdcall ', a, ' ', Integer(p), ' ',s, ' ', k);
end;

procedure ProcCdecl2(a: Integer; b: byte; p: Pointer; s: single; k: string); cdecl;
begin
  writeln('hello cdecl2 ', a, ' ', Integer(p), ' ',s, ' ', k);
end;

begin
  ProcCdecl(1, 0, nil, 0, 'test');
  ProcFastCall(2, 0, nil, 0, 'test');
  ProcStdCall(3, 0, nil, 0, 'test');
  ProcCdecl2(8, 0, nil, 0, 'test');
end.