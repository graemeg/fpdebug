unit cmdloop; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, dbgTypes, commands; 

procedure RunLoop(Process: TDbgProcess);
  
implementation

var
  LastCommand: String;

const
  CmdPrefix = 'duby>';  
  
procedure ExectuteCommand;
var
  s : string;
  p : TStringList;
begin
  write(CmdPrefix);
  readln(s);
  if s = '' then s := LastCommand;
  if s <> '' then begin
    p := TStringList.Create;  
    p.Add(s);
    if not ExecuteCommand(p, nil) then
      writeln('unknown command ', p[0]);
    LastCommand := s;
    p.Free;
  end;
end;

procedure DoRunLoop(Process: TDbgProcess);
begin
  while true do begin
    ExectuteCommand;
  end;
end;

procedure RunLoop(Process: TDbgProcess);
begin
  try
    DoRunLoop(Process);
  except
  end;
end;

end.

