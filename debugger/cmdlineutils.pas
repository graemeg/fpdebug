unit cmdlineutils; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils; 

function GetBinaryName(const cmdLine: string): string;  
  
implementation

function GetBinaryName(const cmdLine: string): string;
var
  i     : Integer;
  st    : Integer;
  StopChars : set of char;
const
  InvalidNameChars : set of char = [';','"','''','\','/','|','<','>','*','?',' '];
begin           
  Result := '';
  i:=1;
  while (i <= length(cmdline)) and (cmdLine[i] in [#9,#10,#13,#32]) do 
    inc(i);  
  if i > length(cmdline) then Exit;    

  if cmdline[i] = '"' then begin
    inc(i);
    StopChars := ['"'];
  end else
    StopChars := InvalidNameChars;
  st := i;
  
  for i := i to length(cmdLine) do 
    if (cmdLine[i] in StopChars) then begin
      Result := Copy(cmdLine, st, i - st);
      Exit;
    end;
  Result := Copy(cmdLine, st, length(cmdLine)-st+1);
end;

end.

