unit memviewer; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, 
  dbgTypes, commands; 

type  
  
  { TViewMemCommand }

  TViewMemCommand = class(TCommand)
    procedure Execute(CmdParams: TStrings; Process: TDbgProcess); override;
    function ResetParamsCache: Boolean; override; 
  end;

implementation


var
  LastReadOfs : TDbgPtr = 0;

function BufToStr(const buf: array of byte; index, Count: Integer): string;
var
  i : Integer;
  j : Integer;
begin
  SetLength(Result, Count);
  j := 1;
  for i := index to index + Count - 1 do begin
    if buf[i] < 32 then Result[j] := '.'
    else Result[j] := char(Buf[i]);
    inc(j);
  end;
end;

function BufToHex(const buf: array of byte; index, Count: Integer): string;
var
  i : Integer;
  j : Integer;
  s : string;
const 
  hexd : array [0..$F] of char = ('0','1','2','3','4','5','6','7','8','9','A','B','C','D','E','F');
begin
  SetLength(Result, Count*3);
  j := 1;
  for i := index to index + Count - 1 do begin
    Result[j] := hexd[buf[i] shr 4];  inc(j);
    Result[j] := hexd[buf[i] and $F]; inc(j);
    Result[j] := #32; inc(j);
  end;
end;

procedure PrintProcessMem(proc: TDbgProcess; Offset: TDbgPtr);
var
  buf : array [0..32*16-1]of byte;
  ofs : String; 
  hex : String;
  bin : String;
  i   : Integer;  
begin
  FillChar(buf[0], sizeof(buf), 0);
  if proc.ReadMem(Offset, sizeof(buf), buf) < 0 then begin
    writeln('cannot read proc mem ');
    Exit;
  end;

  for i := 0 to 31 do begin
    ofs := IntToHex(Offset+i*16, sizeof(TDbgPtr)*2);
    hex := BufToHex(buf, i*16, 16);
    bin := BufToStr(buf, i*16, 16);
    writeln(ofs+ ' | ' + hex + ' | ' + bin);
  end;
end;

{ TViewMemCommand }

procedure TViewMemCommand.Execute(CmdParams: TStrings; Process: TDbgProcess);  
var
  ofs : TDbgPtr;
  err : Integer;
begin
  if CmdParams.Count <= 1 then 
    ofs := LastReadOfs
  else begin
    Val(CmdParams[1], ofs, err);
    if err <> 0 then begin
      writeln('address value is ignored. Use decimal or hexidemcal-pascal form ($0A)');
      Exit;
    end;
  end;
  try
    PrintProcessMem(Process, ofs);
    LastReadOfs := ofs + 32*16;
  except
    writeln('exception while reading process memory');
  end;
end;

function TViewMemCommand.ResetParamsCache: Boolean;  
begin
  Result := true;
end;

initialization
  RegisterCommand(['view','v'], TViewMemCommand.Create);

end.

