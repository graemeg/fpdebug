unit cmdmemview;

{$ifdef fpc}{$mode delphi}{$H+}{$endif}

interface

uses
  Classes, SysUtils,
  dbgTypes, dbgUtils, dbgConsts, dbgMain,
  commands;

type

  { TViewMemCommand }

  TViewMemCommand = class(TCommand)
    procedure Execute(CmdParams: TStrings; Env: TCommandEnvironment); override;
    function ResetParamsCache: Boolean; override;
    function ShortHelp: String; override;
  end;

  { TRegistersView }

  TRegistersView = class(TCommand)
    procedure Execute(CmdParams: TStrings; Env: TCommandEnvironment); override;
    function ShortHelp: String; override;
  end;

  { TStackView }
  TStackView  = class(TCommand)
    procedure Execute(CmdParams: TStrings; Env: TCommandEnvironment); override;
    function ShortHelp: String; override;
  end;

procedure PrintI386Regs(list: TDbgDataList);
  
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

procedure PrintProcessMem(AProcess: TDbgProcess; Offset: TDbgPtr);
var
  buf : array [0..32*16-1]of byte;
  ofs : String; 
  hex : String;
  bin : String;
  i   : Integer;
  res : Integer;
begin
  FillChar(buf[0], sizeof(buf), 0);
  res:=AProcess.ReadMem(Offset, sizeof(buf), buf);
  if res<0 then begin
    writeln('cannot read proc ', AProcess.ID, ', mem. Result = ', res);
    Exit;
  end;

  for i := 0 to 31 do begin
    ofs := IntToHex( TDbgPtr(Offset)+TDbgPtr(i*16), sizeof(TDbgPtr)*2);
    hex := BufToHex(buf, i*16, 16);
    bin := BufToStr(buf, i*16, 16);
    writeln(ofs+ ' | ' + hex + ' | ' + bin);
  end;
end;

{ TViewMemCommand }

procedure TViewMemCommand.Execute(CmdParams: TStrings; Env: TCommandEnvironment);
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
    writeln('env process ID = ', Env.Process.ID);
    PrintProcessMem(Env.Process, ofs);
    LastReadOfs := ofs + 32*16;
  except
    writeln('exception while reading process memory');
  end;
end;

function TViewMemCommand.ResetParamsCache: Boolean;
begin
  Result := true;
end;

function TViewMemCommand.ShortHelp: String;  
begin
  Result:='prints process memory at the give address';
end;

{ TRegistersView }

procedure PrintAllRegs(list: TDbgDataList);
var
  i     : Integer;
  data  : TDbgData;
begin
  for i := 0 to list.Count - 1 do begin
    data := list.RegByIndex(i);
    case data.BitSize of
      8:  writeln( data.Name, ' ', IntToHex(data.UInt8, 2));
      16: writeln( data.Name, ' ', IntToHex(data.UInt16, 4));
      32: writeln( data.Name, ' ', IntToHex(data.UInt32, 8));
      64: writeln( data.Name, ' ', IntToHex(data.UInt64, 16));
    end;
  end;
end;


procedure PrintI386Regs(list: TDbgDataList);

  function StrHex(const regname: String): string;
  begin
    Result := IntToHex(list[regname].UInt32, 8);
  end;

begin
  with list do begin
    WriteLn(Format('EAX: %s;  GS: %s;     EBP: %s;     ', [ StrHex(_Eax), StrHex(_Gs), StrHex(_Ebp)]));
    WriteLn(Format('EBX: %s;  FS: %s;     EIP: %s;     ', [ StrHex(_Ebx), StrHex(_Fs), StrHex(_Eip)]));
    WriteLn(Format('ECX: %s;  ES: %s;  EFLAGS: %s;     ', [ StrHex(_Ecx), StrHex(_Es), StrHex(_EFlags)]));
    WriteLn(Format('EDX: %s;  DS: %s;     ESP: %s;     ', [ StrHex(_Edx), StrHex(_Ds), StrHex(_Esp)]));
    WriteLn(Format('EDI: %s;  CS: %s;                  ', [ StrHex(_Edi), StrHex(_Cs)]));
    WriteLn(Format('ESI: %s;  SS: %s;                  ', [ StrHex(_Esi), StrHex(_Ss)]));
  end;
end;

procedure TRegistersView.Execute(CmdParams: TStrings; Env: TCommandEnvironment);
var
  regs  : TDbgDataBytesList;
begin
  //todo: thread number can be passed in CmdParams
  if not Assigned(Env.Thread) then begin

    writeln('no execution thread');
    Exit;
  end;
  
  regs := TDbgDataBytesList.Create;
  Env.Thread.GetThreadRegs(regs);
  {$ifdef CPUi386}
  PrintI386Regs(regs);
  {$else}
  PrintAllRegs(regs);
  {$endif}

  regs.Free;
end;

function TRegistersView.ShortHelp: String;  
begin
  Result:='prints main thread''s registers';
end;

{ TStackView }

procedure TStackView.Execute(CmdParams:TStrings;Env:TCommandEnvironment);
var
  regs  : TDbgDataBytesList;
  esp   : TDbgPtr;
  i     : Integer;

  eval  : LongWord;
  addr  : LongWord;
  buf   : array [0..4] of byte;
begin
  regs := TDbgDataBytesList.Create;
  Env.Thread.GetThreadRegs(regs);
  esp:=regs[_Esp].DbgPtr;
  for i:=0 to 40 do begin
    write('esp: ', HexStr(esp,8),' ');
    Env.Process.ReadMem(esp, 4, PByteArray(@addr)^);
    write('add1: ', HexStr(addr,8),' ');
    dec(addr, 5);
    write('addr: ', HexStr(addr,8),' ');
    Env.Process.ReadMem(addr, sizeof(buf), buf);
    if (buf[0]=$e8) then
      writeln(' zero call from ', HexStr(addr,8))
    else if (buf[4]=$e8) then
      writeln(' last call from ', HexStr(addr,8))
    else
      writeln;

    inc(esp, 4);
  end;

  {writeln('eip = ', HexStr(regs[_Eip].DbgPtr,8));
  writeln('esp = ', HexStr(regs[_Esp].DbgPtr,8));
  writeln('ebp = ', HexStr(regs[_Ebp].DbgPtr,8));}
  regs.Free;
end;

function TStackView.ShortHelp:String;
begin
  Result:='Show callstack';
end;

procedure RegisterMemCommands;
begin
  RegisterCommand(['view','v'], TViewMemCommand.Create);
  RegisterCommand(['reg','g'], TRegistersView.Create);
  {$ifdef cpui386}
  RegisterCommand(['unwind','stack','st'], TStackView.Create);
  {$endif}
end;

initialization
  RegisterMemCommands;

end.

