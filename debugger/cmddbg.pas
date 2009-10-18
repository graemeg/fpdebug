unit cmddbg; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, contnrs, 
  dbgTypes, dbgUtils, dbgConsts,
  dbgInfoTypes, cmdlineutils, dbgBreakPoints, dbgCPU,
  commands, cmdloop; 

procedure LoadDebugInfo(const FileName: string);
procedure LoadExeDebugInfo(const cmdLine: string);

function GetLineInfo(Addr: TDbgPtr; var FileName: WideString; var LineNum: Integer): Boolean;

implementation

var
  DbgSources : TFPObjectList;
  //DbgInfos   : TFPObjectList;
  CommonInfo : TDbgInfo = nil;
  
type
  
  { TWhereCommand }

  TWhereCommand = class(TCommand)
  public
    procedure Execute(CmdParams: TStrings; Process: TDbgProcess); override;
    function ShortHelp: String; override;
  end;  
  
  { TAddrOf }

  TAddrOf = class(TCommand)
  public
    procedure Execute(CmdParams: TStrings; Process: TDbgProcess); override;
    function ShortHelp: String; override;
  end;
  
  { TIntValue }

  TIntValue = class(TCommand)
    procedure Execute(CmdParams: TStrings; Process: TDbgProcess); override;
    function ShortHelp: String; override;
  end;
  
  { TListSymbols }

  TListSymbols = class(TCommand)
    procedure Execute(CmdParams: TStrings; Process: TDbgProcess); override;
    function ShortHelp: String; override;
  end;
  
  { TSetBreak }

  TSetBreak = class(TCommand)
    procedure Execute(CmdParams: TStrings; Process: TDbgProcess); override;
    function ShortHelp: String; override;
  end;
  
  { TRemoveBreak }

  TRemoveBreak = class(TCommand)
    procedure Execute(CmdParams: TStrings; Process: TDbgProcess); override;
    function ShortHelp: String; override;
  end;

{ TIntValue }

procedure TIntValue.Execute(CmdParams: TStrings; Process: TDbgProcess);  
var
  name  : string;
  err   : Integer;
  addr  : TDbgPtr;
  res   : Integer;
  vl    : Integer;
  sym   : TDbgSymbol;
begin
  if not Assigned(CommonInfo) then begin
    WriteLn('no debug info');
    Exit;    
  end;
  
  if CmdParams.Count <= 1 then begin
    writeln('please specify name of symbol to find, or address');
    Exit;
  end;
  
  name := CmdParams[1];
  val(name, addr, err);
  if err > 0 then begin
    sym := CommonInfo.FindSymbol(name, nil);
    if Assigned(sym) and (sym is TDbgVariable) then
      addr := TDbgVariable(sym).addr
    else  begin
      writeln('symbol not found or cannot be read');
      Exit;
    end;
  end;
    
  res := Process.ReadMem(addr, sizeof(vl), PByteArray(@vl)^);
  if res = sizeof(vl) then writeln(vl)
  else writeln('value cannot be read');
end;

function TIntValue.ShortHelp: String;  
begin
  Result:='prints integer value of a variable by given address o name';
end;

function GetAddr(CmdParams: TStrings; var Addr: TDbgPtr): Boolean;
var
  err : Integer;
begin
  Result := CmdParams.Count > 1;
  if not Result then Exit;
  Val( CmdPArams[1], Addr, err);
  Result:=err=0;
end;
  
{ TRemoveBreak }

procedure TRemoveBreak.Execute(CmdParams: TStrings; Process: TDbgProcess);  
var
  addr  : TDbgPtr;
  bp    : TBreakPoint;
begin
  if not GetAddr(CmdParams, Addr) then begin
    writeln('please specify valid adress to set breakpoint');
    Exit;
  end;
  bp := FindBreakpoint(addr);
  if not Assigned(bp) then
    writeln('no breakpoint at the given address')
  else begin
    RemoveBreakPoint(bp);
    writeln('breakpoint removed');
  end;
end;

function TRemoveBreak.ShortHelp: String;  
begin
  Result := 'removes breakpoint from address';
end;

{ TSetBreak }

procedure TSetBreak.Execute(CmdParams: TStrings; Process: TDbgProcess);  
var
  addr  : TDbgPtr;
begin
  if not GetAddr(CmdParams, Addr) then begin
    writeln('please specify valid adress to set breakpoint');
    Exit;
  end;
  if Assigned(AddEnabledBreakPoint(addr, Process)) then 
    writeln('breakpoint added at: ', HexAddr(addr))
  else
    writeln('failed to set breakpoint at: ', HexAddr(addr));
end;

function TSetBreak.ShortHelp: String;  
begin
  Result := 'sets a breakpoint at address';
end;
  
  
{ TListSymbols }

procedure TListSymbols.Execute(CmdParams: TStrings; Process: TDbgProcess);  
{var
  st  : TStringList;
  i   : Integer;
  s   : string;
  addr  : TDbgPtr;
  maxlen  : integer;}
begin
  if not Assigned(CommonInfo) then begin
    WriteLn('no debug info');
    Exit;    
  end;

  writeln('not implemnted, sorry!');
  
  {
  st := TStringList.Create;
  try
    maxlen := 0;
    CommonInfo.GetNames(st);
    for i := 0 to st.Count - 1 do 
      if length(st[i]) > maxlen then 
        maxlen := length(st[i]);
        
    for i := 0 to st.Count - 1 do begin
      s := st[i];
      CommonInfo.GetAddrByName(s, addr);
      writeln(s:maxlen+1, ' ', IntToHex(addr, sizeof(TDbgPtr)*2));
    end;
  finally
    st.Free;
  end;
  }
end;

function TListSymbols.ShortHelp: String;  
begin
  Result:='prints names lists of all known symbols';
end;


function GetLineNumber(const Str: String; var FileName: String; var LineNum: Integer): Boolean;
var
  i   : integer;
  ln  : string;
  err : Integer;
begin
  i := length(str);
  while (i > 0) and (str[i] <> ':') do dec(i);
  Result := i > 1;
  if not Result then Exit;
  
  ln := Copy(Str, i+1, length(Str)-i);
  Val(ln, LineNum,err);
  Result := err = 0;
  
  if Result then 
    FileName := Copy( Str, 1, i-1);
end;

function GetFullFileName(info: TDbgInfo; const ShortName: AnsiString): AnsiString;
var
  st  : TStringList;
  i   : Integer;
  l   : AnsiString;
begin
  Result := '';
  l := AnsiLowerCase(ShortName);
  st := TStringList.Create;
  try
    info.EnumFiles(st);
    for i := 0 to st.Count - 1 do begin
      if AnsiLowerCase( ExtractFileName( st[i] ) ) = l then begin
        Result := st[i];
        Exit;
      end;
    end;
  finally
    st.Free;
  end;
end;

function GetFileSymbol(info: TDbgInfo; const ShortName: AnsiString): TDbgFileInfo;
var
  nm  : AnsiString;
begin
  nm := GetFullFileName(info, ShortName);
  Result := info.FindFile(nm);
end;

{ TAddrOf }

procedure TAddrOf.Execute(CmdParams: TStrings; Process: TDbgProcess);  
var
  name      : String;
  sym       : TDbgSymbol;
  fsym      : TDbgFileInfo;
  FileName  : string;
  addr      : TDbgPtr;
  LineNum   : Integer;
begin
  if not Assigned(CommonInfo) then begin
    WriteLn('no debug info');
    Exit;    
  end;
  
  if CmdParams.Count <= 1 then begin
    writeln('please specify name of symbol to find, or filename:linenumber');
    Exit;
  end;
  
  name := CmdParams[1];
  sym := CommonInfo.FindSymbol(name, nil);
  if Assigned(sym) then begin
    if sym is TDbgVariable then
      writeln('variable ', name, ' addr: $', HexAddr(TDbgVariable(sym).addr) )
    else 
      writeln('symbol found: ', sym.ClassName)
  end else if GetLineNumber(CmdParams[1], FileName, LineNum) then begin
    fsym := GetFileSymbol(CommonInfo, FileName);
    if Assigned(fsym) then begin
      if fsym.FindAddrByLine(LineNum, addr) then
        writeln('line addr = ', IntToHex(addr, sizeof(TDbgPtr)*2))
      else
        writeln('not line information for line: ', LineNum, ', file ', FileName);
    end else
      writeln('file not found: ', FileName);
  end else
    writeln('symbol not found');
end;

function TAddrOf.ShortHelp: String;  
begin
  Result:='returns address of a symbol by it''s name';
end;
  
function GetFileString(const Name: WideString; LineNum: Integer): String;
var
  st  : TStringList;
begin 
  st := TStringList.Create;
  try
    st.LoadFromFile(Name);
    dec(LineNum);
    if (LineNum >= 0) and (LineNum < st.Count) then
      Result := st[LineNum];
  except
    Result := Format('error file opening file: %s', [Name]);
  end;
  st.Free;
end;  
  
procedure TWhereCommand.Execute(CmdParams: TStrings; Process: TDbgProcess);
var
  regs : TDbgDataList;
  fn   : WideString;
  num  : Integer;
  addr : TDbgPtr;
begin
  if CmdParams.Count <= 1 then begin
    regs := GetProcessRegisters(Process);
    if not Assigned(regs) then begin
      writeln('Cannot read process state. ');
      Exit;
    end;
    addr := regs[_Eip].DbgPtr;
  end;
  if GetLineInfo( addr, fn, num) then Writeln( GetFileString(fn, num));
end;

function TWhereCommand.ShortHelp: String;  
begin
  Result:='returns current execution address of main thread (if possible: filename and line number/line)';
end;

procedure InitDebugCommands;
begin
  DbgSources := TFPObjectList.Create(true);
  
  //RegisterCommand(['where',  'w'], TWhereCommand.Create);
  RegisterCommand(['addrof', 'a'], TAddrOf.Create);
  RegisterCommand(['value', 'l'], TIntValue.Create);
  //RegisterCommand(['listsym'], TListSymbols.Create);
end;

procedure ReleaseDebugCommands;
begin
  DbgSources.Free;
end;

type
  TBreakHandler = class(TObject)
    procedure BreakHandle(Process: TDbgProcess; Event : TDbgEvent);
  end;

procedure TBreakHandler.BreakHandle(Process: TDbgProcess; Event : TDbgEvent);
var
  bp    : TBreakPoint;  
begin
  if Event.Kind = dek_BreakPoint then begin
    writelN('Break?');
    if Event.addr = 0 then begin
      // no address. do nothing.
      writelN('break point event, but no address is given');
      Exit;
    end;
    bp := FindBreakpoint(Event.Addr);

    if not Assigned(bp) then begin
      writeln('not found break point at '+ HexAddr(Event.Addr) +'. Is it hardcoded?');
      Exit;
    end;

    if HandleBreakpoint(bp, Event.Thread, Process ) then     
      WriteLn('breakpoint handled and disabled at ', HexAddr(bp.Addr) )
    else
      WriteLn('failed to handle break point at ', HexAddr(bp.Addr) );
  end;
end;  
  
var
  bh: TBreakHandler = nil;
 
procedure InitBreakCommands;
begin
  bh := TBreakHandler.Create;
  InstallHandler(@bh.BreakHandle);
  RegisterCommand(['break', 'b'], TSetBreak.Create);
  RegisterCommand(['rmbreak', 'rb'], TRemoveBreak.Create);
end;

procedure ReleaseBreakCommands;
begin
  bh.Free;
end;

  
procedure LoadDebugInfo(const FileName: string);
var
  source   : TDbgDataSource;
  infolist : TFPList;
  //i        : Integer;
begin
  source := GetDataSource(FileName);  
  if not Assigned(source) then 
    writeln('[LoadDebugInfo] cannot find reader for the file: ', FileName);
  //writeln('[LoadDebugInfo] source file accepted: ', FileName);
  DbgSources.Add(source);
  
  infolist := TFPList.Create;
  GetDebugInfos(source, infolist);
  if infolist.Count > 0 then 
    CommonInfo := TDbgInfo(infolist[0]);
  {for i := 0 to infolist.Count - 1 do begin
    //writeln('[LoadDebugInfo] debug info found: ', TDbgInfo(infolist[i]).ClassName );
    DbgInfos.Add( TObject(infolist[i]));
  end;}
  infolist.Free;  
end;  

procedure LoadExeDebugInfo(const cmdLine: string);
var
  binname : String;
begin
  binName := GetBinaryName(cmdLine);
  LoadDebugInfo(binName);
end;

function GetLineInfo(Addr: TDbgPtr; var FileName: WideString; var LineNum: Integer): Boolean;
var
  f   : TDbgFileInfo;
begin
  f := CommonInfo.FindFile(FileName);
  Result := Assigned(f) and f.FindLineByAddr(Addr, LineNum);
end;

initialization
  InitDebugCommands;
  InitBreakCommands;

finalization
  ReleaseDebugCommands;
  ReleaseBreakCommands;

end.

