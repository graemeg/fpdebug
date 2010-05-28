unit cmdDbg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, contnrs, 
  dbgTypes, dbgUtils, dbgConsts,
  dbgCPU, dbgInfoTypes, dbgInfoUtils, dbgDataRead,
  cmdlineutils,
  commands, cmdloop; 

//todo: remove debug info loading to some common units
procedure LoadDebugInfo(const FileName: string);
procedure LoadExeDebugInfo(const cmdLine: string);

function GetLineInfo(Addr: TDbgPtr; var FileName: WideString; var LineNum: Integer): Boolean;

var
  CommonInfo   : TDbgInfo = nil;
  DbgInfoFiles : TStringList = nil;

implementation

type
  
  { TWhereCommand }

  TWhereCommand = class(TCommand)
  public
    procedure Execute(CmdParams: TStrings; Env: TCommandEnvironment); override;
    function ShortHelp: String; override;
  end;  
  
  { TAddrOf }

  TAddrOf = class(TCommand)
  public
    procedure Execute(CmdParams: TStrings; Env: TCommandEnvironment); override;
    function ShortHelp: String; override;
  end;
  
  { TIntValue }

  TIntValue = class(TCommand)
    procedure Execute(CmdParams: TStrings; Env: TCommandEnvironment); override;
    function ShortHelp: String; override;
  end;
  
  { TListSymbols }

  TListSymbols = class(TCommand)
    procedure Execute(CmdParams: TStrings; Env: TCommandEnvironment); override;
    function ShortHelp: String; override;
  end;
  
  { TSetBreak }

  TSetBreak = class(TCommand)
    procedure Execute(CmdParams: TStrings; Env: TCommandEnvironment); override;
    function ShortHelp: String; override;
  end;
  
  { TRemoveBreak }

  TRemoveBreak = class(TCommand)
    procedure Execute(CmdParams: TStrings; Env: TCommandEnvironment); override;
    function ShortHelp: String; override;
  end;

{ TIntValue }

procedure TIntValue.Execute(CmdParams: TStrings; Env: TCommandEnvironment);
var
  name    : string;
  err     : Integer;
  addr    : TDbgPtr;
  res     : Integer;
  sym     : TDbgSymbol;
  reader  : TDbgTypeRead;
  vardata : array of byte;
  varsize : LongWord;
  vartype : TDbgSymType;
  p       : TDbgPtr;
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
    if Assigned(sym) and (sym is TDbgSymVar) then begin
      addr:=GetVarAddr( TDbgSymVar(sym), Env.Thread);
    end else  begin
      writeln('symbol not found or cannot be read');
      Exit;
    end;
  end else
    sym:=nil;

  if Assigned(sym) and (sym is TDbgSymVar) then begin
    vartype:=TDbgSymVar(sym).VarType;
    reader:=GetReaderForType(TDbgSymClass(vartype.ClassType));
    if Assigned(reader) then begin

      if vartype.isRefType then begin
        res:=Env.Process.ReadMem(addr, sizeof(p), PByteArray(@p)^);
        addr:=p+vartype.DerefOfs;
      end;

      varsize:=vartype.GetVarSize;
      SetLength(vardata, varsize);
      res := Env.Process.ReadMem(addr, varsize, vardata[0]);
      if res = varsize then
        writeln(reader.Dump(VarType, vardata[0], varsize) )
      else
        writeln('value of var scannot be read. ',
          'var ', TDbgSymVar(sym).Name , ' addr: $', HexAddr(addr));
    end else
      writeln('cannot find reader for the type. var ', TDbgSymVar(sym).Name ,
              ' addr: $', HexAddr(addr));
  end else
    writeln('not a variable. var addr: ', HexAddr(addr));
end;

function TIntValue.ShortHelp: String;  
begin
  Result:='prints integer value of a variable by given address o name';
end;

function GetAddr(CmdParams: TStrings; var Addr: TDbgPtr): Boolean;
var
  err : Integer;
  i   : Integer;
  nm  : AnsiString;
  fname   : AnsiString;
  full    : AnsiString;
  linenum : AnsiString;
  ln      : Integer;

begin
  Result := CmdParams.Count > 1;
  if not Result then Exit;
  Val( CmdParams[1], Addr, err);
  Result:=err=0;

  // try to get file name
  if not Result then begin
    nm:=CmdParams[1];
    for i:=length(nm) downto 1 do
      if nm[i]=':' then begin
        fname:=Copy(nm, 1, i-1);
        linenum:=Copy(nm, i+1, length(nm)-1);
        Val(linenum, ln, err);
        Result:=(err=0);
        if Result then begin
          full:=FindSourceFileName(CommonInfo, fname);
          if full='' then begin
            WriteLn('unable to find full file name of "', fname,'"');
            Result:=False;
          end else begin
            Result:=FindLineAddr(CommonInfo, full, ln, Addr);
            if not Result then
              Writeln('unable to find an addr for: ', full,' ', ln);
          end;
        end else
          Writeln('bad line number: ', ln);
        if Result then
          writeln(full,':',ln);
        Exit;
      end;
  end;

end;
  
{ TRemoveBreak }

procedure TRemoveBreak.Execute(CmdParams: TStrings; Env: TCommandEnvironment);
{var
  addr  : TDbgPtr;
  bp    : TBreakPoint;}
begin
{  if not GetAddr(CmdParams, Addr) then begin
    writeln('please specify valid adress to set breakpoint');
    Exit;
  end;
  bp := FindBreakpoint(addr);
  if not Assigned(bp) then
    writeln('no breakpoint at the given address')
  else begin
    RemoveBreakPoint(bp);
    writeln('breakpoint removed');
  end;}
end;

function TRemoveBreak.ShortHelp: String;  
begin
  Result := 'removes breakpoint from address';
end;

{ TSetBreak }

procedure TSetBreak.Execute(CmdParams: TStrings; Env: TCommandEnvironment);
var
  addr  : TDbgPtr;
begin
  if not GetAddr(CmdParams, Addr) then begin
    writeln('please specify valid adress to set breakpoint');
    Exit;
  end;

  if Env.Process.isBreakpointEnabled(Addr) then begin
    Env.Process.DisableBreakpoint(Addr);
    writeln('disabling breakpoint at ', HexAddr(Addr));
  end else begin
    if Env.Process.EnableBreakpoint(Addr) then
      writeln('breakpoint set at ', HexAddr(Addr))
    else
      writeln('unable to set breakpoint at ', HexAddr(Addr));
  end;
end;

function TSetBreak.ShortHelp: String;  
begin
  Result := 'sets a breakpoint at address';
end;
  
  
{ TListSymbols }

procedure TListSymbols.Execute(CmdParams: TStrings; Env: TCommandEnvironment);
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
    info.EnumSourceFiles(st);
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

function GetFileSymbol(info: TDbgInfo; const ShortName: AnsiString): TDbgSymFile;
var
  nm  : AnsiString;
begin
  nm := GetFullFileName(info, ShortName);
  Result := info.FindFile(nm);
end;

{ TAddrOf }

procedure TAddrOf.Execute(CmdParams: TStrings; Env: TCommandEnvironment);
var
  name      : String;
  sym       : TDbgSymbol;
  fsym      : TDbgSymFile;
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
    addr:=GetVarAddr( TDbgSymVar(sym), Env.Thread);
    if sym is TDbgSymVar then begin
      writeln('variable ', name, ' addr: $', HexAddr(Addr) );
    end else
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
  Result:='returns address of a symbol by it''s name, or address of the line of code';
end;
  
procedure PrintFileString(const Name: WideString; LineNum: Integer);
var
  st    : TStringList;
  first : integer;
  i     : integer;
begin 
  st := TStringList.Create;
  try
    st.LoadFromFile(Name);
    dec(LineNum);

    first := LineNum - 5;
    if first < 0 then first := 0;
    
    for i := first to Min(st.Count-1, LineNum + 5) do begin
      if i = LineNum 
        then writeln('>>>',i+1, ': ', st[i])
        else writeln('   ',i+1, ': ', st[i]);
    end;
  except
  end;
  st.Free;
end;  
  
procedure TWhereCommand.Execute(CmdParams: TStrings; Env: TCommandEnvironment);
var
  regs : TDbgDataList;
  fn   : WideString;
  num  : Integer;
  addr : TDbgPtr;
  err  : Integer;
begin
  Exit;
  
  if CmdParams.Count <= 1 then begin
    //regs := GetProcessRegisters(Process, 0);
    regs:=nil;
    if not Assigned(regs) then begin
      writeln('Cannot read process state. ');
      Exit;
    end;
    addr := regs[_Eip].DbgPtr;
  end else
    Val(CmdParams[1], addr, err);
  if GetLineInfo(addr, fn, num) then begin
    writeln('found at: ', fn);
    writelN('line num: ', num);
    PrintFileString(fn, num);
  end else
    writeln('address not found');
end;

function TWhereCommand.ShortHelp: String;  
begin
  Result:='returns current execution address of main thread (if possible: filename and line number/line)';
end;

type
  TBreakHandler = class(TObject)
    procedure BreakHandle(Process: TDbgTarget; Event : TDbgEvent);
  end;

procedure TBreakHandler.BreakHandle(Process: TDbgTarget; Event : TDbgEvent);
{var
  bp    : TBreakPoint;  }
begin
{  if Event.Kind = dek_BreakPoint then begin
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
  end;}
end;  
  
var
  bh: TBreakHandler = nil;
 
procedure ReleaseBreakCommands;
begin
  bh.Free;
end;

  
procedure LoadDebugInfo(const FileName: string);
begin
  if not Assigned(CommonInfo) then CommonInfo := TDbgInfo.Create;
  if LoadDebugInfoFromFile(CommonInfo, FileName) then
    DbgInfoFiles.Add(FileName);
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
  f   : TDbgSymFile;
  i   : integer;
  st  : TStringList;
begin
  st := TStringList.Create;
  CommonInfo.EnumSourceFiles(st);
  for i := 0 to st.Count - 1 do  begin
    f := CommonInfo.FindFile(st[i]); 
    Result := Assigned(f) and f.FindLineByAddr(Addr, LineNum);
    if Result then begin
      FileName := st[i];
      Exit;
    end;
  end;
  Result := false;
end;

procedure RegisterDebugCommands;
begin
  RegisterCommand(['where',  'w'], TWhereCommand.Create);
  RegisterCommand(['addr', '@'], TAddrOf.Create);
  RegisterCommand(['value', 'l'], TIntValue.Create);
end;

procedure RegisterBreakCommands;
begin
  bh := TBreakHandler.Create;
  RegisterCommand(['break', 'b'], TSetBreak.Create);
  RegisterCommand(['rmbreak', 'rb'], TRemoveBreak.Create);
end;

initialization
  RegisterDebugCommands;
  RegisterBreakCommands;
  DbgInfoFiles := TStringList.Create;

finalization
  ReleaseBreakCommands;

  if Assigned(CommonInfo) then CommonInfo.Free;
  DbgInfoFiles.Free;

end.

