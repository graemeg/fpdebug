unit cmddbg; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, contnrs, 
  dbgTypes, dbgUtils, dbgConsts,
  dbgInfoTypes, cmdlineutils,
  commands; 

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
  
  { TListSymbols }

  TListSymbols = class(TCommand)
    procedure Execute(CmdParams: TStrings; Process: TDbgProcess); override;
    function ShortHelp: String; override;
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

{ TAddrOf }

procedure TAddrOf.Execute(CmdParams: TStrings; Process: TDbgProcess);  
var
  name  : String;
  sym   : TDbgSymbol;
begin
  if not Assigned(CommonInfo) then begin
    WriteLn('no debug info');
    Exit;    
  end;
  
  if CmdParams.Count <= 1 then begin
    writeln('please specify name of symbol to find');
    Exit;
  end;
  
  name := CmdParams[1];
  sym := CommonInfo.FindSymbol(name, nil);
  if Assigned(sym) then
    writeln('symbol found: ', sym.ClassName)
  else
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
  
  WriteLn('addr = ', addr);
  if GetLineInfo( addr, fn, num) then Writeln( GetFileString(fn, num));
end;

function TWhereCommand.ShortHelp: String;  
begin
  Result:='returns current execution address of main thread (if possible: filename and line number/line)';
end;

procedure InitDebugCommands;
begin
  DbgSources := TFPObjectList.Create(true);
  
  RegisterCommand(['where',  'w'], TWhereCommand.Create);
  RegisterCommand(['addrof', 'a'], TAddrOf.Create);
  RegisterCommand(['listsym'], TListSymbols.Create);
end;

procedure ReleaseDebugCommands;
begin
  DbgSources.Free;
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

finalization
  ReleaseDebugCommands;

end.

