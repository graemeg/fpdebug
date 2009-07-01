unit winDbgProc; 

{$mode objfpc}{$H+}

interface

uses
  Windows, DbgTypes, SysUtils;
  
const
  hexsize = sizeof(TDbgPtr)*2;

function CreateDebugProcess(const CmdLine: String; out Info: TProcessInformation): Boolean;

function ReadProcMem(dwProc: THandle; Offset : TDbgPtr; Count: Integer; var data: array of byte): Integer;
function WriteProcMem(dwProc: THandle; Offset : TDbgPtr; Count: Integer; const data: array of byte): Integer;

procedure WinEventToDbgEvent(ProcessHandle: THandle; const Win: TDebugEvent; var Dbg: TDbgEvent);

implementation

function ReadPointerSize(dwProc: THandle; Offset: TDbgPtr): TDbgPtr;
begin
  if ReadProcMem(dwProc, Offset, sizeof(Result), PbyteArray(@Result)^) < 0 then begin
    //writeln('failed to read pointer !');
    Result := 0;
  end;
end;

function ReadPCharAtProc(dwProc: THandle; Offset: TDbgPtr; IsUnicode: Boolean): WideString;
var
  i   : Integer;  
  buf : array of byte;
  s   : AnsiString;
begin
  i := 0;
  SetLength(buf, 0);
  repeat
    if i >= length(buf) then begin
      if length(buf) = 0 then SetLength(buf, 1024)
      else SetLength(buf, length(Result)*2);
    end;
    
    if ReadProcMem(dwProc, Offset, length(buf), PbyteArray(@buf[i])^) < 0 then begin
      //writeln('failed to read');
      Result := '';
      Exit;
    end;
    
    if not IsUnicode then 
      while (i < length(buf)) and (buf[i] <> 0) do inc(i)
    else begin
      while (i < length(buf)) and (PWORD(@buf[i])^ <> 0) do inc(i, 2)
    end;
    
  until (i < length(buf));

  if not isUnicode then begin
    if i > 0 then begin
      SetLength(s, i);
      Move(buf[0], s[1], i);
      Result := s;
    end else
      Result := '';
  end else begin
    SetLength(Result, i div 2);
    Move(buf[0], Result[1], i);
  end;
end;

function ReadPCharAtPointer(dwProc: THandle; PointerOffset: TDbgPtr; isUnicode: Boolean): String;
var
  ptr : TDbgPtr;
begin
  Result := '';
  //writelN('PointerOffset = ',PointerOffset, ' ', IntToHex(PointerOffset, 8));
  if PointerOffset = 0 then Exit;
  
  ptr := ReadPointerSize(dwProc, PointerOffset);
  //writelN('ptr = ',ptr);
  if ptr = 0 then 
    Exit
  else 
    Result := ReadPCharAtProc(dwProc, ptr, isUnicode);
end;

function ReadProcMem(dwProc: THandle; Offset : TDbgPtr; Count: Integer; var data: array of byte): Integer;
var
  res : LongWord;
begin
  if not ReadProcessMemory(dwProc, Pointer(Offset), @data[0], Count, res) then begin
    Result := -1;
    //writeln('error reading Proc mem = ',GetLastError);
  end else
    Result := res;  
end;

function WriteProcMem(dwProc: THandle; Offset : TDbgPtr; Count: Integer; const data: array of byte): Integer;
var
  res : LongWord;
begin
  if not WriteProcessMemory(dwProc, Pointer(Offset), @data[0], Count, res) then
    Result := -1
  else
    Result := res;
end;


function CreateDebugProcess(const CmdLine: String; out Info: TProcessInformation): Boolean;
var
  StartUpInfo : TSTARTUPINFO;
const
  CreateFlags = DEBUG_PROCESS or CREATE_NEW_CONSOLE;
  
begin
  FillChar(StartUpInfo, SizeOf(StartupInfo), 0);
  StartUpInfo.cb := SizeOf(StartupInfo);
  StartUpInfo.dwFlags := STARTF_USESTDHANDLES or STARTF_USESHOWWINDOW;
  StartUpInfo.wShowWindow := SW_SHOWNORMAL or SW_SHOW;
                                                   
  System.FillChar(Info, sizeof(Info), 0);
  
  //todo:  CreateProcessW 
  Result := CreateProcess(nil, PChar(CmdLine), nil, nil, True, 
    CreateFlags, nil, nil, StartUpInfo, Info);
end;

procedure WinBreakPointToDbg(const Win: TDebugEvent; var Dbg: TDbgEvent);
begin
  Dbg.Kind := dek_BreakPoint;
  {$ifdef CPUI386}
  Dbg.Addr := TDbgPtr(Win.Exception.ExceptionRecord.ExceptionAddress);
  {$endif}
  Dbg.Thread := Win.dwThreadId;
end;

function DebugWinEvent(ProcessHandle: THandle; const Win: TDebugEvent): String;
var
  nm : String;
begin
  Result := '(ev = '+IntToStr(Win.dwDebugEventCode)+') ';
  case Win.dwDebugEventCode of
    EXCEPTION_DEBUG_EVENT: begin
      Result := Result + 'EXCEPTION';
    end;
    CREATE_THREAD_DEBUG_EVENT: begin
      Result := Result + '  CREATE_THREAD';
    end;
    CREATE_PROCESS_DEBUG_EVENT: begin
      Result := Result + 'CREATE_PROCESS';
      //writeln('baseofimage = ',  PtrUInt(Win.CreateProcessInfo.lpBaseOfImage), ' ', IntToHex(PtrUInt(Win.CreateProcessInfo.lpBaseOfImage), HexSize));
      //writeln('startaddr   = ',  PtrUInt(Win.CreateProcessInfo.lpStartAddress), ' ', IntToHex(PtrUInt(Win.CreateProcessInfo.lpStartAddress), HexSize));
      //writeln('imagename   = ',  PtrUInt(Win.CreateProcessInfo.lpImageName));
    end;
    EXIT_THREAD_DEBUG_EVENT: Result := Result + 'EXIT_THREAD';
    EXIT_PROCESS_DEBUG_EVENT: Result := Result + 'EXIT_PROCESS';

    LOAD_DLL_DEBUG_EVENT: begin
      //writeln('hFile     = ', PtrUInt(Win.LoadDll.hFile));
      //writeln('baseofdll = ', PtrUInt(Win.LoadDll.lpBaseOfDll), ' ',
      //                      IntToHex( PtrUInt(Win.LoadDll.lpBaseOfDll), hexsize));
      //writeln('debugInfo = ', PtrUInt(Win.LoadDll.dwDebugInfoFileOffset));
      //writeln('infoSize  = ', PtrUInt(Win.LoadDll.nDebugInfoSize));
      //writeln('imagename = ', PtrUInt(Win.LoadDll.lpImageName),' ',
      //                      IntToHex( PtrUInt(Win.LoadDll.lpImageName), hexsize));
      //writeln('isUnicode = ', PtrUInt(Win.LoadDll.fUnicode));
      
      Result := Result + 'LOAD_DLL';
      nm :=  ReadPCharAtPointer(ProcessHandle, TDbgPtr(Win.LoadDll.lpImageName), Boolean(Win.LoadDll.fUnicode));
      if nm <> '' then 
        Result := Result + ', dllname = '+ nm;
    end;
    UNLOAD_DLL_DEBUG_EVENT: Result := Result + 'UNLOAD_DLL';
    OUTPUT_DEBUG_STRING_EVENT: Result := Result + 'OUTPUT_DEBUG';
    RIP_EVENT: Result := Result + 'RIP_EVENT';
  else
    Result := 'UNKNOWN'; 
  end;
end;

procedure WinEventToDbgEvent(ProcessHandle: THandle; const Win: TDebugEvent; var Dbg: TDbgEvent);
begin
  Dbg.Debug := DebugWinEvent(ProcessHandle, Win);
  case Win.dwDebugEventCode of
    CREATE_PROCESS_DEBUG_EVENT:
      Dbg.Kind := dek_ProcessStart;
    EXIT_PROCESS_DEBUG_EVENT:     
      Dbg.Kind := dek_ProcessTerminated;
    EXCEPTION_DEBUG_EVENT:  
    begin
      case Win.Exception.ExceptionRecord.ExceptionCode  of
        EXCEPTION_BREAKPOINT: WinBreakPointToDbg(Win, Dbg);
      else
        Dbg.Kind := dek_Other;
      end;
    end;
  else
    Dbg.Kind := dek_Other;
  end;
end;

end.

