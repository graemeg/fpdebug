unit winDbgProc; 

{$mode objfpc}{$H+}

interface

uses
  Windows, DbgTypes;

function CreateDebugProcess(const CmdLine: string; out Info: TProcessInformation): Boolean;

procedure WinEventToDbgEvent(const Win: TDebugEvent; out Dbg: TDbgEvent);

implementation


function CreateDebugProcess(const CmdLine: string; out Info: TProcessInformation): Boolean;
var
  StartUpInfo : TSTARTUPINFO;
begin
  FillChar(StartUpInfo, SizeOf(StartupInfo), 0);
  StartUpInfo.cb := SizeOf(StartupInfo);
  StartUpInfo.dwFlags := {STARTF_USESTDHANDLES or} STARTF_USESHOWWINDOW;
  StartUpInfo.wShowWindow := SW_SHOWNORMAL or SW_SHOW;

  System.FillChar(Info, sizeof(Info), 0);
  
  //todo:  CreateProcessW 
  Result := CreateProcess(nil, PChar(CmdLine), nil, nil, True, 
    DETACHED_PROCESS or DEBUG_PROCESS or CREATE_NEW_PROCESS_GROUP, nil, nil, StartUpInfo, Info);
end;


procedure WinEventToDbgEvent(const Win: TDebugEvent; out Dbg: TDbgEvent);
begin
  writeln('code = ',Win.dwDebugEventCode);
  case Win.dwDebugEventCode of
    CREATE_PROCESS_DEBUG_EVENT:
      Dbg.Kind := dek_ProcessStart;
    EXIT_PROCESS_DEBUG_EVENT:     
      Dbg.Kind := dek_ProcessTerminated;
  else
    Dbg.Kind := dek_Other;
  end;
end;

end.

