unit winTools32;

interface

{$ifdef fpc}{$mode delphi}{$h+}{$endif}

uses
  Windows;

type
  TThreadEntry32 = packed record
    dwSize              : DWORD;
    cntUsage            : DWORD;
    th32ThreadID        : DWORD;
    th32OwnerProcessID  : DWORD;
    tpBasePri           : LONG;
    tpDeltaPri          : LONG;
    dwFlags             : DWORD;
  end;
  PHreadEntry2=^TThreadEntry32;

const
  { Indicates that the snapshot handle is to be inheritable. }
  TH32CS_INHERIT      = $80000000;

  { Includes all heaps of the process specified in th32ProcessID  }
  { in the snapshot. To enumerate the heaps, see Heap32ListFirst. }
  TH32CS_SNAPHEAPLIST = $00000001;

  { Includes all processes in the system in the snapshot.         }
  { To enumerate the processes, see Process32First.               }
  TH32CS_SNAPPROCESS  = $00000002;

  { Includes all threads in the system in the snapshot.                  }
  { To enumerate the threads, see Thread32First.                         }
  {                                                                      }
  { To identify the threads that belong to a specific process, compare   }
  { its process identifier to the th32OwnerProcessID member of           }
  { the THREADENTRY32 structure when enumerating the threads.            }
  TH32CS_SNAPTHREAD   = $00000004;

  { Includes all modules of the process specified in th32ProcessID in the snapshot.     }
  { To enumerate the modules, see Module32First.                                        }
  { If the function fails with ERROR_BAD_LENGTH, retry the function until it succeeds.  }
  {                                                                                     }
  { 64-bit Windows:  Using this flag in a 32-bit process includes the 32-bit            }
  {    modules of the process specified in th32ProcessID, while using it in             }
  {    a 64-bit process includes the 64-bit modules. To include the                     }
  {    32-bit modules of the process specified in th32ProcessID from a 64-bit process,  }
  {    use the TH32CS_SNAPMODULE32 flag }
  TH32CS_SNAPMODULE   = $00000008;

  { Includes all 32-bit modules of the process specified in th32ProcessID        }
  { in the snapshot when called from a 64-bit process. This flag can be combined }
  { with TH32CS_SNAPMODULE or TH32CS_SNAPALL. If the function fails with         }
  { ERROR_BAD_LENGTH, retry the function until it succeeds.                      }
  TH32CS_SNAPMODULE32 = $00000010;

  { Includes all processes and threads in the system, plus the heaps and modules       }
  { of the process specified in th32ProcessID. Equivalent to specifying the           }
  { TH32CS_SNAPHEAPLIST, TH32CS_SNAPMODULE, TH32CS_SNAPPROCESS, and TH32CS_SNAPTHREAD }
  { values combined using an OR operation ('|').}
  TH32CS_SNAPALL = TH32CS_SNAPHEAPLIST or
                   TH32CS_SNAPMODULE or
                   TH32CS_SNAPPROCESS or
                   TH32CS_SNAPTHREAD or
                   TH32CS_SNAPMODULE32;
const
  kernellib = 'kernel32.dll';

var
  CreateToolhelp32Snapshot : function (dwFlags, th32ProcessID: DWORD): THandle; stdcall = nil;
  Thread32First : function (hSnapshot : THandle; var lpte: TThreadEntry32): WinBool; stdcall = nil;
  Thread32Next  : function (hSnapshot : THandle; var lpte: TThreadEntry32): WinBool; stdcall = nil;

implementation

procedure LoadToolFuncs;
var
  lib : HINST;
begin
  lib:=LoadLibraryA('kernel32.dll');
  CreateToolhelp32Snapshot:=GetProcAddress(lib, 'CreateToolhelp32Snapshot');
  Thread32First:=GetProcAddress(lib, 'Thread32First');
  Thread32Next:=GetProcAddress(lib, 'Thread32Next');
  FreeLibrary(lib);
end;

initialization
  LoadToolFuncs;
end.
