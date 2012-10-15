unit winDbgProc;

{$mode objfpc}{$H+}

interface

uses
  JwaWinBase, JwaTlHelp32, Windows, SysUtils, DbgTypes, dbgConsts;
  {, winTools32 - use FPC package, instead of custom tool}

type
  TFloatingSaveArea = packed record
    ControlWord   : Cardinal;
    StatusWord    : Cardinal;
    TagWord       : Cardinal;
    ErrorOffset   : Cardinal;
    ErrorSelector : Cardinal;
    DataOffset    : Cardinal;
    DataSelector  : Cardinal;
    RegisterArea  : array[0..79] of Byte;
    Cr0NpxState   : Cardinal;
  end;
  PFloatingSaveArea = ^TFloatingSaveArea;

  TContext32 = record
  { The flags values within this flag control the contents of
    a CONTEXT record.

    If the context record is used as an input parameter, then
    for each portion of the context record controlled by a flag
    whose value is set, it is assumed that that portion of the
    context record contains valid context. If the context record
    is being used to modify a threads context, then only that
    portion of the threads context will be modified.

    If the context record is used as an IN OUT parameter to capture
    the context of a thread, then only those portions of the thread's
    context corresponding to set flags will be returned.

    The context record is never used as an OUT only parameter. }

    ContextFlags: DWORD;

  { This section is specified/returned if CONTEXT_DEBUG_REGISTERS is
    set in ContextFlags.  Note that CONTEXT_DEBUG_REGISTERS is NOT
    included in CONTEXT_FULL. }

    Dr0       : DWORD;
    Dr1       : DWORD;
    Dr2       : DWORD;
    Dr3       : DWORD;
    Dr6       : DWORD;
    Dr7       : DWORD;

  { This section is specified/returned if the
    ContextFlags word contians the flag CONTEXT_FLOATING_POINT. }

    FloatSave : TFloatingSaveArea;

  { This section is specified/returned if the
    ContextFlags word contians the flag CONTEXT_SEGMENTS. }

    SegGs     : DWORD;
    SegFs     : DWORD;
    SegEs     : DWORD;
    SegDs     : DWORD;

  { This section is specified/returned if the
    ContextFlags word contians the flag CONTEXT_INTEGER. }

    Edi       : DWORD;
    Esi       : DWORD;
    Ebx       : DWORD;
    Edx       : DWORD;
    Ecx       : DWORD;
    Eax       : DWORD;

  { This section is specified/returned if the
    ContextFlags word contians the flag CONTEXT_CONTROL. }

    Ebp       : DWORD;
    Eip       : DWORD;
    SegCs     : DWORD;
    EFlags    : DWORD;
    Esp       : DWORD;
    SegSs     : DWORD;
  end;
  PContext32 = ^TContext32;


//
// Context Frame
//
//  This frame has a several purposes: 1) it is used as an argument to
//  NtContinue, 2) is is used to constuct a call frame for APC delivery,
//  and 3) it is used in the user level thread creation routines.
//
//
// The flags field within this record controls the contents of a CONTEXT
// record.
//
// If the context record is used as an input parameter, then for each
// portion of the context record controlled by a flag whose value is
// set, it is assumed that that portion of the context record contains
// valid context. If the context record is being used to modify a threads
// context, then only that portion of the threads context is modified.
//
// If the context record is used as an output parameter to capture the
// context of a thread, then only those portions of the thread's context
// corresponding to set flags will be returned.
//
// CONTEXT_CONTROL specifies SegSs, Rsp, SegCs, Rip, and EFlags.
//
// CONTEXT_INTEGER specifies Rax, Rcx, Rdx, Rbx, Rbp, Rsi, Rdi, and R8-R15.
//
// CONTEXT_SEGMENTS specifies SegDs, SegEs, SegFs, and SegGs.
//
// CONTEXT_DEBUG_REGISTERS specifies Dr0-Dr3 and Dr6-Dr7.
//
// CONTEXT_MMX_REGISTERS specifies the floating point and extended registers
//     Mm0/St0-Mm7/St7 and Xmm0-Xmm15).
//

//typedef struct DECLSPEC_ALIGN(16) _CONTEXT {
type
  {$note todo, fix alignment}
  M128A = record
    Low   : ULONGLONG;
    High  : LONGLONG;
  end;
  _M128A = M128A;
  TM128A = M128A;
  PM128A = TM128A;

  XMM_SAVE_AREA32 = record
    ControlWord     : WORD;
    StatusWord      : WORD;
    TagWord         : BYTE;
    Reserved1       : BYTE;
    ErrorOpcode     : WORD;
    ErrorOffset     : DWORD;
    ErrorSelector   : WORD;
    Reserved2       : WORD;
    DataOffset      : DWORD;
    DataSelector    : WORD;
    Reserved3       : WORD;
    MxCsr           : DWORD;
    MxCsr_Mask      : DWORD;
    FloatRegisters  : array[0..7] of M128A;
    XmmRegisters    : array[0..16] of M128A;
    Reserved4       : array[0..95] of BYTE;
  end;
  _XMM_SAVE_AREA32 = XMM_SAVE_AREA32;
  TXmmSaveArea = XMM_SAVE_AREA32;
  PXmmSaveArea = ^TXmmSaveArea;


  TContext64 = record
    // Register parameter home addresses.
    //
    // N.B. These fields are for convience - they could be used to extend the
    //      context record in the future.
    P1Home        : DWORD64;
    P2Home        : DWORD64;
    P3Home        : DWORD64;
    P4Home        : DWORD64;
    P5Home        : DWORD64;
    P6Home        : DWORD64;

    // Control flags.
    ContextFlags  : DWORD;
    MxCsr         : DWORD;

    // Segment Registers and processor flags.
    SegCs         : WORD;
    SegDs         : WORD;
    SegEs         : WORD;
    SegFs         : WORD;
    SegGs         : WORD;
    SegSs         : WORD;
    EFlags        : DWORD;

    // Debug registers
    Dr0           : DWORD64;
    Dr1           : DWORD64;
    Dr2           : DWORD64;
    Dr3           : DWORD64;
    Dr6           : DWORD64;
    Dr7           : DWORD64;

    // Integer registers.
    Rax           : DWORD64;
    Rcx           : DWORD64;
    Rdx           : DWORD64;
    Rbx           : DWORD64;
    Rsp           : DWORD64;
    Rbp           : DWORD64;
    Rsi           : DWORD64;
    Rdi           : DWORD64;
    R8            : DWORD64;
    R9            : DWORD64;
    R10           : DWORD64;
    R11           : DWORD64;
    R12           : DWORD64;
    R13           : DWORD64;
    R14           : DWORD64;
    R15           : DWORD64;

    // Program counter.
    Rip           : DWORD64;

    // Floating point state.
    FltSave       : XMM_SAVE_AREA32; // MWE: only translated the FltSave part of the union

    // Vector registers.
    VectorRegister  : array[0..25] of M128A;
    VectorControl   : DWORD64;

    // Special debug control registers.
    DebugControl          : DWORD64;
    LastBranchToRip       : DWORD64;
    LastBranchFromRip     : DWORD64;
    LastExceptionToRip    : DWORD64;
    LastExceptionFromRip  : DWORD64;
  end;


const
  hexsize = sizeof(TDbgPtr)*2;

function CreateDebugProcess(const CmdLine: String; OnlyProcess: Boolean; out Info: TProcessInformation): Boolean;

function ReadProcMem(dwProc: THandle; Offset : TDbgPtr; Count: Integer; var data: array of byte): Integer;
function WriteProcMem(dwProc: THandle; Offset : TDbgPtr; Count: Integer; const data: array of byte): Integer;

function DebugWinEvent(ProcessHandle: THandle; const Win: TDebugEvent): String;
procedure WinEventToDbgEvent(const WinEvent: TDebugEvent; var Dbg: TDbgEvent);

function DoReadThreadRegs32(ThreadHandle: THandle; Regs: TDbgDataList): Boolean;
function DoWriteThreadRegs32(ThreadHandle: THandle; Regs: TDbgDataList): Boolean;

function DoReadThreadRegs64(ThreadHandle: THandle; Regs: TDbgDataList): Boolean;
function DoWriteThreadRegs64(ThreadHandle: THandle; Regs: TDbgDataList): Boolean;

function SetThread32SingleStep(ThreadHandle: THandle): Boolean;

function SuspendProcess(AProcID: LongWord): Boolean;
function ResumeProcess(AProcID: LongWord): Boolean;

var
  Wow64GetThreadContext : function( hThread: THANDLE; var lpContext ): LongBool; stdcall = nil;
  Wow64SuspendThread    : function( hThread: THANDLE ): DWORD; stdcall = nil;
  Wow64SetThreadContext : function ( hThread: THANDLE; const lpContext ): LongBool; stdcall = nil;

function IntGetThreadContext(hThread: HANDLE; var lpContext): BOOL; stdcall; external kernel32 name 'GetThreadContext';
function IntSetThreadContext(hThread: THandle; const lpContext): BOOL; external kernel32 name 'SetThreadContext';


implementation

const
  SIZE_OF_80387_REGISTERS = 80;
  { Values for contextflags  }
  CONTEXT_i386 = $10000;    // this assumes that i386 and
  CONTEXT_i486 = $10000;    // i486 have identical context records

  CONTEXT32_CONTROL            = CONTEXT_i386 or 1;              // SS:SP, CS:IP, FLAGS, BP
  CONTEXT32_INTEGER            = CONTEXT_i386 or 2;              // AX, BX, CX, DX, SI, DI
  CONTEXT32_SEGMENTS           = CONTEXT_i386 or 4;             // DS, ES, FS, GS
  CONTEXT32_FLOATING_POINT     = CONTEXT_i386 or 8;       // 387 state
  CONTEXT32_DEBUG_REGISTERS    = CONTEXT_i386 or $10;    // DB 0-3,6,7
  CONTEXT32_EXTENDED_REGISTERS = CONTEXT_i386 or $20; // cpu specific extensions
  CONTEXT32_FULL               = (CONTEXT32_CONTROL or CONTEXT32_INTEGER) or CONTEXT32_SEGMENTS;
  { our own invention  }
  CONTEXT32_DEBUGGER           = CONTEXT32_FULL or CONTEXT32_FLOATING_POINT;
  CONTEXT32_ALL = //todo:  {CONTEXT32_FLOATING_POINT or}
                CONTEXT32_DEBUG_REGISTERS or
                CONTEXT32_SEGMENTS or CONTEXT32_INTEGER or CONTEXT32_CONTROL;
  FLAG32_TRACE_BIT = $100;

const
  INITIAL_MXCSR = $1f80;            // initial MXCSR value
  INITIAL_FPCSR = $027f;            // initial FPCSR value

  CONTEXT_AMD64 = $100000;

  CONTEXT64_CONTROL         = (CONTEXT_AMD64 or $00000001);
  CONTEXT64_INTEGER         = (CONTEXT_AMD64 or $00000002);
  CONTEXT64_SEGMENTS        = (CONTEXT_AMD64 or $00000004);
  CONTEXT64_FLOATING_POINT  = (CONTEXT_AMD64 or $00000008);
  CONTEXT64_DEBUG_REGISTERS = (CONTEXT_AMD64 or $00000010);

  CONTEXT64_FULL            = (CONTEXT64_CONTROL or CONTEXT64_INTEGER or CONTEXT64_FLOATING_POINT);
  CONTEXT64_ALL             = (CONTEXT64_CONTROL or CONTEXT64_INTEGER or CONTEXT64_SEGMENTS or
                               CONTEXT64_FLOATING_POINT or CONTEXT64_DEBUG_REGISTERS);

  CONTEXT64_EXCEPTION_ACTIVE    = $08000000;
  CONTEXT64_SERVICE_ACTIVE      = $10000000;
  CONTEXT64_EXCEPTION_REQUEST   = $40000000;
  CONTEXT64_EXCEPTION_REPORTING = $80000000;


const
  kernel32 = 'kernel32.dll';

function DoReadThreadRegs32(ThreadHandle: THandle; Regs: TDbgDataList): Boolean;
var
  ctx32 : TContext32;
begin
  FillChar(ctx32, sizeof(ctx32), 0);
  ctx32.ContextFlags := CONTEXT32_ALL;

  Result := IntGetThreadContext(ThreadHandle, ctx32);
  if not Result then Exit;

  with ctx32 do begin
    Regs.Reg[_Edi].UInt32 := Edi;
    Regs.Reg[_Esi].UInt32 := Esi;
    Regs.Reg[_Ebx].UInt32 := Ebx;
    Regs.Reg[_Edx].UInt32 := Edx;
    Regs.Reg[_Ecx].UInt32 := Ecx;
    Regs.Reg[_Eax].UInt32 := Eax;

    Regs.Reg[_Gs].UInt32 := SegGs;
    Regs.Reg[_Fs].UInt32 := SegFs;
    Regs.Reg[_Es].UInt32 := SegEs;
    Regs.Reg[_Ds].UInt32 := SegDs;
    Regs.Reg[_Ss].UInt32 := SegSs;
    Regs.Reg[_Cs].UInt32 := SegCs;

    Regs.Reg[_Ebp].UInt32 := Ebp;
    Regs.Reg[_Eip].UInt32 := Eip;
    Regs.Reg[_EFlags].UInt32 := EFlags;
    Regs.Reg[_Esp].UInt32 := Esp;

    Regs.Reg[_Dr0].UInt32 := Dr0;
    Regs.Reg[_Dr1].UInt32 := Dr1;
    Regs.Reg[_Dr2].UInt32 := Dr2;
    Regs.Reg[_Dr3].UInt32 := Dr3;
    Regs.Reg[_Dr6].UInt32 := Dr6;
    Regs.Reg[_Dr7].UInt32 := Dr7;
  end;
end;


function DoWriteThreadRegs32(ThreadHandle: THandle; Regs: TDbgDataList): Boolean;
var
  ctx32 : TContext32;
begin
  FillChar(ctx32, sizeof(ctx32), 0);
  ctx32.ContextFlags := CONTEXT32_ALL;
  
  with ctx32 do begin
    Edi := Regs.Reg[_Edi].UInt32;
    Esi := Regs.Reg[_Esi].UInt32;
    Ebx := Regs.Reg[_Ebx].UInt32;
    Edx := Regs.Reg[_Edx].UInt32;
    Ecx := Regs.Reg[_Ecx].UInt32;
    Eax := Regs.Reg[_Eax].UInt32;

    SegGs := Regs.Reg[_Gs].UInt32;
    SegFs := Regs.Reg[_Fs].UInt32;
    SegEs := Regs.Reg[_Es].UInt32;
    SegDs := Regs.Reg[_Ds].UInt32;
    SegSs := Regs.Reg[_Ss].UInt32;
    SegCs := Regs.Reg[_Cs].UInt32;

    Ebp := Regs.Reg[_Ebp].UInt32;
    Eip := Regs.Reg[_Eip].UInt32;
    EFlags := Regs.Reg[_EFlags].UInt32;
    Esp := Regs.Reg[_Esp].UInt32;

    Dr0 := Regs.Reg[_Dr0].UInt32;
    Dr1 := Regs.Reg[_Dr1].UInt32;
    Dr2 := Regs.Reg[_Dr2].UInt32;
    Dr3 := Regs.Reg[_Dr3].UInt32;
    Dr6 := Regs.Reg[_Dr6].UInt32;
    Dr7 := Regs.Reg[_Dr7].UInt32;
  end;

  Result := IntSetThreadContext(ThreadHandle, ctx32);
  //if not Result then
  //  WriteLn('SetThreadContext = ', GetLastError);
end;

function DoReadThreadRegs64(ThreadHandle: THandle; Regs: TDbgDataList): Boolean;
{var
  ctx   : TContext64;}
begin
  Result:=False;
end;

function DoWriteThreadRegs64(ThreadHandle: THandle; Regs: TDbgDataList): Boolean;
begin
  Result:=False;
end;

function SetThread32SingleStep(ThreadHandle: THandle): Boolean;
var
  ctx : TContext32;
begin
  FillChar(ctx, sizeof(ctx), 0);
  ctx.ContextFlags := CONTEXT32_CONTROL;
  Result := IntGetThreadContext(ThreadHandle, ctx);
  if not Result then Exit;
  
  ctx.ContextFlags := CONTEXT32_CONTROL;
  ctx.EFlags := ctx.EFlags or FLAG32_TRACE_BIT;
  
  // PContext(@ctx)^ is a bit hacky 
  Result := IntSetThreadContext(ThreadHandle, ctx);
end;


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
  Result:='';
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
  if PointerOffset = 0 then Exit;
  
  ptr := ReadPointerSize(dwProc, PointerOffset);
  if ptr = 0 then Exit
  else  Result := ReadPCharAtProc(dwProc, ptr, isUnicode);
end;

function ReadProcMem(dwProc: THandle; Offset : TDbgPtr; Count: Integer; var data: array of byte): Integer;
var
  res : LongWord;
  //err : LongWord;
begin
  if not ReadProcessMemory(dwProc, Pointer(Offset), @data[0], Count, res) then begin
    //err:=GetLastError;
    Result := -1;
  end else
    Result := res;  
end;

function WriteProcMem(dwProc: THandle; Offset : TDbgPtr; Count: Integer; const data: array of byte): Integer;
var
  res : LongWord;
begin
  if not WriteProcessMemory(dwProc, Pointer(Offset), @data[0], Count, res) then begin
    Result := -1;
  end else
    Result := res;
end;


function CreateDebugProcess(const CmdLine: String; OnlyProcess: Boolean; out Info: TProcessInformation): Boolean;
var
  StartUpInfo : TSTARTUPINFO;
  CreateFlags : DWORD;
  
begin
  CreateFlags:=DEBUG_PROCESS or CREATE_NEW_CONSOLE;
  if OnlyProcess then CreateFlags:=CreateFlags or DEBUG_ONLY_THIS_PROCESS;
  
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

function DebugWinExcpetionCode(Code: LongWord): String;
begin
  case Code of
    EXCEPTION_ACCESS_VIOLATION:         Result := 'ACCESS_VIOLATION';
    EXCEPTION_DATATYPE_MISALIGNMENT:    Result := 'DATATYPE_MISALIGNMENT';
    EXCEPTION_BREAKPOINT:               Result := 'BREAKPOINT';
    EXCEPTION_SINGLE_STEP:              Result := 'SINGLE_STEP';
    EXCEPTION_ARRAY_BOUNDS_EXCEEDED:    Result := 'ARRAY_BOUNDS_EXCEEDED';
    EXCEPTION_FLT_DENORMAL_OPERAND:     Result := 'FLT_DENORMAL_OPERAND';
    EXCEPTION_FLT_DIVIDE_BY_ZERO:       Result := 'FLT_DIVIDE_BY_ZERO';
    EXCEPTION_FLT_INEXACT_RESULT:       Result := 'FLT_INEXACT_RESULT';
    EXCEPTION_FLT_INVALID_OPERATION:    Result := 'FLT_INVALID_OPERATION';
    EXCEPTION_FLT_OVERFLOW:             Result := 'FLT_OVERFLOW';
    EXCEPTION_FLT_STACK_CHECK:          Result := 'FLT_STACK_CHECK';
    EXCEPTION_FLT_UNDERFLOW:            Result := 'FLT_UNDERFLOW';
    EXCEPTION_INT_DIVIDE_BY_ZERO:       Result := 'INT_DIVIDE_BY_ZERO';
    EXCEPTION_INT_OVERFLOW:             Result := 'INT_OVERFLOW';
    EXCEPTION_PRIV_INSTRUCTION:         Result := 'PRIV_INSTRUCTION';
    EXCEPTION_IN_PAGE_ERROR:            Result := 'IN_PAGE_ERROR';
    EXCEPTION_ILLEGAL_INSTRUCTION:      Result := 'ILLEGAL_INSTRUCTION';
    EXCEPTION_NONCONTINUABLE_EXCEPTION: Result := 'NONCONTINUABLE_EXCEPTION';
    EXCEPTION_STACK_OVERFLOW:           Result := 'STACK_OVERFLOW';
    EXCEPTION_INVALID_DISPOSITION:      Result := 'INVALID_DISPOSITION';
    EXCEPTION_GUARD_PAGE:               Result := 'GUARD_PAGE';
    EXCEPTION_INVALID_HANDLE:           Result := 'INVALID_HANDLE';
    //CONTROL_C_EXIT: Result := ''; // find CONTROL_C_EXIT declaration
  else
    Result := 'Unknown: $' + IntToHex(Code, 8);
  end;
end;

function DebugWinEvent(ProcessHandle: THandle; const Win: TDebugEvent): String;
var
  nm : String;
begin
  Result := '';
  case Win.dwDebugEventCode of
    EXCEPTION_DEBUG_EVENT: begin
      Result := Result + 'EXCEPTION';
      if Win.Exception.dwFirstChance <> 0 then
        Result := Result + ' first chance'
      else
        Result := Result + ' last chance';
      Result := Result+#10#13+
        Format('Code:   %s', [DebugWinExcpetionCode(Win.Exception.ExceptionRecord.ExceptionCode)])+' '+
        Format('Flags:  %d', [Win.Exception.ExceptionRecord.ExceptionFlags])+' '+
        Format('Addr:   %x', [PtrUInt(Win.Exception.ExceptionRecord.ExceptionAddress)])+' '+
        Format('Params: %d', [Win.Exception.ExceptionRecord.NumberParameters]);
    end;
    CREATE_THREAD_DEBUG_EVENT: begin
      Result := Result + 'CREATE_THREAD';
    end;
    CREATE_PROCESS_DEBUG_EVENT: begin
      Result := Result + 'CREATE_PROCESS';
    end;
    EXIT_THREAD_DEBUG_EVENT: Result := Result + 'EXIT_THREAD';
    EXIT_PROCESS_DEBUG_EVENT: Result := Result + 'EXIT_PROCESS';

    LOAD_DLL_DEBUG_EVENT, UNLOAD_DLL_DEBUG_EVENT: begin
      if Win.dwDebugEventCode = LOAD_DLL_DEBUG_EVENT then
        Result := Result + 'LOAD_DLL '
      else
        Result := Result + 'UNLOAD_DLL ';
      nm := ReadPCharAtPointer(ProcessHandle, TDbgPtr(Win.LoadDll.lpImageName), Boolean(Win.LoadDll.fUnicode));
      Result:=Result+', name image = ' + IntToStr(TDbgPtr(Win.LoadDll.lpImageName));
      if nm <> '' then
        Result := Result + ', dllname = '+ nm;
    end;
    OUTPUT_DEBUG_STRING_EVENT: Result := Result + 'OUTPUT_DEBUG';
    RIP_EVENT: Result := Result + 'RIP_EVENT';
  else
    Result := 'UNKNOWN'; 
  end;
end;

procedure WinEventToDbgEvent(const WinEvent: TDebugEvent; var Dbg: TDbgEvent);
begin
  Dbg.Thread := WinEvent.dwThreadId;
  Dbg.Process := WinEvent.dwProcessId;
  case WinEvent.dwDebugEventCode of
    CREATE_PROCESS_DEBUG_EVENT:
      Dbg.Kind := dek_ProcessStart;
    EXIT_PROCESS_DEBUG_EVENT:     
      Dbg.Kind := dek_ProcessTerminated;
    EXCEPTION_DEBUG_EVENT:  
    begin
      case WinEvent.Exception.ExceptionRecord.ExceptionCode of
        EXCEPTION_BREAKPOINT: 
          WinBreakPointToDbg(WinEvent, Dbg);
        EXCEPTION_SINGLE_STEP: 
          dbg.Kind := dek_SingleStep;
      else
        Dbg.Kind := dek_SysExc;
      end;
      Dbg.Addr := TDbgPtr(WinEvent.Exception.ExceptionRecord.ExceptionAddress);
    end;
  else
    Dbg.Kind := dek_SysCall;
  end;
end;

function SuspendProcess(AProcID: LongWord): Boolean;
var
  Snap  : THandle;
  entry : TThreadEntry32;
  thr   : THandle;
begin
  //any hacks?
  Result:=False;
  //if not Assigned(CreateToolhelp32Snapshot) then Exit;

  Snap:=CreateToolhelp32Snapshot(TH32CS_SNAPTHREAD, AProcID);
  if Snap<>0 then Exit;

  Thread32First(Snap, entry);
  thr:=OpenThread(THREAD_SUSPEND_RESUME, True, entry.th32ThreadID);
  if thr<>INVALID_HANDLE_VALUE then begin
    SuspendThread(thr);
    CloseHandle(thr);
  end;
  CloseHandle(Snap);
end;

function ResumeProcess(AProcID: LongWord): Boolean;
var
  Snap  : THandle;
  entry : TThreadEntry32;
  thr   : THandle;
begin
  //any hacks?
  Result:=False;
  //if not Assigned(CreateToolhelp32Snapshot) then Exit;

  //todo: check 64-bit compatibility
  Snap:=CreateToolhelp32Snapshot(TH32CS_SNAPTHREAD, AProcID);
  if Snap<>0 then Exit;

  Thread32First(Snap, entry);
  thr:=OpenThread(THREAD_SUSPEND_RESUME, True, entry.th32ThreadID);
  if thr<>INVALID_HANDLE_VALUE then begin
    ResumeThread(thr);
    CloseHandle(thr);
  end;
  CloseHandle(Snap);
end;

initialization


end.

