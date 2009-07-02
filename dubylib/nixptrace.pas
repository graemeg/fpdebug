{  `ptrace' debugger support interface.  Linux version.
   Copyright (C) 1996-1999,2000,2006,2007 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, write to the Free
   Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
   02111-1307 USA.  }

unit nixPtrace;

{$ifdef fpc}{$mode delphi}{$h+}{$endif}

interface

uses
  SysUtils, //todo: remove SysUtils
  BaseUnix{, syscall};

// 08048000

{$linklib libc}

{  Core file format: The core file is written in such a way that gdb
   can understand it and provide useful information to the user (under
   linux we use the 'trad-core' bfd).  There are quite a number of
   obstacles to being able to view the contents of the floating point
   registers, and until these are solved you will not be able to view the
   contents of them.  Actually, you can read in the core file and look at
   the contents of the user struct to find out what the floating point
   registers contain.

   The actual file contents are as follows:

   UPAGE: 1 page consisting of a user struct that tells gdb what is present
   in the file.  Directly after this is a copy of the task_struct, which
   is currently not used by gdb, but it may come in useful at some point.
   All of the registers are stored as part of the upage.  The upage should
   always be only one page.

   DATA: The data area is stored.  We use current->end_text to
   current->brk to pick up all of the user variables, plus any memory
   that may have been malloced.  No attempt is made to determine if a page
   is demand-zero or if a page is totally unused, we just cover the entire
   range.  All of the addresses are rounded in such a way that an integral
   number of pages is written.

   STACK: We need the stack information in order to get a meaningful
   backtrace.  We need to write the data from (esp) to
   current->start_stack, so we round each of these off in order to be able
   to write an integer number of pages.
   The minimum core file size is 3 pages, or 12288 bytes.
}

{$ifdef CPU64}
  { 8*16 bytes for each FP-reg = 128 bytes  }
  { 16*16 bytes for each XMM-reg = 256 bytes  }

type
  user_fpregs_struct = packed record
    cwd   : uint16_t;
    swd   : uint16_t;
    ftw   : uint16_t;
    fop   : uint16_t;
    rip   : uint64_t;
    rdp   : uint64_t;
    mxcsr : uint32_t;
    mxcr_mask : uint32_t;
    st_space  : array[0..31] of uint32_t;
    xmm_space : array[0..63] of uint32_t;
    padding   : array[0..23] of uint32_t;
  end;

  user_regs_struct = packed record
    r15, r14, r13, r12 : dword;
    rbp, rbx           : dword;
    r11, r10, r9, r8   : dword;
    rax, rcx, rdx      : dword;
    rsi, rdi           : dword;
    orig_rax           : dword;
    rip     : dword;
    cs      : dword;
    eflags  : dword;
    rsp     : dword;
    ss      : dword;
    fs_base : dword;
    gs_base : dword;
    ds, es, fs, gs : dword;
  end;

  { When the kernel dumps core, it starts by dumping the user struct -
   this will be used by gdb to figure out where the data and stack segments
   are within the file, and what virtual addresses to use. }

  Tuser = packed record
    regs        : user_regs_struct;   { We start with the registers, to mimic the way that }
                                      { "memory" is returned from the ptrace(3,...) function.  }
    u_fpvalid   : longint;            { True if math co-processor being used. }
                                      { for this mess. Not yet used. }
    i387        : user_fpregs_struct; { Math Co-processor registers. }

    { The rest of this junk is to help gdb figure out what goes where }
    u_tsize     : dword;   { Text segment size (pages).  }
    u_dsize     : dword;   { Data segment size (pages).  }
    u_ssize     : dword;   { Stack segment size (pages). }
    start_code  : dword;   { Starting virtual address of text. }
    start_stack : dword;   { Starting virtual address of stack area. }
                  				 { This is actually the bottom of the stack, }
                				   { the top of the stack is always found in the esp register. }
    signal      : longint; { Signal that caused the core dump. }
    reserved    : longint;   { No longer used}
    u_ar0       : ^user_regs_struct;    { Used by gdb to help find the values for the registers }
    u_fpstate   : ^user_fpregs_struct;  { Math Co-processor pointer. }
    magic       : dword;                { To uniquely identify a core file }
    u_comm      : array[0..31] of char; { User command that was responsible }
    u_debugreg  : array[0..7] of dword; { x86 debug registers? }
  end;

{$else}
  { These are the 32-bit x86 structures.   }

type
  user_fpregs_struct = packed record
    cwd, swd, twd : longint;
    fip, fcs, foo : longint;
    fos           : longint;
    st_space      : array[0..19] of longint;
  end;

  { 8*16 bytes for each FP-reg = 128 bytes  }
  { 8*16 bytes for each XMM-reg = 128 bytes  }
  user_fpxregs_struct = packed record
    cwd : word;
    swd : word;
    twd : word;
    fop : word;
    fip : longint;
    fcs : longint;
    foo : longint;
    fos : longint;
    mxcsr     : longint;
    reserved  : longint;
    st_space  : array[0..31] of longint;
    xmm_space : array[0..31] of longint;
    padding   : array[0..55] of longint;
  end;

  user_regs_struct = packed record
    ebx, ecx, edx      : longint;
    esi, edi           : longint;
    ebp                : longint;
    eax                : longint;
    xds, xes, xfs, xgs : longint;
    orig_eax  : longint;
    eip       : longint;
    xcs       : longint;
    eflags    : longint;
    esp       : longint;
    xss       : longint;
  end;

  Tuser = packed record
    regs        : user_regs_struct;
    u_fpvalid   : longint;
    i387        : user_fpregs_struct;
    u_tsize     : dword;
    u_dsize     : dword;
    u_ssize     : dword;
    start_code  : dword;
    start_stack : dword;
    signal      : longint;
    reserved    : longint;
    u_ar0       : ^user_regs_struct;
    u_fpstate   : ^user_fpregs_struct;
    magic       : dword;
    u_comm      : array[0..31] of char;
    u_debugreg  : array[0..7] of longint;
  end;

{$endif}

type
  Tptrace_request = Integer;

const
{ Type of the REQUEST argument to `ptrace.'  }
  { Indicate that the process making this request should be traced.
     All signals received by this process can be intercepted by its
     parent, and its parent can use the other `ptrace' requests.  }
  PTRACE_TRACEME = 0;
  PT_TRACE_ME = PTRACE_TRACEME;

  { Return the word in the process's text space at address ADDR.  }
  PTRACE_PEEKTEXT = 1;
  PT_READ_I = PTRACE_PEEKTEXT;

  { Return the word in the process's data space at address ADDR.  }
  PTRACE_PEEKDATA = 2;
  PT_READ_D = PTRACE_PEEKDATA;

  { Return the word in the process's user area at offset ADDR.  }
  PTRACE_PEEKUSER = 3;
  PT_READ_U = PTRACE_PEEKUSER;

  { Write the word DATA into the process's text space at address ADDR.  }
  PTRACE_POKETEXT = 4;
  PT_WRITE_I = PTRACE_POKETEXT;

  { Write the word DATA into the process's data space at address ADDR.  }
  PTRACE_POKEDATA = 5;
  PT_WRITE_D = PTRACE_POKEDATA;

  { Write the word DATA into the process's user area at offset ADDR.  }
  PTRACE_POKEUSER = 6;
  PT_WRITE_U = PTRACE_POKEUSER;

  { Continue the process.  }
  PTRACE_CONT = 7;
  PT_CONTINUE = PTRACE_CONT;

  { Kill the process.  }
  PTRACE_KILL = 8;
  PT_KILL = PTRACE_KILL;

  { Single step the process.
     This is not supported on all machines.  }
  PTRACE_SINGLESTEP = 9;
  PT_STEP = PTRACE_SINGLESTEP;

  { Get all general purpose registers used by a processes.
     This is not supported on all machines.  }
  PTRACE_GETREGS = 12;
  PT_GETREGS = PTRACE_GETREGS;

  { Set all general purpose registers used by a processes.
     This is not supported on all machines.  }
  PTRACE_SETREGS = 13;
  PT_SETREGS = PTRACE_SETREGS;

  { Get all floating point registers used by a processes.
     This is not supported on all machines.  }
  PTRACE_GETFPREGS = 14;
  PT_GETFPREGS = PTRACE_GETFPREGS;

  { Set all floating point registers used by a processes.
     This is not supported on all machines.  }
  PTRACE_SETFPREGS = 15;
  PT_SETFPREGS = PTRACE_SETFPREGS;

  { Attach to a process that is already running. }
  PTRACE_ATTACH = 16;
  PT_ATTACH = PTRACE_ATTACH;

  { Detach from a process attached to with PTRACE_ATTACH.  }
  PTRACE_DETACH = 17;
  PT_DETACH = PTRACE_DETACH;

  { Get all extended floating point registers used by a processes.
     This is not supported on all machines.  }
  PTRACE_GETFPXREGS = 18;
  PT_GETFPXREGS = PTRACE_GETFPXREGS;

  { Set all extended floating point registers used by a processes.
     This is not supported on all machines.  }
  PTRACE_SETFPXREGS = 19;
  PT_SETFPXREGS = PTRACE_SETFPXREGS;

  { Continue and stop at the next (return from) syscall.  }
  PTRACE_SYSCALL = 24;
  PT_SYSCALL = PTRACE_SYSCALL;

  { Set ptrace filter options.  }
  PTRACE_SETOPTIONS = $4200;
  PT_SETOPTIONS = PTRACE_SETOPTIONS;

  { Get last ptrace message.  }
  PTRACE_GETEVENTMSG = $4201;
  PT_GETEVENTMSG = PTRACE_GETEVENTMSG;

  { Get siginfo for process.  }
  PTRACE_GETSIGINFO = $4202;
  PT_GETSIGINFO = PTRACE_GETSIGINFO;

  { Set new siginfo for process.  }
  PTRACE_SETSIGINFO = $4203;
  PT_SETSIGINFO = PTRACE_SETSIGINFO;

{  Options set using PTRACE_SETOPTIONS.  }
type
  Tptrace_setoptions = Integer;

const
  PTRACE_O_TRACESYSGOOD	= $00000001;
  PTRACE_O_TRACEFORK    = $00000002;
  PTRACE_O_TRACEVFORK   = $00000004;
  PTRACE_O_TRACECLONE   = $00000008;
  PTRACE_O_TRACEEXEC      = $00000010;
  PTRACE_O_TRACEVFORKDONE = $00000020;
  PTRACE_O_TRACEEXIT      = $00000040;
  PTRACE_O_MASK           = $0000007f;

{ Wait extended result codes for the above trace options.  }
type
  Tptrace_eventcodes = Integer;

const
  PTRACE_EVENT_FORK       = 1;
  PTRACE_EVENT_VFORK      = 2;
  PTRACE_EVENT_CLONE      = 3;
  PTRACE_EVENT_EXEC       = 4;
  PTRACE_EVENT_VFORK_DONE = 5;
  PTRACE_EVENT_EXIT       = 6;

{  Perform process tracing functions.  REQUEST is one of the values
   above, and determines the action to be taken.
   For all requests except PTRACE_TRACEME, PID specifies the process to be
   traced.

   PID and the other arguments described above for the various requests should
   appear (those that are used for the particular request) as:
     pid_t PID, void *ADDR, int DATA, void *ADDR2
   after REQUEST.  }

//function ptrace(request: Tptrace_request; args: array of const): Integer; cdecl; external;

type
  TPtraceWord = Integer;
  PPtraceWord = ^TPtraceWord;

function ptrace(request: Tptrace_request; pid: TPid; addr, data: Pointer): TPtraceWord;
 cdecl; external name 'ptrace';

function ptraceMe: TPtraceWord;
function ptraceCont(pid: TPid; Signal: Integer): TPtraceWord;
function ptraceGetSigInfo(pid: TPid; var siginfo: tsiginfo): TPtraceWord;
function ptracePeekData(pid: TPid; offset: QWord; var w: TPtraceWord): Boolean;
function ptracePeekUser(pid: TPid; offset: QWord; var w: TPtraceWord): Boolean;
function ptracePeekSysUser(pid: TPid; var userdata: Tuser): Boolean;

implementation

{function ptrace(request: Tptrace_request; pid: TPid; addr, data: Pointer): TPtraceWord;
begin
  Result := Do_SysCall(syscall_nr_ptrace, PTRACE_CONT, pid, addr, TSysParam(data));
end;}

function ptraceCont(pid: TPid; Signal: Integer): TPtraceWord;
begin
  Result := ptrace(PTRACE_CONT, pid, nil, Pointer(Signal));
end;

function ptraceMe: TPtraceWord;
begin
  Result := ptrace(PTRACE_TRACEME, FpGetpid, nil, nil);
end;

function ptraceGetSigInfo(pid: TPid; var siginfo: tsiginfo): TPtraceWord;
begin
  Result := ptrace(PTRACE_GETSIGINFO, pid, nil, @siginfo);
end;

function ptracePeekData(pid: TPid; offset: QWord; var w: TPtraceWord): Boolean;
var
  p : PtrInt;
  err : integer;
begin
  p := offset;
  w := ptrace(PTRACE_PEEKDATA, pid, Pointer(p), nil);
  err := fpgeterrno;
  Result := err = 0;
  if not Result then
    writeln('errno = ', err);
end;

function ptracePeekUser(pid: TPid; offset: QWord; var w: TPtraceWord): Boolean;
var
  p : PtrInt;
  err : Integer;
begin
  p := offset;
  w := ptrace(PTRACE_PEEKUSER, pid, Pointer(p), nil);
  err := fpgeterrno;
  Result := err = 0;
  if not Result then
    writeln('errno = ', err);
end;

function ptracePeekSysUser(pid: TPid; var userdata: Tuser): Boolean;
type
  TByteArray = array [word] of byte;
  PByteArray = ^TByteArray;
var
  p : PByteArray;
  i : PtrInt;
begin
  p := @userdata;
  i := 0;
  Result := true;
  while i < sizeof(Tuser) do begin
    PPtraceWord(@p[i])^:= ptrace(PTRACE_PEEKUSER, pid, Pointer(i), nil);
    inc(i, sizeof(TPtraceWord));
    Result := errno = 0;
    if not Result then Break;
  end;
  writeln;
end;

end.

