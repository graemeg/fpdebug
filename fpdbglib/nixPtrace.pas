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

interface

uses
  BaseUnix{, syscall};

{$linklib libc}

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

function ptrace(request: Tptrace_request; pid: TPid; addr, data: Pointer): TPtraceWord;
 cdecl; external name 'ptrace';

function _ptrace_traceme: TPtraceWord;
function _ptrace_cont(pid: TPid; Signal: Integer): TPtraceWord;
function _ptrace_getsiginfo(pid: TPid; var siginfo: tsiginfo): TPtraceWord;

implementation

{function ptrace(request: Tptrace_request; pid: TPid; addr, data: Pointer): TPtraceWord;
begin
  Result := Do_SysCall(syscall_nr_ptrace, PTRACE_CONT, pid, addr, TSysParam(data));
end;}

function _ptrace_cont(pid: TPid; Signal: Integer): TPtraceWord;
begin
  Result := ptrace(PTRACE_CONT, pid, nil, Pointer(Signal));
end;

function _ptrace_traceme: TPtraceWord;
begin
  Result := ptrace(PTRACE_TRACEME, FpGetpid, nil, nil);
end;

function _ptrace_getsiginfo(pid: TPid; var siginfo: tsiginfo): TPtraceWord;
begin
  Result := ptrace(PTRACE_GETSIGINFO, pid, nil, @siginfo);
end;

end.

