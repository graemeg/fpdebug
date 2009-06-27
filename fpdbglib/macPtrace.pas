unit macPtrace;

{$mode objfpc}{$H+}

interface


const
 	PT_TRACE_ME	= 0;	// child declares it's being traced
 	PT_READ_I	  = 1;	// read word in child's I space
 	PT_READ_D	  = 2;	// read word in child's D space
  PT_READ_U	  = 3;	// read word in child's user structure
 	PT_WRITE_I	= 4;	// write word in child's I space
  PT_WRITE_D	= 5;	// write word in child's D space
	PT_WRITE_U	= 6;	// write word in child's user structure
	PT_CONTINUE	= 7;	// continue the child
	PT_KILL		  = 8;	// kill the child process
	PT_STEP		  = 9;	// single step the child
 	PT_ATTACH	  = 10;	// trace some running process
	PT_DETACH	  = 11;	// stop tracing a process
	PT_SIGEXC	  = 12;	// signals as exceptions for current_proc
  PT_THUPDATE	   = 13;	// signal for thread#
  PT_ATTACHEXC   = 14;	// attach to running process with signal exception

 	PT_FORCEQUOTA  = 30;	// Enforce quota for root
 	PT_DENY_ATTACH = 31;

	PT_FIRSTMACH	= 32;	// for machine-specific requests

const
  CONT_STOP_ADDR : PtrUInt = 1;

type
  pid_t = Integer;

//todo: fix fo syscall
function ptrace(_request: integer; _pid: pid_t; _addr: PtrUInt; _data: integer): integer; cdecl; external name 'ptrace';

function ptraceme: Boolean;
function ptraceme_cont(pid: pid_t; addr: PtrUInt; signal: Integer): Boolean;

implementation

function ptraceme: Boolean;
begin
  Result := ptrace(PT_TRACE_ME, 0, 0, 0) = 0;
end;

function ptraceme_cont(pid: pid_t; addr: PtrUInt; signal: Integer): Boolean;
begin
  Result := ptrace(PT_CONTINUE, pid, addr, signal) = 0;
end;



end.

