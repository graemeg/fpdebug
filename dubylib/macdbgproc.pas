unit macDbgProc; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  machapi,  mach_port;
  
// if err <> 0, writes to stdout comment and error.
// function returns err value
function debugout_kret(err: kern_return_t; const comment: AnsiString): kern_return_t;

// mach-port allocation routines
function MachAllocPortForSelf: mach_port_name_t;
function MachAllocPortForTask(atask: task_t): mach_port_name_t;

//function MachRecvMessage(port: mach_port_name_t; var buf: array of byte; bufSize: Integer): Integer;

// reads tasks memory
function ReadTaskMem(task: task_t; const Offset, Size: Qword; var data: array of byte; var BytesRead: Qword): kern_return_t;

// ???
procedure RecvMessage(port: port_t);

implementation

function MachAllocPortForSelf: mach_port_name_t;
begin
  Result := MachAllocPortForTask( task_t(mach_task_self) );
end;

function MachAllocPortForTask(atask: task_t): mach_port_name_t;
var
  portname: mach_port_name_t;
begin
  if debugout_kret(
    mach_port_allocate( ipc_space_t(atask), MACH_PORT_RIGHT_RECEIVE, portname)
    ,'mach_port_allocate') = 0 then
    Result := portname
  else
    Result := 0;
end;

function ReadTaskMem(task: task_t; const Offset, Size: Qword; var data: array of byte; var BytesRead: Qword): kern_return_t;
begin
  Result := 0;
  BytesRead := 0;
end;

function debugout_kret(err: kern_return_t; const comment: AnsiString): kern_return_t;
begin
  if err <> 0 then writeln(comment, ': ', err, ' ', IntToHex(err, 8));
  Result := err;
end;

procedure RecvMessage(port: port_t);
begin
 
end;

end.

