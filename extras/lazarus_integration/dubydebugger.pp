{      ------------------------------------------------  
       DubyDebugger.pp  -  Duby Debugger 
       ------------------------------------------------ 
 
 ***************************************************************************
 *                                                                         *
 *   This source is free software; you can redistribute it and/or modify   *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This code is distributed in the hope that it will be useful, but      *
 *   WITHOUT ANY WARRANTY; without even the implied warranty of            *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
 *   General Public License for more details.                              *
 *                                                                         *
 *   A copy of the GNU General Public License is available on the World    *
 *   Wide Web at <http://www.gnu.org/copyleft/gpl.html>. You can also      *
 *   obtain it by writing to the Free Software Foundation,                 *
 *   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.        *
 *                                                                         *
 ***************************************************************************
}
unit DubyDebugger;

{$mode objfpc}{$h+}

interface

uses
  Classes, SysUtils, LMessages, FileUtil, Process, Debugger,
  LCLProc, LCLIntf, DebugUtils,
  BaseDebugManager, Dialogs, ProcessList, Forms,
  dbgTypes, dbgMain, dbgAsyncMain, dubyLazInit, ProcessDebugger;

type
  TDubyDebugger = class;

  { TDubySyncForm }

  TDubySyncForm = class(TCustomForm)
  protected
    fDuby : TDubyDebugger;
    procedure WndProc(var msg: TLMessage); override;
  end;

  { TDubyCallback }

  TDubyCallback = class(TDbgASyncCallback)
  private
    fSyncForm   : TDubySyncForm;
  public
    constructor Create(ADuby: TDubyDebugger);
    destructor Destroy; override;
    procedure StateChanged; override;
  end;

  { TDubyDebugger }

  TDubyDebugger = class(TDebugger)
  private
    async       : TDbgAsyncMain;
    fCallback   : TDubyCallback;
    Terminated  : Boolean;
    function  ProcessEnvironment(const AVariable: String; const ASet: Boolean): Boolean;
    function  ProcessRun: Boolean;
    function  ProcessStop: Boolean;
  protected
    function  GetSupportedCommands: TDBGCommands; override;
    function  RequestCommand(const ACommand: TDBGCommand; const AParams: array of const): Boolean; override;

    procedure TerminateMain;
    procedure ASyncStateChanged;
  public
    class function Caption: String; override;
    class function HasExePath: boolean; override;
    constructor Create(const AExternalDebugger: String); override;
    destructor Destroy; override;
  end;

implementation

type
  { TDBGProcess }
  TDBGProcess = class(TProcess)
  private
    FOnDestroy: TNotifyEvent;
  protected
  public
    destructor Destroy; override;
    property OnDestroy: TNotifyEvent read FOnDestroy write FOnDestroy;
  end;

{ TDBGProcess }

destructor TDBGProcess.Destroy;
begin
  if Assigned(FOnDestroy) then FOnDestroy(Self);
  inherited Destroy;
end;


{ TDubyDebugger }

function TDubyDebugger.ProcessEnvironment(const AVariable: String; const ASet: Boolean): Boolean;
begin
  // We don't have to do anything, we'll use the Environment when running
  Result := True;
end;

function TDubyDebugger.ProcessRun: Boolean;
var
  trg : TDbgTarget;
begin
  DebugLn('PR: %s %s', [FileName, Arguments]);

  if async<>nil then begin
    MessageDlg('Duby Debugger', 'There is already a process running!', mtError, [mbOK], 0);
    Result := False;
    Exit;
  end;
  if not Assigned(fCallback) then fCallback:=TDubyCallback.Create(Self);

  trg:=DebugProcessStart( UTF8Decode('"'+FileName+'"'));
  if not Assigned(trg) then begin
    MessageDlg('Duby Debugger', Format('Unable to create process: %s', [FileName]), mtError, [mbOK], 0);
    Result:=False;
    Exit;
  end;
  writeln('launching: ', FileName);
  async:=TDbgAsyncMain.Create(TDbgAsyncCallback(fcallback));
  async.SetMain(TDbgMain.Create(trg, 0, true));
  async.Resume;

  SetState(dsRun);
  Result := True;
end;

function TDubyDebugger.ProcessStop: Boolean;
begin
  if not Assigned(async) then async.Terminate;
  // SetState(dsStop);
  Result := True;
end;

function TDubyDebugger.GetSupportedCommands: TDBGCommands;
begin
  Result := [dcRun, dcStop, dcEnvironment]
end;

function TDubyDebugger.RequestCommand(const ACommand: TDBGCommand; const AParams: array of const): Boolean;
begin
  case ACommand of
    dcRun:         Result := ProcessRun;
    dcStop:        Result := ProcessStop;
    dcEnvironment: Result := ProcessEnvironment(String(APArams[0].VAnsiString), AParams[1].VBoolean);
  end;
end;

procedure TDubyDebugger.TerminateMain;
var
  m   : TDbgMain;
  ev  : TDbgEvent;
begin
  m:=async.Main;
  writeln('terminating MAIN');
  async.SetMain(nil);
  // the next even should be "terminated"
  writeln('Freeing Main!');
  m.WaitNextEvent(ev);
  m.Terminate;
  m.Free;

  writeln('Freeing async!');
  async.WaitFor;
  async.Free;
  async:=nil;
end;

procedure TDubyDebugger.ASyncStateChanged;
begin
  if Terminated then Exit;

  Terminated:=async.State=mstTerminated;
  async.Resume;

  if Terminated then begin
    SetState(dsStop);
    TerminateMain;
    SetState(dsNone);
  end;
end;

class function TDubyDebugger.Caption: String;
begin
  Result := 'duby';
end;

class function TDubyDebugger.HasExePath: boolean;
begin
  Result:= False; // no need to have a valid exe path for the process debugger
end;

constructor TDubyDebugger.Create(const AExternalDebugger:String);
begin
  inherited Create(AExternalDebugger);
end;

destructor TDubyDebugger.Destroy;
begin
  fcallback.Free;
  inherited Destroy;
end;

{ TDubyCallback }

constructor TDubyCallback.Create(ADuby: TDubyDebugger);
begin
  inherited Create;
  fSyncForm:=TDubySyncForm.Create(Application);
  fSyncForm.FDuby:=ADuby;
end;

destructor TDubyCallback.Destroy;
begin
  fSyncForm.Free;
  inherited Destroy;
end;

procedure TDubyCallback.StateChanged;
begin
  writeln('changin state!');
  PostMessage(fSyncForm.Handle, WM_USER, 0, 0);
end;

{ TDubySyncForm }

procedure TDubySyncForm.WndProc(var msg: TLMessage);
begin
  if msg.msg=WM_USER then fDuby.ASyncStateChanged;
  inherited WndProc(msg);
end;

initialization
  RegisterDebugger(TDubyDebugger);

end.
