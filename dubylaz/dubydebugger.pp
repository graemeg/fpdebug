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
  Classes, SysUtils, FileUtil, Process, Debugger, LCLProc, DebugUtils,
  BaseDebugManager, Dialogs, ProcessList,
  dbgTypes, dbgMain, dbgAsyncMain, dubyLazInit;

type
  { TDubyDebugger }

  TDubyDebugger = class(TDebugger)
  private
    async     : TDbgAsyncMain;
    fcallback : TObject;
    function  ProcessEnvironment(const AVariable: String; const ASet: Boolean): Boolean;
    function  ProcessRun: Boolean;
    function  ProcessStop: Boolean;
  protected
    function  GetSupportedCommands: TDBGCommands; override;
    function  RequestCommand(const ACommand: TDBGCommand; const AParams: array of const): Boolean; override;

    procedure ASyncStateChanged;
  public
    class function Caption: String; override;
    class function HasExePath: boolean; override;
    destructor Destroy; override;
  published
  end;

  { TDubyCallback }

  TDubyCallback = class(TDbgASyncCallback)
  private
    fDebugger: TDubyDebugger;
  public
    constructor Create(ADebugger: TDubyDebugger);
    procedure StateChanged; override;
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
  if not Assigned(fcallback) then fcallback:=TDubyCallback.Create(Self);

  trg:=DebugProcessStart( UTF8Decode('"'+FileName+'"'));
  if not Assigned(trg) then begin
    MessageDlg('Duby Debugger', Format('Unable to create process: %s', [FileName]), mtError, [mbOK], 0);
    Result:=False;
    Exit;
  end;
  async:=TDbgAsyncMain.Create(TDbgAsyncCallback(fcallback));
  async.Main:=TDbgMain.Create(trg, 0);

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

procedure TDubyDebugger.ASyncStateChanged;
begin

end;

class function TDubyDebugger.Caption: String;
begin
  Result := 'duby';
end;

class function TDubyDebugger.HasExePath: boolean;
begin
  Result:= False; // no need to have a valid exe path for the process debugger
end;

destructor TDubyDebugger.Destroy;
begin
  fcallback.Free;
  inherited Destroy;
end;

{ TDubyCallback }

constructor TDubyCallback.Create(ADebugger:TDubyDebugger);
begin
  inherited Create;
  fDebugger:=ADebugger;
end;

procedure TDubyCallback.StateChanged;
begin
  fDebugger.AsyncStateChanged;
end;

initialization
  RegisterDebugger(TDubyDebugger);

end.
