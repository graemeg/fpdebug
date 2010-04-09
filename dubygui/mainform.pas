unit mainform;

{$mode objfpc}{$H+}

interface

uses
  Classes,SysUtils,FileUtil,LResources,Forms,Controls,Graphics,Dialogs,StdCtrls,
  ComCtrls,SynEdit,dbgTypes,dbgAsyncMain,dbgProject, DebugInfoForm;

type

  { TMainDubyForm }

  TMainDubyForm = class(TForm)
    btnRun:TButton;
    Button1:TButton;
    btnTerminate:TButton;
    editCmdLine:TEdit;
    Label1:TLabel;
    SynEdit1:TSynEdit;
    procedure btnRunClick(Sender:TObject);
    procedure Button1Click(Sender:TObject);
    procedure btnTerminateClick(Sender:TObject);
    procedure FormCreate(Sender:TObject);
  private
    { private declarations }
  public
    { public declarations }
    paused  : Boolean;
    running : Boolean;
    procedure ASyncChangeState(Sender: TObject);
  end;

var
  MainDubyForm: TMainDubyForm;

implementation

{$R *.lfm}

{ TMainDufsdafbyForm }

procedure TMainDubyForm.FormCreate(Sender:TObject);
begin
  ASync.OnStateChanged:=@ASyncChangeState;
end;

procedure TMainDubyForm.btnRunClick(Sender:TObject);
begin
  if (not running) or (ASync.State=mstStopped)   then begin
    if not FileExistsUtf8(editCmdLine.Text) then Exit;
    if not Assigned(ASync.Main) then begin
      running:=StartDebug(editCmdLine.Text);
      debugInfo.ReadDebugInfo;
    end;
    ASync.Resume;
    btnRun.Caption:='Suspend';
  end else begin
    if not paused then begin
      ASync.Main.Process[0].Suspend;
      btnRun.Caption:='Pause';
    end else begin
      ASync.Main.Process[0].Resume;
      btnRun.Caption:='Suspend';
    end;
    paused:=not paused;
  end;
end;

procedure TMainDubyForm.Button1Click(Sender:TObject);
begin
  debugInfo.Show;
end;

procedure TMainDubyForm.btnTerminateClick(Sender:TObject);
begin
  if Assigned(ASync.Main) then
    ASync.Main.Terminate;
end;

procedure TMainDubyForm.ASyncChangeState(Sender: TObject);
begin
  case ASync.State of
    mstStopped: begin
      Caption := 'Stopped';
      btnRun.Caption:='Continue';
      SynEdit1.Lines.Add( EventKindStr[ASync.LastEvent.Kind] + ' '+ASync.LastEvent.Debug);
      if ASync.LastEvent.Kind=dek_SysCall then
        ASync.Resume;
    end;
    mstExecuting: Caption := 'Executing';
  end;
end;

end.

