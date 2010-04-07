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
    Button2:TButton;
    editCmdLine:TEdit;
    Label1:TLabel;
    SynEdit1:TSynEdit;
    procedure btnRunClick(Sender:TObject);
    procedure Button1Click(Sender:TObject);
    procedure Button2Click(Sender:TObject);
    procedure FormCreate(Sender:TObject);
  private
    { private declarations }
  public
    { public declarations }
    procedure ASyncChangeState(Sender: TObject);
  end;

var
  MainDubyForm: TMainDubyForm;

implementation

{$R *.lfm}

{ TMainDubyForm }

procedure TMainDubyForm.FormCreate(Sender:TObject);
begin
  ASync.OnStateChanged:=@ASyncChangeState;
end;

procedure TMainDubyForm.btnRunClick(Sender:TObject);
begin
  if not FileExistsUtf8(editCmdLine.Text) then Exit;
  if not Assigned(ASync.Main) then begin
    StartDebug(editCmdLine.Text);
    debugInfo.ReadDebugInfo;
  end;
  ASync.Resume;
end;

procedure TMainDubyForm.Button1Click(Sender:TObject);
begin
  debugInfo.Show;
end;

procedure TMainDubyForm.Button2Click(Sender:TObject);
begin
  if Assigned(ASync.Main) then
    ASync.Main.Terminate;
end;

procedure TMainDubyForm.ASyncChangeState(Sender: TObject);
begin
  case ASync.State of
    mstStopped: begin
      Caption := 'Stopped';
      SynEdit1.Lines.Add( EventKindStr[ASync.LastEvent.Kind] + ' '+ASync.LastEvent.Debug);
      if ASync.LastEvent.Kind=dek_SysCall then
        ASync.Resume;
    end;
    mstExecuting: Caption := 'Executing';
  end;
end;

end.

