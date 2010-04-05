unit mainform;

{$mode objfpc}{$H+}

interface

uses
  Classes,SysUtils,FileUtil,LResources,Forms,Controls,Graphics,Dialogs,StdCtrls,
  dbgAsyncMain, dbgProject;

type

  { TForm1 }

  TForm1 = class(TForm)
    btnRun:TButton;
    editCmdLine:TEdit;
    Label1:TLabel;
    procedure btnRunClick(Sender:TObject);
    procedure FormCreate(Sender:TObject);
  private
    { private declarations }
  public
    { public declarations }
    procedure ASyncChangeState(Sender: TObject);
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender:TObject);
begin
  ASync.OnStateChanged:=@ASyncChangeState;
end;

procedure TForm1.btnRunClick(Sender:TObject);
begin
  if not Assigned(Main) then StartDebug(editCmdLine.Text);
  ASync.Resume;
end;

procedure TForm1.ASyncChangeState(Sender: TObject);
begin
  case ASync.State of
    mstStopped:  Caption := 'Stopped';
    mstExecuting: Caption := 'Executing';
  end;
end;

end.

