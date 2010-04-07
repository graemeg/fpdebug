unit DebugInfoForm;

{$mode objfpc}{$H+}

interface

uses
  Classes,SysUtils,FileUtil,LResources,Forms,Controls,Graphics,Dialogs,
  ComCtrls,ExtCtrls,SynEdit,dbgProject;

type

  { TdebugInfo }

  TdebugInfo = class(TForm)
    ListView1:TListView;
    Notebook1:TNotebook;
    Page1:TPage;
    Splitter1:TSplitter;
    SynEdit1:TSynEdit;
    procedure ListView1DblClick(Sender:TObject);
    procedure Notebook1ChangeBounds(Sender:TObject);
  private
    { private declarations }
  public
    { public declarations }
    procedure ReadDebugInfo;
    procedure LoadContent(const FileName: AnsiString);
    procedure OpenFileName;
  end;

var
  debugInfo: TdebugInfo;

implementation

procedure TdebugInfo.ListView1DblClick(Sender:TObject);
var
  i   : Integer;
  fn  : AnsiString;
begin
  i:=ListView1.ItemIndex;
  if i<0 then Exit;
  fn:=IncludeTrailingPathDelimiter(ListView1.Items[i].SubItems[0])+ ListView1.Items[i].Caption;
  LoadContent(UTF8Decode(fn));
end;

procedure TdebugInfo.Notebook1ChangeBounds(Sender:TObject);
begin

end;

{$R *.lfm}

{ TdebugInfo }

procedure TdebugInfo.ReadDebugInfo;
var
  i   : Integer;
  st  : TStringList;
  l   : TListItem;
begin
  ListView1.Clear;
  st:=TStringList.Create;
  try
    dbgInfo.EnumSourceFiles(st);
    for i:=0 to st.Count-1 do begin
      l:=ListView1.Items.Add;
      l.Caption:=ExtractFileName(st[i]);
      l.SubItems.Add(ExtractFileDir(st[i]));
    end;
  finally
    st.Free;
  end;
end;

procedure TdebugInfo.LoadContent(const FileName:AnsiString);
begin
  Page1.Caption:=ExtractFileName(FileName);
  SynEdit1.Lines.LoadFromFile(FileName);
end;

procedure TdebugInfo.OpenFileName;
begin

end;

end.

