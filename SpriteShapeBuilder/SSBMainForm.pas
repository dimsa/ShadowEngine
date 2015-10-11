unit SSBMainForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation, FMX.Objects, uSpriteShapeBuilder;

type
  TSSBForm = class(TForm)
    SelectImage: TImage;
    AddImageBtn: TCornerButton;
    DeleteImageBtn: TCornerButton;
    AddPolyBtn: TCornerButton;
    AddCircleBtn: TCornerButton;
    DelShapeBtn: TCornerButton;
    SaveProjectBtn: TCornerButton;
    LoadProjectBtn: TCornerButton;
    MenuPanel: TPanel;
    MainPanel: TPanel;
    OpenDialog: TOpenDialog;
    Label1: TLabel;
    SaveForEngineBtn: TCornerButton;
    SaveDialog: TSaveDialog;
    procedure AddImageBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure SaveProjectBtnClick(Sender: TObject);
    procedure LoadProjectBtnClick(Sender: TObject);
    procedure SaveForEngineBtnClick(Sender: TObject);
  private
    SSB: TSpriteShapeBuilder;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  SSBForm: TSSBForm;

implementation

{$R *.fmx}

procedure TSSBForm.AddImageBtnClick(Sender: TObject);
begin
  if OpenDialog.Execute then
    SSB.AddElement(OpenDialog.FileName);
end;

procedure TSSBForm.FormCreate(Sender: TObject);
begin
  SSB := TSpriteShapeBuilder.Create;
  SSB.Init(Self);
end;

procedure TSSBForm.LoadProjectBtnClick(Sender: TObject);
begin
  if OpenDialog.Execute then
    SSB.LoadProject(OpenDialog.FileName);
end;

procedure TSSBForm.SaveForEngineBtnClick(Sender: TObject);
begin
  if SaveDialog.Execute then
    SSB.SaveForEngine(SaveDialog.FileName);
end;

procedure TSSBForm.SaveProjectBtnClick(Sender: TObject);
begin
  if SaveDialog.Execute then
    SSB.SaveProject(SaveDialog.FileName);
end;

end.
