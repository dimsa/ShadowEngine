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
    procedure AddImageBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
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
  begin
    SSB.AddElement(OpenDialog.FileName);
  end;

end;

procedure TSSBForm.FormCreate(Sender: TObject);
begin
  SSB := TSpriteShapeBuilder.Create;
  SSB.Init(MainPanel);
end;

end.
