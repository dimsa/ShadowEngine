unit SSBMainForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation, FMX.Objects, uSpriteShapeBuilder, System.ImageList,
  FMX.ImgList, FMX.Layouts;

type
  TSSBForm = class(TForm)
    SelectImage: TImage;
    SaveProjectBtn: TCornerButton;
    LoadProjectBtn: TCornerButton;
    MenuPanel: TPanel;
    MainPanel: TPanel;
    OpenDialog: TOpenDialog;
    InfoLbl: TLabel;
    SaveForEngineBtn: TCornerButton;
    SaveDialog: TSaveDialog;
    Instruments: TPanel;
    Shape_img: TImage;
    Object_img: TImage;
    Picture_rect: TRectangle;
    Object_rect: TRectangle;
    Shape_rect: TRectangle;
    InsrumentTabs: TLayout;
    Picture_Inst: TLayout;
    Picture_img: TImage;
    Object_Inst: TLayout;
    Shape_Inst: TLayout;
    AddPictureBtn: TCornerButton;
    DelPictureBtn: TCornerButton;
    AddCircleBtn: TCornerButton;
    AddPolyBtn: TCornerButton;
    DelShapeBtn: TCornerButton;
    AddObjectBtn: TCornerButton;
    DelObjectBtn: TCornerButton;
    EdtObjectBtn: TCornerButton;
    EdtShapeBtn: TCornerButton;
    procedure AddPictureBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure SaveProjectBtnClick(Sender: TObject);
    procedure LoadProjectBtnClick(Sender: TObject);
    procedure SaveForEngineBtnClick(Sender: TObject);
    procedure Shape_edtClick(Sender: TObject);
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

procedure TSSBForm.AddPictureBtnClick(Sender: TObject);
begin
  if OpenDialog.Execute then
    SSB.AddElement(OpenDialog.FileName);
end;

procedure TSSBForm.FormCreate(Sender: TObject);
begin
  Picture_Inst.Position.X := 0;
  Object_Inst.Position.X := 0;
  Shape_Inst.Position.X := 0;

  Picture_Inst.Visible := False;
  Object_Inst.Visible := False;
  Shape_Inst.Visible := False;


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

procedure TSSBForm.Shape_edtClick(Sender: TObject);
begin
  ShowMessage('11');
end;

end.
