unit SSBMainForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation, FMX.Objects, uSpriteShapeBuilder, System.ImageList,
  FMX.ImgList, FMX.Layouts, uSSBTypes, FMX.Effects;

type
  TSSBForm = class(TForm)
    Selected: TImage;
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
    Background: TImage;
    GlowEffect1: TGlowEffect;
    procedure FormCreate(Sender: TObject);
    procedure SaveProjectBtnClick(Sender: TObject);
    procedure LoadProjectBtnClick(Sender: TObject);
    procedure SaveForEngineBtnClick(Sender: TObject);
    procedure DelPictureBtnClick(Sender: TObject);
    procedure BackgroundMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; var Handled: Boolean);
    procedure AddPictureBtnClick(Sender: TObject);
    procedure Picture_imgClick(Sender: TObject);
    procedure Object_imgClick(Sender: TObject);
    procedure Shape_imgClick(Sender: TObject);
    procedure BackgroundMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Single);
    procedure BackgroundResize(Sender: TObject);
    procedure AddObjectBtnClick(Sender: TObject);
    procedure DelObjectBtnClick(Sender: TObject);
    procedure BackgroundMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    procedure BackgroundMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    procedure AddCircleBtnClick(Sender: TObject);
    procedure AddPolyBtnClick(Sender: TObject);
  private
    { Private declarations }
  public

    { Public declarations }
  end;

var
  SSBForm: TSSBForm;
  SSB: TSpriteShapeBuilder;

implementation

{$R *.fmx}

procedure TSSBForm.AddCircleBtnClick(Sender: TObject);
begin
  SSB.Objecter.AddCircle;
end;

procedure TSSBForm.AddObjectBtnClick(Sender: TObject);
begin
  SSB.Objecter.AddObj;
end;

procedure TSSBForm.AddPictureBtnClick(Sender: TObject);
begin
  SSB.Imager.AddImg;
end;

procedure TSSBForm.AddPolyBtnClick(Sender: TObject);
begin
  SSB.Objecter.AddPoly;
end;

procedure TSSBForm.BackgroundMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
begin
  SSB.Objecter.MouseDown;
  SSB.Imager.MouseDown;
end;

procedure TSSBForm.BackgroundMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Single);
begin
  SSBForm.Caption := x.ToString() + ' ' + y.ToString();
  SSB.Imager.MouseMove;
  SSB.Objecter.MouseMove;
end;

procedure TSSBForm.BackgroundMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
begin
  SSB.Objecter.MouseUp;
  SSB.Imager.MouseUp;
end;

procedure TSSBForm.BackgroundMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; var Handled: Boolean);
begin
  if MainPanel.Scale.X + ((WheelDelta / 120) * 0.1) > 0.1 then
  begin
    MainPanel.Scale.X := MainPanel.Scale.X + ((WheelDelta / 120) * 0.1);
    MainPanel.Scale.Y := MainPanel.Scale.X;
  end;
end;

procedure TSSBForm.BackgroundResize(Sender: TObject);
begin
  SSBForm.Caption := Random(100).ToString;
end;

procedure TSSBForm.DelObjectBtnClick(Sender: TObject);
begin
  SSB.Objecter.DelObj;
end;

procedure TSSBForm.DelPictureBtnClick(Sender: TObject);
begin
  SSB.Imager.DelImg;
end;

procedure TSSBForm.FormCreate(Sender: TObject);
begin
  Picture_Inst.Position.X := 0;
  Object_Inst.Position.X := 0;
  Shape_Inst.Position.X := 0;

  Picture_Inst.Visible := False;
  Object_Inst.Visible := False;
  Shape_Inst.Visible := False;

  SSB := TSpriteShapeBuilder.Create(Self, MainPanel, Background, Selected, OpenDialog);
  SSB.Init(Self);
end;

procedure TSSBForm.LoadProjectBtnClick(Sender: TObject);
begin
  if OpenDialog.Execute then
    SSB.LoadProject(OpenDialog.FileName);
end;

procedure TSSBForm.Object_imgClick(Sender: TObject);
begin
  SSB.Status := TSSBStatus.sObject;
end;

procedure TSSBForm.Picture_imgClick(Sender: TObject);
begin
  SSB.Status := TSSBStatus.sPicture;
  SSB.Imager.Init;
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

procedure TSSBForm.Shape_imgClick(Sender: TObject);
begin
  SSB.Status := TSSBStatus.sShape;
end;

end.
