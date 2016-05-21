unit SSBMainForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation, FMX.Objects, System.ImageList,
  FMX.ImgList, FMX.Layouts, uSSBTypes, FMX.Effects,
  uMainPresenter, uIMainView;

type

  TSSBForm = class(TForm, IMainView)
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
    DelPointBtn: TCornerButton;
    AddPointBtn: TCornerButton;
    procedure FormCreate(Sender: TObject);
    procedure SaveProjectBtnClick(Sender: TObject);
    procedure LoadProjectBtnClick(Sender: TObject);
    procedure SaveForEngineBtnClick(Sender: TObject);
    procedure DelPictureBtnClick(Sender: TObject);
    procedure BackgroundMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; var Handled: Boolean);
    procedure AddPictureBtnClick(Sender: TObject);
    {procedure Picture_imgClick(Sender: TObject);
    procedure Object_imgClick(Sender: TObject);
    procedure Shape_imgClick(Sender: TObject); }
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
    procedure DelShapeBtnClick(Sender: TObject);
    procedure AddPointBtnClick(Sender: TObject);
    procedure DelPointBtnClick(Sender: TObject);

  private
    FPanels: array[TSSBStatus] of TLayout;
    FStatus: TSSBStatus;
    FMainPresenter: TMainPresenter;
    function LoadDialog(out AFileName: string): boolean;
    function GetStatus: TSSBStatus;
    procedure SetStatus(const AStatus: TSSBStatus);
    function ClientToScreenPoint(const APoint: TPoint): TPoint;
  public

    { Public declarations }
  end;

var
  SSBForm: TSSBForm;

implementation

{$R *.fmx}

procedure TSSBForm.AddCircleBtnClick(Sender: TObject);
begin
  FMainPresenter.Objecter.AddCircle;
end;

procedure TSSBForm.AddObjectBtnClick(Sender: TObject);
begin
  FMainPresenter.Objecter.AddObj;
end;

procedure TSSBForm.AddPictureBtnClick(Sender: TObject);
begin
  FMainPresenter.Imager.AddImg;
end;

procedure TSSBForm.AddPointBtnClick(Sender: TObject);
begin
  FMainPresenter.Objecter.AddPoint;
end;

procedure TSSBForm.AddPolyBtnClick(Sender: TObject);
begin
  FMainPresenter.Objecter.AddPoly;
end;

procedure TSSBForm.BackgroundMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
begin
  FMainPresenter.Objecter.MouseDown;
  FMainPresenter.Imager.MouseDown;
end;

procedure TSSBForm.BackgroundMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Single);
begin
  SSBForm.Caption := x.ToString() + ' ' + y.ToString();
  FMainPresenter.Imager.MouseMove;
  FMainPresenter.Objecter.MouseMove;
end;

procedure TSSBForm.BackgroundMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
begin
  FMainPresenter.Objecter.MouseUp;
  FMainPresenter.Imager.MouseUp;
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

function TSSBForm.ClientToScreenPoint(const APoint: TPoint): TPoint;
begin
  Result := Self.ClientToScreen(APoint).Round;
end;

procedure TSSBForm.DelObjectBtnClick(Sender: TObject);
begin
  FMainPresenter.Objecter.DelObj;
end;

procedure TSSBForm.DelPictureBtnClick(Sender: TObject);
begin
  FMainPresenter.Imager.DelImg;
end;

procedure TSSBForm.DelPointBtnClick(Sender: TObject);
begin
  FMainPresenter.Objecter.DelPoint;
end;

procedure TSSBForm.DelShapeBtnClick(Sender: TObject);
begin
  FMainPresenter.Objecter.DelShape;
end;

procedure TSSBForm.FormCreate(Sender: TObject);
begin
  Picture_Inst.Position.X := 0;
  Object_Inst.Position.X := 0;
  Shape_Inst.Position.X := 0;

  FPanels[TSSBStatus.sPicture] := Picture_Inst;
  FPanels[TSSBStatus.sObject] := Object_Inst;
  FPanels[TSSBStatus.sShape] := Shape_Inst;

  Picture_Inst.Visible := False;
  Object_Inst.Visible := False;
  Shape_Inst.Visible := False;

  FMainPresenter := TMainPresenter.Create(Self);//, MainPanel, Background, Selected, OpenDialog);



//  FMainPresenter.Init(Self);
end;

function TSSBForm.GetStatus: TSSBStatus;
begin
  Result := FStatus;
end;

function TSSBForm.LoadDialog(out AFileName: string): boolean;
begin
  Result := OpenDialog.Execute;

  AFileName := '';
  if Result then
    AFileName := OpenDialog.FileName;
end;

procedure TSSBForm.LoadProjectBtnClick(Sender: TObject);
begin
  if OpenDialog.Execute then
    FMainPresenter.LoadProject(OpenDialog.FileName);
end;

{procedure TSSBForm.Object_imgClick(Sender: TObject);
begin
  FMainPresenter.Status := TSSBStatus.sObject;
end;

procedure TSSBForm.Picture_imgClick(Sender: TObject);
begin
  FMainPresenter.Status := TSSBStatus.sPicture;
end;   }

procedure TSSBForm.SaveForEngineBtnClick(Sender: TObject);
begin
  if SaveDialog.Execute then
    FMainPresenter.SaveForEngine(SaveDialog.FileName);
end;

procedure TSSBForm.SaveProjectBtnClick(Sender: TObject);
begin
  if SaveDialog.Execute then
    FMainPresenter.SaveProject(SaveDialog.FileName);
end;

procedure TSSBForm.SetStatus(const AStatus: TSSBStatus);
begin
  FStatus := AStatus;

  FPanels[FStatus].Visible := False;
  FPanels[FStatus].Visible := True;
end;

{procedure TSSBForm.Shape_imgClick(Sender: TObject);
begin
  FMainPresenter.Status := TSSBStatus.sShape;
end;  }

end.
