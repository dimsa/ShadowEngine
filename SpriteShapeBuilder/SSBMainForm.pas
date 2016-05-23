unit SSBMainForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation, FMX.Objects, System.ImageList,
  FMX.ImgList, FMX.Layouts, uSSBTypes, FMX.Effects,
  uMainPresenter, uIMainView, uWorkSpaceView;

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
    procedure Picture_imgMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    procedure Object_imgMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    procedure Shape_imgMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    function FormTopLeft: TPointF;
  private
    FPanels: array[TSSBStatus] of TLayout;
    FStatus: TSSBStatus;
    FMainPresenter: TMainPresenter;
    FWorkSpaceView: TWorkSpaceView;
    function LoadDialog(out AFileName: string): boolean;
    function GetStatus: TSSBStatus;
    procedure SetStatus(const AStatus: TSSBStatus);
    function ClientToScreenPoint(const APoint: TPoint): TPoint;
    function FilenameFromDlg(out AFileName: string): boolean;
  public

  end;

var
  SSBForm: TSSBForm;

implementation

{$R *.fmx}

procedure TSSBForm.AddCircleBtnClick(Sender: TObject);
begin
  FWorkSpaceView.Objecter.AddCircle;
end;

procedure TSSBForm.AddObjectBtnClick(Sender: TObject);
begin
  FWorkSpaceView.Objecter.AddObj;
end;

procedure TSSBForm.AddPictureBtnClick(Sender: TObject);
begin
  FWorkSpaceView.Imager.AddImg;
end;

procedure TSSBForm.AddPointBtnClick(Sender: TObject);
begin
  FWorkSpaceView.Objecter.AddPoint;
end;

procedure TSSBForm.AddPolyBtnClick(Sender: TObject);
begin
  FWorkSpaceView.Objecter.AddPoly;
end;

procedure TSSBForm.BackgroundMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
begin
  FWorkSpaceView.Objecter.MouseDown;
  FWorkSpaceView.Imager.MouseDown;
end;

procedure TSSBForm.BackgroundMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Single);
begin
//  SSBForm.Caption := x.ToString() + ' ' + y.ToString();
  FWorkSpaceView.Imager.MouseMove;
  FWorkSpaceView.Objecter.MouseMove;
end;

procedure TSSBForm.BackgroundMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
begin
  FWorkSpaceView.Objecter.MouseUp;
  FWorkSpaceView.Imager.MouseUp;
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
  FWorkSpaceView.Objecter.DelObj;
end;

procedure TSSBForm.DelPictureBtnClick(Sender: TObject);
begin
  FWorkSpaceView.Imager.DelImg;
end;

procedure TSSBForm.DelPointBtnClick(Sender: TObject);
begin
  FWorkSpaceView.Objecter.DelPoint;
end;

procedure TSSBForm.DelShapeBtnClick(Sender: TObject);
begin
  FWorkSpaceView.Objecter.DelShape;
end;

function TSSBForm.FilenameFromDlg(out AFileName: string): boolean;
begin
  AFileName := '';
  Result := OpenDialog.Execute;

  if Result then
    AFileName := OpenDialog.FileName;
end;

procedure TSSBForm.FormCreate(Sender: TObject);
begin
  // Initiilizing of controls
  Picture_Inst.Position.X := 0;
  Object_Inst.Position.X := 0;
  Shape_Inst.Position.X := 0;

  FPanels[TSSBStatus.sPicture] := Picture_Inst;
  FPanels[TSSBStatus.sObject] := Object_Inst;
  FPanels[TSSBStatus.sShape] := Shape_Inst;

  Picture_Inst.Visible := False;
  Object_Inst.Visible := False;
  Shape_Inst.Visible := False;

  // MVP
  FWorkSpaceView := TWorkSpaceView.Create(MainPanel, Background, Selected, OpenDialog, FormTopLeft);
  FMainPresenter := TMainPresenter.Create(Self, FWorkSpaceView);

  FWorkSpaceView.Imager := FMainPresenter.Imager;
  FWorkSpaceView.Objecter := FMainPresenter.Objecter;
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
  FMainPresenter.LoadProject;
end;

procedure TSSBForm.Object_imgMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
begin
  FMainPresenter.InitObjecter;
end;

procedure TSSBForm.Picture_imgMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
begin
  FMainPresenter.InitImager;
end;

procedure TSSBForm.SaveForEngineBtnClick(Sender: TObject);
begin
  FMainPresenter.SaveForEngine;
end;

procedure TSSBForm.SaveProjectBtnClick(Sender: TObject);
begin
    FMainPresenter.SaveProject;
end;

procedure TSSBForm.SetStatus(const AStatus: TSSBStatus);
begin
  FPanels[FStatus].Visible := False;
  FStatus := AStatus;
  FPanels[FStatus].Visible := True;
end;

procedure TSSBForm.Shape_imgMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
begin
  FMainPresenter.InitShaper;
end;

function TSSBForm.FormTopLeft: TPointF;
begin
  Result := ClientToScreenPoint(TPoint.Zero);
end;

end.
