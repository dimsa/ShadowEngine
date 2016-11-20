unit SSBMainForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation, FMX.Objects, System.ImageList,
  FMX.ImgList, FMX.Layouts, uSSBTypes, FMX.Effects,
  uMainPresenter, uIMainView, uWorkSpaceView, uPictureFrames, uObjectFrame,
  uShapeFrame;

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
    Picture_img: TImage;
    Background: TImage;
    GlowEffect1: TGlowEffect;
    PictureFrame: TPictureFrame;
    ObjectFrame: TObjectFrame;
    ShapeFrame: TShapeFrame;
    LineOneLayout: TLayout;
    LineTwoLayout: TLayout;
    Rendition_rect: TRectangle;
    Rendition_img: TImage;
    Sounder_rect: TRectangle;
    Sounder_img: TImage;
    procedure FormCreate(Sender: TObject);
    procedure SaveProjectBtnClick(Sender: TObject);
    procedure LoadProjectBtnClick(Sender: TObject);
    procedure SaveForEngineBtnClick(Sender: TObject);

    procedure BackgroundMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; var Handled: Boolean);
    procedure BackgroundMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Single);
    procedure BackgroundResize(Sender: TObject);

    procedure BackgroundMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    procedure BackgroundMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    procedure Picture_imgMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    procedure Shape_imgMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    function FormTopLeft: TPointF;

    procedure BackgroundPaint(Sender: TObject; Canvas: TCanvas; const ARect: TRectF);
  private
    FFrames: array[TSSBStatus] of TFrame;
    FStatus: TSSBStatus;
    FMainPresenter: TMainPresenter;
    FWorkSpaceView: TWorkSpaceView;
    function LoadDialog(out AFileName: string): boolean;
    function GetStatus: TSSBStatus;
    procedure SetStatus(const AStatus: TSSBStatus);
    function ClientToScreenPoint(const APoint: TPoint): TPoint;
    function FilenameFromDlg(out AFileName: string): boolean;
    procedure InitFrames;
  public

  end;

var
  SSBForm: TSSBForm;

implementation

{$R *.fmx}

procedure TSSBForm.BackgroundMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
begin
  FWorkSpaceView.Objecter.MouseDown;
  FWorkSpaceView.Imager.MouseDown;
end;

procedure TSSBForm.BackgroundMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Single);
begin
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
    MainPanel.RecalcSize;
  end;
end;

procedure TSSBForm.BackgroundPaint(Sender: TObject; Canvas: TCanvas; const ARect: TRectF);
begin
  Canvas.BeginScene();
  Canvas.Fill.Color := TAlphaColorRec.White;
  Canvas.FillRect(ARect, 0, 0, [], 1, FMX.Types.TCornerType.ctBevel);
  Canvas.EndScene();
end;

procedure TSSBForm.BackgroundResize(Sender: TObject);
begin
  SSBForm.Caption := Random(100).ToString;
end;

function TSSBForm.ClientToScreenPoint(const APoint: TPoint): TPoint;
begin
  Result := Self.ClientToScreen(APoint).Round;
end;

function TSSBForm.FilenameFromDlg(out AFileName: string): boolean;
begin
  AFileName := '';
  Result := OpenDialog.Execute;

  if Result then
    AFileName := OpenDialog.FileName;
end;

procedure TSSBForm.InitFrames;
var
  i: TSSBStatus;
begin
  PictureFrame.Init(FMainPresenter.Imager);
  ObjectFrame.Init(FMainPresenter.Objecter);
  ShapeFrame.Init(FMainPresenter.Objecter);
  FFrames[TSSBStatus.sPicture] := PictureFrame;
  FFrames[TSSBStatus.sObject] := ObjectFrame;
  FFrames[TSSBStatus.sShape] := ShapeFrame;

  for i := Low(TSSBStatus) to High(TSSBStatus) do
    FFrames[i].Visible := False;
end;

procedure TSSBForm.FormCreate(Sender: TObject);
begin
  // MVP
  FWorkSpaceView := TWorkSpaceView.Create(MainPanel, Background, Selected, OpenDialog, FormTopLeft);

  FMainPresenter := TMainPresenter.Create(Self, FWorkSpaceView);
  InitFrames;

  FWorkSpaceView.Imager := FMainPresenter.Imager;
  FWorkSpaceView.Objecter := FMainPresenter.Objecter;

  FMainPresenter.InitImager;
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
  FFrames[FStatus].Visible := False;
  FStatus := AStatus;
  FFrames[FStatus].Visible := True;
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
