unit SSBMainForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation, FMX.Objects, System.ImageList,
  FMX.ImgList, FMX.Layouts, uSSBTypes, FMX.Effects,
  uMainPresenter, uIMainView, uWorkSpaceView, uPictureFrames, uObjectFrame,
  uShapeFrame, uStatusSelectorFrame, uMainPanelFrame;

type

  TSSBForm = class(TForm, IMainView)
    Selected: TImage;
    SaveProjectBtn: TCornerButton;
    LoadProjectBtn: TCornerButton;
    MenuPanel: TPanel;
    OpenDialog: TOpenDialog;
    InfoLbl: TLabel;
    SaveForEngineBtn: TCornerButton;
    SaveDialog: TSaveDialog;
    Instruments: TPanel;
    GlowEffect1: TGlowEffect;
    PictureFrame: TPictureFrame;
    ObjectFrame: TObjectFrame;
    ShapeFrame: TShapeFrame;
    StatusSelectorFrame: TStatusSelectorFrame;
    MainPanelFrame: TMainPanelFrame;
    procedure FormCreate(Sender: TObject);
    procedure SaveProjectBtnClick(Sender: TObject);
    procedure LoadProjectBtnClick(Sender: TObject);
    procedure SaveForEngineBtnClick(Sender: TObject);
  private
    FFrames: array[TSSBStatus] of TFrame;
    FStatus: TSSBStatus;
    FMainPresenter: TMainPresenter;
    FWorkSpaceView: TWorkSpaceView;
    function FormTopLeft: TPointF;
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
  StatusSelectorFrame.Init(FMainPresenter);
  FFrames[TSSBStatus.sPicture] := PictureFrame;
  FFrames[TSSBStatus.sObject] := ObjectFrame;
  FFrames[TSSBStatus.sShape] := ShapeFrame;

  for i := Low(TSSBStatus) to High(TSSBStatus) do
    FFrames[i].Visible := False;
end;

procedure TSSBForm.FormCreate(Sender: TObject);
begin
  // MVP
  FWorkSpaceView := TWorkSpaceView.Create(MainPanelFrame,  Selected, OpenDialog, FormTopLeft);

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

function TSSBForm.FormTopLeft: TPointF;
begin
  Result := ClientToScreenPoint(TPoint.Zero);
end;

end.
