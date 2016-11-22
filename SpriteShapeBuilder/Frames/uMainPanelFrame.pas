unit uMainPanelFrame;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Objects, uBasePresenterIncapsulator, uIWorkSpaceView, uIItemView, uITableView,
  FMX.Controls.Presentation;

type
  TMainPanelFrame = class(TFrame)
    MainImg: TImage;
    Panel: TPanel;
    procedure MainImgMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure MainImgMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure MainImgMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single);
    procedure MainImgMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; var Handled: Boolean);
  private
    FMouseMoved: TNotifyEvent;
    FMouseUpped: TNotifyEvent;
    FMouseDowned: TNotifyEvent;
    { Private declarations }
  public
    property MouseDowned: TNotifyEvent read FMouseDowned write FMouseDowned;
    property MouseUpped: TNotifyEvent read FMouseUpped write FMouseUpped;
    property MouseMoved: TNotifyEvent read FMouseMoved write FMouseMoved;
  end;

implementation

{$R *.fmx}

procedure TMainPanelFrame.MainImgMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  if Assigned(FMouseDowned) then
    FMouseDowned(Self);
end;

procedure TMainPanelFrame.MainImgMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Single);
begin
  if Assigned(FMouseMoved) then
    FMouseMoved(Self);
end;

procedure TMainPanelFrame.MainImgMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
begin
  if Assigned(FMouseUpped) then
    FMouseUpped(Self);
end;

procedure TMainPanelFrame.MainImgMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; var Handled: Boolean);
begin
  if Panel.Scale.X + ((WheelDelta / 120) * 0.1) > 0.1 then
  begin
    Panel.Scale.X := Panel.Scale.X + ((WheelDelta / 120) * 0.1);
    Panel.Scale.Y := Panel.Scale.X;
    Panel.RecalcSize;
  end;
end;

end.
