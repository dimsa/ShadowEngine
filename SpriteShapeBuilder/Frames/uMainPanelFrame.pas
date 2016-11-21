unit uMainPanelFrame;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Objects, uBasePresenterIncapsulator;

type
  TMainPanelFrame = class(TFrame)
    MainImg: TImage;
    procedure MainImgMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    procedure MainImgMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    procedure MainImgMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Single);
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

end.
