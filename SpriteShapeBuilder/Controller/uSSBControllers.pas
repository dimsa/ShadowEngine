unit uSSBControllers;

interface

uses
  System.Classes, System.UITypes, System.Types,
  FMX.StdCtrls, FMX.Controls, FMX.Graphics, FMX.Objects,
  uSSBModels, uClasses, uEasyDevice;


type
  TSSBImagerController = class
  private
    FModel: TSSBImagerModel;
    FAddButton, FDelButton: TControl;
    FPanel: TPanel;
    FIsMouseDown: Boolean;
    FMouseStartPoint: TPointF;
    FMouseElementPoint: TPointF;
    FElementStartPosition: TPointF;

    // Методы на клик
    procedure DoSelectImage(ASender: TObject);
    procedure DoDelImage(ASender: TObject);

    // Обработчики мыши
    procedure DoZoom(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; var Handled: Boolean);
    procedure DoMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    procedure DoMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    procedure DoMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Single);
  public
    constructor Create(const ASSBImagerModel: TSSBImagerModel);
    procedure AddBitmap(const AFileName: string); overload;
    procedure AddBitmap(const ABitmap: TBitmap); overload;
    procedure InitView(
      AAddButton, ADelButton: TControl;
      AMainPanel: TPanel);
  end;

implementation

{ TSSBImagerController }

procedure TSSBImagerController.AddBitmap(const AFileName: string);
{var
  vImg: TImage;
begin
  vImg := TImage.Create(FPanel);
  vImg.Parent := FPanel;
  vImg.Position.Point := Point(0, 0);

  vImg.Bitmap.LoadFromFile(AFileName);
  AddImage(vImg);   }
begin
end;

procedure TSSBImagerController.AddBitmap(const ABitmap: TBitmap);
var
  vImg: TImage;
begin
  vImg := TImage.Create(FPanel);
  vImg.Width := ABitmap.Width;
  vImg.Height := ABitmap.Height;
  vImg.Parent := FPanel;

  vImg.Assign(ABitmap);

  with vImg do
  begin
    Parent := FPanel;
    OnClick := DoSelectImage;
    OnMouseDown := DoMouseDown;
    OnMouseUp := DoMouseUp;
    OnMouseMove := DoMouseMove;
    OnMouseWheel := DoZoom;
  end;

  //FImages.Add(AImage);
end;

constructor TSSBImagerController.Create(const ASSBImagerModel: TSSBImagerModel);
begin
  FModel := ASSBImagerModel;
end;

procedure TSSBImagerController.DoDelImage(ASender: TObject);
begin
  FModel.DelSelected;
end;

procedure TSSBImagerController.DoMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Single);
var
  vSelected: TImage;
begin
  FIsMouseDown := True;

  FMouseStartPoint := MousePos / FPanel.Scale.X;//Point;
  FMouseElementPoint.X := X;
  FMouseElementPoint.Y := Y;

  if Sender is TImage then
  begin
    if FModel.Select(TImage(Sender)) then
    begin
      vSelected := TImage(Sender);

      FElementStartPosition := vSelected.Position.Point;
      FMouseElementPoint.X := X;
      FMouseElementPoint.Y := Y;
    end;

    {vFigure := SelectedElement.FigureByCoord(FMouseElementPoint, True);
    if vFigure <> nil then
    begin
      if vFigure.KeyPointLocal(FMouseElementPoint, vPoint, CPrec*2, True) then
      begin
        FSelectedElement.AddPointToDraw(vPoint, TAlphaColorRec.Red);
        FElementStartPosition := vPoint ;
        FLockPoint := True;
      end;
    end;
   FSelectedElement.Repaint; }
  end;

end;

procedure TSSBImagerController.DoMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Single);
var
  i: Integer;
  vX, vY: Integer;
  vPoint: TPointF;
begin
  if Sender = FModel.Selected  then
  begin
    if FIsMouseDown then
      with FModel.Selected do
      begin
        Position.Point := FElementStartPosition + MousePos / FPanel.Scale.X - FMouseStartPoint;
        FModel.Adjust(FModel.Selected);
      end
   end;

  FMouseElementPoint.X := X;
  FMouseElementPoint.Y := Y;
end;

procedure TSSBImagerController.DoMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
begin
  FIsMouseDown := False;
end;

procedure TSSBImagerController.DoSelectImage(ASender: TObject);
begin
  if ASender is TImage then
    FModel.Select(ASender as TImage)
end;

procedure TSSBImagerController.DoZoom(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; var Handled: Boolean);
begin
  if FPanel.Scale.X + ((WheelDelta / 120) * 0.1) > 0.1 then
  begin
    FPanel.Scale.X := FPanel.Scale.X + ((WheelDelta / 120) * 0.1);
    FPanel.Scale.Y := FPanel.Scale.X;
  end;
end;

procedure TSSBImagerController.InitView(AAddButton, ADelButton: TControl;
  AMainPanel: TPanel);
begin
  FAddButton := AAddButton;
  FDelButton := ADelButton;
  FPanel := AMainPanel;

  FDelButton.OnClick := DoDelImage;
  FPanel.OnMouseDown := DoMouseDown;
  FPanel.OnMouseUp := DoMouseUp;
  FPanel.OnMouseMove := DoMouseMove;
  FPanel.OnMouseWheel := DoZoom;
end;

end.
