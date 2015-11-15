unit uSSBControllers;

interface

uses
  System.Classes, System.UITypes, System.Types, System.SysUtils,
  FMX.StdCtrls, FMX.Controls, FMX.Graphics, FMX.Objects,
  uSSBModels, uClasses, uEasyDevice;


type

  TSSBController = class abstract
  public
    // Обработчики мыши
    procedure DoZoom(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; var Handled: Boolean); virtual; abstract;
    procedure DoMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single); virtual; abstract;
    procedure DoMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single); virtual; abstract;
    procedure DoMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Single); virtual; abstract;
    procedure DoCommand(const ACommandName: string; AParams: array of Const); overload; virtual; abstract;
    procedure DoCommand(const ACommandName: string); overload;
  end;

  TSSBImagerController = class(TSSBController)
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
      WheelDelta: Integer; var Handled: Boolean); override;
    procedure DoMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single); override;
    procedure DoMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single); override;
    procedure DoMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Single); override;
    procedure Adjust(AControl: TControl);
  public
    constructor Create(const ASSBImagerModel: TSSBImagerModel);
    procedure DoCommand(const ACommandName: string; AParams: array of Const); override;
    procedure AddImage(const AFileName: string); overload;
    procedure AddImage(const AImg: TImage); overload;
    procedure InitView(
      AAddButton, ADelButton: TControl;
      AMainPanel: TPanel);
  end;

implementation

{ TSSBImagerController }

procedure TSSBImagerController.AddImage(const AFileName: string);
var
  vImg: TImage;
begin
  vImg := TImage.Create(FPanel);
  vImg.Parent := FPanel;
  vImg.Position.Point := Point(0, 0);

  vImg.Bitmap.LoadFromFile(AFileName);
  vImg.Width := vImg.Bitmap.Width;
  vImg.Height:= vImg.Bitmap.Height;
  AddImage(vImg);
end;

procedure TSSBImagerController.AddImage(const AImg: TImage);
var
  vImg: TImage;
begin
  vImg := AImg;
  with vImg do
  begin
    Parent := FPanel;
    OnClick := DoSelectImage;
    OnMouseDown := DoMouseDown;
    OnMouseUp := DoMouseUp;
    OnMouseMove := DoMouseMove;
    OnMouseWheel := DoZoom;
  end;

  FModel.Add(vImg);
  //FImages.Add(AImage);
end;

procedure TSSBImagerController.Adjust(AControl: TControl);
var
  i, vX, vY: Integer;
begin
  for i := 0 to FModel.ImageCount - 1 do
    with FModel do
    if Images[i] <> AControl then
    begin
      for vX := 0 to 3 do
        for vY := 0 to 3 do
          with AControl do
          begin
            if (Points[vX].X <= Images[i].Points[vY].X + CPrec) and
              (Points[vX].X >= Images[i].Points[vY].X - CPrec) then
              Points[vX] := PointF(Images[i].Points[vY].X, Points[vX].Y);

            if (Points[vX].Y <= Images[i].Points[vY].Y + CPrec) and
              (Points[vX].Y >= Images[i].Points[vY].Y - CPrec) then
              Points[vX] := PointF(Points[vX].X, Images[i].Points[vY].Y);
          end;
    end;
end;

constructor TSSBImagerController.Create(const ASSBImagerModel: TSSBImagerModel);
begin
  FModel := ASSBImagerModel;
end;

procedure TSSBImagerController.DoCommand(const ACommandName: string;
  AParams: array of Const);
var
  vName: string;
  vS: ansistring;
begin
  vName := LowerCase(ACommandName);

  if vName = 'add' then
  begin
    vS := string(AParams[0].VString);
    AddImage(vS);
  end;

 if vName = 'remove' then
    DoDelImage(nil);
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
        Adjust(FModel.Selected);
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


{
procedure TSSBView.DoMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
var
  vFigure: TSSBFigure;
  vPoint: TPointF;
begin
  FIsMouseDown := True;
  FMouseStartPoint := MousePos / FPanel.Scale.X;//Point;
  FMouseElementPoint.X := X;
  FMouseElementPoint.Y := Y;

//  if FStatus = sPicture then
 // begin

  if Sender is TSSBElement then
  begin
    SelectedElement := TSSBElement(Sender);

    FElementStartPosition := FSelectedElement.Position.Point;

    FMouseElementPoint.X := X;
    FMouseElementPoint.Y := Y;
    vFigure := SelectedElement.FigureByCoord(FMouseElementPoint, True);
    if vFigure <> nil then
    begin
      if vFigure.KeyPointLocal(FMouseElementPoint, vPoint, CPrec*2, True) then
      begin
        FSelectedElement.AddPointToDraw(vPoint, TAlphaColorRec.Red);
        FElementStartPosition := vPoint ;
        FLockPoint := True;
      end;
    end;
   FSelectedElement.Repaint;
  end;

end;

procedure TSSBView.DoMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Single);
var
  i: Integer;
  vX, vY: Integer;
  vFigure: TSSBFigure;
  vPoint: TPointF;
begin
  if Sender = FSelectedElement then
  begin
    if FIsMouseDown then
      with FSelectedElement do
      begin
        if FLockPoint = False then
        begin
          Position.Point := FElementStartPosition + MousePos / FPanel.Scale.X - FMouseStartPoint;

          for i := 0 to FElements.Count - 1 do
            if FElements[i] <> FSelectedElement then
            begin
              for vX := 0 to 3 do
                for vY := 0 to 3 do
                begin
                  if (Points[vX].X <= FElements[i].Points[vY].X + CPrec) and
                    (Points[vX].X >= FElements[i].Points[vY].X - CPrec) then
                    Points[vX] := PointF(FElements[i].Points[vY].X, Points[vX].Y) ;

                  if (Points[vX].Y <= FElements[i].Points[vY].Y + CPrec) and
                    (Points[vX].Y >= FElements[i].Points[vY].Y - CPrec)   then
                    Points[vX] := PointF(Points[vX].X, FElements[i].Points[vY].Y) ;
                end;
            end;
        end else
        begin
          FSelectedElement.ChangeLockedPoint(
            FElementStartPosition + MousePos / FPanel.Scale.X - FMouseStartPoint);
        end;
      end;

    FMouseElementPoint.X := X;
    FMouseElementPoint.Y := Y;

    vFigure :=SelectedElement.FigureByCoord(FMouseElementPoint);
    if vFigure <> nil then
    begin
      if vFigure.KeyPointLocal(FMouseElementPoint, vPoint, CPrec*2) then
      begin
        FSelectedElement.AddPointToDraw(vPoint, TAlphaColorRec.Red);
      end;
    end;
   FSelectedElement.Repaint;
  end;
end;

procedure TSSBView.DoMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
begin
  FIsMouseDown := False;
  FLockPoint := False;

  if FSelectedElement <> Nil then
    FSelectedElement.UnlockPoint;
end;

procedure TSSBView.DoZoom(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; var Handled: Boolean);
begin
  if FPanel.Scale.X + ((WheelDelta / 120) * 0.1) > 0.1 then
  begin
    FPanel.Scale.X := FPanel.Scale.X + ((WheelDelta / 120) * 0.1);
    FPanel.Scale.Y := FPanel.Scale.X;
  end;
end;}
{ TSSBController }

procedure TSSBController.DoCommand(const ACommandName: string);
begin
  DoCommand(ACommandName, []);
end;

end.
