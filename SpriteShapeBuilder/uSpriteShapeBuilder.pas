unit uSpriteShapeBuilder;

interface

uses
  System.Generics.Collections, FMX.Objects, FMX.StdCtrls, System.Classes, FMX.Forms,
  FMX.Dialogs, System.SysUtils, System.UITypes, FMX.Types, System.Types,
  uSSBElement, uNamedList, uEasyDevice, uNewFigure, uClasses;

type
  TSpriteShapeBuilder = class
  private
    FPanel: TPanel;
    FImageForSelect: TImage;
    FElements: TNamedList<TSSBElement>;
    FSelectedElement: TSSBElement;
    FIsMouseDown: Boolean;
    FMouseStartPoint, FMouseElementPoint, FElementStartPosition: TPointF;
    procedure DoAddCircle(ASender: TObject);
    procedure DoAddPoly(ASender: TObject);
    procedure DoSelect(ASender: TObject);
    procedure DoDelete(ASender: TObject);
    procedure DoZoom(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; var Handled: Boolean);
    procedure DoMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    procedure DoMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    procedure DoMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Single);
    procedure SetSelectedElement(const Value: TSSBElement);
  public
    property SelectedElement: TSSBElement read FSelectedElement write SetSelectedElement;
    procedure AddElement(const AFileName: string);
    procedure LoadProject(const AFileName: string);
    procedure SaveProject(const AFileName: string);
    constructor Create;
    procedure Init(const AProgForm: TForm);
    destructor Destroy; override;
  const
    CPrec = 5;
  end;


implementation

{ TSpriteShapeBuilder }

procedure TSpriteShapeBuilder.AddElement(const AFileName: string);
var
  vSSBElement: TSSBElement;
begin
  vSSBElement := TSSBElement.Create(FPanel);
  vSSBElement.Parent := FPanel;
  vSSBElement.Bitmap.LoadFromFile(AFileName);
  vSSBElement.Width := vSSBElement.Bitmap.Width;
  vSSBElement.Height := vSSBElement.Bitmap.Height;

  vSSBElement.OnClick := DoSelect;
  vSSBElement.OnMouseDown := DoMouseDown;
  vSSBElement.OnMouseUp := DoMouseUp;
  vSSBElement.OnMouseMove := DoMouseMove;
  vSSBElement.OnMouseWheel := DoZoom;

  FElements.Add(vSSBElement);
end;

constructor TSpriteShapeBuilder.Create;
begin
  FElements := TNamedList<TSSBElement>.Create;
end;

destructor TSpriteShapeBuilder.Destroy;
var
  vSSBElement: TSSBElement;
begin
  for vSSBElement in FElements do
    vSSBElement.Free;
  FElements.Clear;
  FElements.Free;

  inherited;
end;

procedure TSpriteShapeBuilder.DoAddCircle(ASender: TObject);
begin
  if FSelectedElement = nil then
    Exit;
  FSelectedElement.AddCircle;
end;

procedure TSpriteShapeBuilder.DoAddPoly(ASender: TObject);
begin
  if FSelectedElement = nil then
    Exit;
  FSelectedElement.AddPoly;
end;

procedure TSpriteShapeBuilder.DoDelete(ASender: TObject);
begin
  if FSelectedElement = nil then
    Exit;
  FElements.Delete(FSelectedElement);
  FSelectedElement.Free;
end;

procedure TSpriteShapeBuilder.DoMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
var
  vFigure: TNewFigure;
  vPoint: TPointF;
begin
  FIsMouseDown := True;
  FMouseStartPoint := MousePos / FPanel.Scale.X;//Point;
  FMouseElementPoint.X := X;
  FMouseElementPoint.Y := Y;
  if Sender is TSSBElement then
  begin
    SelectedElement := TSSBElement(Sender);
    FElementStartPosition := FSelectedElement.Position.Point;

    vFigure := SelectedElement.FigureByCoord(FMouseElementPoint);
    if vFigure <> nil then
    begin
      if vFigure.KeyPointLocal(FMouseElementPoint, vPoint, 10) then
      begin
        FSelectedElement.Canvas.BeginScene();
        vFigure.DrawPoint(FSelectedElement.Canvas, vPoint + FSelectedElement.Position.Point + FPanel.Position.Point, TAlphaColorRec.Red);
        FSelectedElement.Canvas.EndScene();
      end;
    end;
  end;

end;

procedure TSpriteShapeBuilder.DoMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Single);
var
  i: Integer;
  vX, vY: Integer;
  vFigure: TNewFigure;
  vPoint: TPointF;
begin
  if Sender = FSelectedElement then
  begin
    if FIsMouseDown then
      with FSelectedElement{TSSBElement(Sender)} do
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
      end;


    FMouseElementPoint.X := X;
    FMouseElementPoint.Y := Y;
    vFigure := SelectedElement.FigureByCoord(FMouseElementPoint);
    if vFigure <> nil then
    begin
      if vFigure.KeyPointLocal(FMouseElementPoint, vPoint, 10) then
      begin
        FSelectedElement.Canvas.BeginScene();
        vFigure.DrawPoint(FSelectedElement.Canvas, vPoint * FPanel.Scale.Point + FSelectedElement.Position.Point + FPanel.Position.Point, TAlphaColorRec.Red);
        FSelectedElement.Canvas.EndScene();
      end;
    end;
  end;
end;

procedure TSpriteShapeBuilder.DoMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
begin
  FIsMouseDown := False;
end;

procedure TSpriteShapeBuilder.DoSelect(ASender: TObject);
begin
  if ASender is TSSBElement then
    SelectedElement := TSSBElement(ASender);
end;

procedure TSpriteShapeBuilder.DoZoom(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; var Handled: Boolean);
begin
  if FPanel.Scale.X + ((WheelDelta / 120) * 0.1) > 0.1 then
  begin
    FPanel.Scale.X := FPanel.Scale.X + ((WheelDelta / 120) * 0.1);
    FPanel.Scale.Y := FPanel.Scale.X;
  end;
end;

procedure TSpriteShapeBuilder.Init(const AProgForm: TForm);
var
  vDelBtn, vAddCircle, vAddPoly: TCornerButton;
begin
  FPanel := TPanel(AProgForm.FindComponent('MainPanel'));
  FPanel.OnMouseWheel := DoZoom;
  FPanel.OnMouseDown := DoMouseDown;
  FPanel.OnMouseUp := DoMouseUp;
  FPanel.OnMouseMove := DoMouseMove;
  try
    FPanel.Canvas.BeginScene;
    FPanel.Canvas.Fill.Color := TAlphaColorRec.Blanchedalmond;
    FPanel.Canvas.FillRect(FPanel.BoundsRect, 0, 0, [], 1, FMX.Types.TCornerType.Round);
  finally
    FPanel.Canvas.EndScene;
  end;

  FImageForSelect := TImage(AProgForm.FindComponent('SelectImage'));

  vDelBtn := TCornerButton(AProgForm.FindComponent('DeleteImageBtn'));
  vDelBtn.OnClick := DoDelete;

  vAddCircle := TCornerButton(AProgForm.FindComponent('AddCircleBtn'));
  vAddCircle.OnClick := DoAddCircle;
  vAddPoly := TCornerButton(AProgForm.FindComponent('AddPolyBtn'));
  vAddPoly.OnClick := DoAddPoly;
end;

procedure TSpriteShapeBuilder.LoadProject(const AFileName: string);
begin

end;

procedure TSpriteShapeBuilder.SaveProject(const AFileName: string);
begin

end;

procedure TSpriteShapeBuilder.SetSelectedElement(const Value: TSSBElement);
begin
  FSelectedElement := Value;
  FImageForSelect.Bitmap.Assign(Value.Bitmap);
end;

end.


