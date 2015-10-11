unit uSSBElement;

interface

uses
  FMX.Objects, FMX.Controls, System.Classes, System.Generics.Collections,
  {$IFDEF VER290} System.Math.Vectors, {$ENDIF} System.Math, FMX.Types,
  System.Types, FMX.Graphics, System.UITypes,
  uSSBFigure, uIntersectorClasses, uClasses;

type
  TSSBElement = class(TImage, ISerializable)
  private
    FFigures: TList<TSSBFigure>;
    FPoint: TPointF;
    FColor: TColor;
    FNeedDraw: Boolean;
    FLock: Boolean;
    FLockedFigure: TSSBFigure;
    FSelectedFigure: TSSBFigure; // Фигура над которой мышь
    FPath: String;
    procedure DoShapeRepaint(Sender: TObject; Canvas: TCanvas;
      const ARect: TRectF);
  public
    property Path: String read FPath write FPath;

    procedure AddCircle;
    procedure AddPoly;
    procedure AddPointToDraw(const APoint: TPointF; const AColor: TColor);
    procedure ChangeLockedPoint(const ANewPoint: TPointF);
    procedure UnlockPoint;
    function FigureByCoord(const APoint: TPointF; const ALock: Boolean = False): TSSBFigure;
    function Serialize: string;
    procedure Deserialize(const AJsonText: String);
  //  function KeyPointByCoord(const APoint: TPointF; const ALock: Boolean = False): TSSBFigure;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{ TSSBElement }

procedure TSSBElement.AddCircle;
var
  vFigure: TSSBFigure;
  vCircle: TCircle;
begin
  vFigure := TSSBFigure.Create(TSSBFigure.cfCircle);
  vCircle.X := Self.Width / 2;
  vCircle.Y := Self.Height / 2;
  vCircle.Radius := Min(Width, Height) / 2;

  vFigure.SetData(vCircle);
  FFigures.Add(vFigure);
  Self.Repaint;
end;

procedure TSSBElement.AddPointToDraw(const APoint: TPointF;
  const AColor: TColor);
begin
  FPoint := APoint;
  FColor := AColor;
  FNeedDraw := True;
end;

procedure TSSBElement.AddPoly;
var
  vFigure: TSSBFigure;
  vPoly: TPolygon;
begin
  vFigure := TSSBFigure.Create(TSSBFigure.cfPoly);
  SetLength(vPoly, 3);
  vPoly[0] := PointF(Self.Width* 0.5, Self.Height * 0.33);
  vPoly[1] := PointF(Self.Width* 0.75, Self.Height * 0.66);
  vPoly[2] := PointF(Self.Width* 0.25, Self.Height * 0.66);

  vFigure.SetData(vPoly);
  FFigures.Add(vFigure);
  Self.Repaint;
end;

procedure TSSBElement.ChangeLockedPoint(const ANewPoint: TPointF);
begin
  if FLock then
    if FLockedFigure <> nil then
      FLockedFigure.ChangeLockedPoint(ANewPoint);
end;

constructor TSSBElement.Create(AOwner: TComponent);
begin
  inherited;
  FFigures := TList<TSSBFigure>.Create;
  Self.OnPaint := DoShapeRepaint;
end;

procedure TSSBElement.Deserialize(const AJsonText: String);
begin

end;

destructor TSSBElement.Destroy;
var
  vFigure: TSSBFigure;
begin
  for vFigure in FFigures do
  begin
    FFigures.Remove(vFigure);
    vFigure.Free;
  end;
  FFigures.Clear;

  inherited;
end;

procedure TSSBElement.DoShapeRepaint(Sender: TObject; Canvas: TCanvas;
  const ARect: TRectF);
var
  i, vPr: Integer;
begin

  for i := 0 to FFigures.Count - 1 do
  begin

    if (FLock) and (FFigures[i] = FLockedFigure) then
    begin
      FFigures[i].Draw(Canvas, TAlphaColorRec.Green) end
    else begin
      if FFigures[i] = FSelectedFigure then
      begin
        FFigures[i].Draw(Canvas, TAlphaColorRec.Aquamarine)
      end
      else
        FFigures[i].Draw(Canvas);
    end;
  end;

  if FNeedDraw then
  begin
    Canvas.BeginScene();
     vPr := 5;
    Canvas.Fill.Color := FColor;
    Canvas.FillEllipse(
      RectF(
      FPoint.X - vPr,
      FPoint.Y - vPr,
      FPoint.X + vPr,
      FPoint.Y + vPr),
      0.75
   );
    Canvas.EndScene;

    FNeedDraw := False;

   //     vFigure.DrawPoint(FSelectedElement.Canvas, (vPoint + FSelectedElement.Position.Point) * FPanel.Scale.Point + FPanel.Position.Point, TAlphaColorRec.Red);
       // FSelectedElement.Canvas.EndScene();
  end;

end;

function TSSBElement.FigureByCoord(const APoint: TPointF; const ALock: Boolean): TSSBFigure;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to FFigures.Count - 1 do
  begin
    if FFigures[i].BelongPointLocal(APoint) then
    begin
      Result := FFigures[i];
      if ALock then
      begin
        FLock := True;
        FLockedFigure := Result;
      end;
    end;
  end;

  FSelectedFigure := Result;
end;

function TSSBElement.Serialize: string;
begin

end;

procedure TSSBElement.UnlockPoint;
begin
  if FLockedFigure <> nil then
    FLockedFigure.UnlockPoint;
  FLockedFigure := nil;
  FLock := False;
end;

end.
