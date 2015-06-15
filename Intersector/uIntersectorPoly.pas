unit uIntersectorPoly;

interface

uses
  System.Types, System.Math,
  {$IFDEF VER290} System.Math.Vectors, {$ENDIF}
  uIntersectorFigure;

type
  TPolyFigure = class(TFigure)
  private
    FPolygon: TPolygon;
  protected
    FPoints: TArray<TPointF>;
  public
    property AsType: TPolygon read FPolygon;
    procedure AddPoint(const APoint: TPointF);
    procedure Rotate(const AValue: Single); override;
    procedure Scale(const AValue: TPointF); override;
    procedure Translate(const AValue: TPointF); override;

  end;

implementation


{ TPolyFigure }

procedure TPolyFigure.AddPoint(const APoint: TPointF);
begin
  SetLength(FPolygon, Length(FPolygon) + 1);
  FPolygon[High(FPolygon)] := APoint;
end;

procedure TPolyFigure.Rotate(const AValue: Single);
var
  i, vN: Integer;
begin
  vN := Length(FPolygon) - 1;
  for i := 0 to vN do
  begin
    FPolygon[i].X := FCenter.X * Cos(AValue) - FCenter.Y * Cos(AValue);
    FPolygon[i].Y := FCenter.X * Sin(AValue) + FCenter.Y * Cos(AValue);
  end;
end;

procedure TPolyFigure.Scale(const AValue: TPointF);
var
  i, vN: Integer;
begin
  vN := Length(FPolygon) - 1;
  for i := 0 to vN do
    FPolygon[i] := FPolygon[i] * AValue;
end;

procedure TPolyFigure.Translate(const AValue: TPointF);
var
  i, vN: Integer;
begin
  vN := Length(FPolygon) - 1;
  for i := 0 to vN do
    FPolygon[i] := FPolygon[i] + AValue;
end;

end.
