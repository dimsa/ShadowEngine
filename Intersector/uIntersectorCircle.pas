unit uIntersectorCircle;

interface

uses
  System.Types,
  uIntersectorFigure;

type
  TCircleFigure = class(TFigure)
  private
    FRadius: Double;
    procedure SetRadius(const Value: Double);
    function GetRadius: Double;
  protected
    procedure Compute; override;
  public
    property Radius: Double read GetRadius write SetRadius;
    //property ScaledRadius: Double read GetScaledRadius;
    function IntersectWith(const AFigure: TFigure): Boolean; override;
    function BelongPoint(const AX, AY: Double): Boolean; override;
    function FigureRect: TRectF; override;
    constructor Create; override;
  end;

implementation

uses
  uIntersectorMethods;

{ TCircleFigure }

function TCircleFigure.BelongPoint(const AX, AY: Double): Boolean;
begin
  Result := IsPointInCircle(PointF(AX, AY), Self);
//  Result := (sqr(Self.Position.X - AX) + Sqr(Self.Position.Y - AY)) <= Sqr(Self.Radius);
end;

procedure TCircleFigure.Compute;
begin
  inherited;

end;

constructor TCircleFigure.Create;
begin
  inherited;
  SetLength(FOriginalPoints, 2);
  // 0 - это центр окружности. 1 - Это радиус
  FOriginalPoints[0].X := 0;
  FOriginalPoints[0].Y := 0;
  FOriginalPoints[1].X := 0;
  FOriginalPoints[1].Y := 0;
end;

function TCircleFigure.FigureRect: TRectF;
begin
  Result := RectF(
    Self.Points[0].X - Radius,
    Self.Points[0].Y - Radius,
    Self.Points[0].X + Radius,
    Self.Points[0].Y + Radius
  );
end;

function TCircleFigure.GetRadius: Double;
begin
  Result := Self.FOriginalPoints[1].X;
end;

{function TCircleFigure.GetScaledRadius: Double;
begin
  Result := Self.Position.ScaleX * FRadius;
end;}

function TCircleFigure.IntersectWith(const AFigure: TFigure): Boolean;
begin
  Result := IsFiguresCollide(Self, AFigure);
end;

procedure TCircleFigure.SetRadius(const Value: Double);
begin
  Self.FOriginalPoints[1].X := Value;
end;

end.
