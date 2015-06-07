unit uIntersectorCircle;

interface

uses
  System.Types,
  uIntersectorFigure, uIntersectorClasses;

type
  TCircleFigure = class(TFigure)
  private
//    FRadius: Double;
    FCircle: TCircle;
    procedure SetRadius(const Value: Single);
 //   function GetRadius: Double;
{  protected
    procedure Compute; override; }
  public
    property Radius: Single read FCircle.Radius write SetRadius;
    //property ScaledRadius: Double read GetScaledRadius;
//    function IntersectWith(const AFigure: TFigure): Boolean; override;
//    function BelongPoint(const AX, AY: Double): Boolean; override;
    property AsType: TCircle read FCircle;
    procedure Rotate(const AValue: Single); override;
    procedure Scale(const AValue: TPointF); override;
    procedure Translate(const AValue: TPointF); override;
    function FigureRect: TRectF; override;
    constructor Create; override;
  end;

implementation

uses
  uIntersectorMethods;

{ TCircleFigure }

constructor TCircleFigure.Create;
begin
  inherited;
  FCenter := TPointF.Zero;
  FCircle.Radius := 0;
 { SetLength(FPoints, 2);
  // 0 - это центр окружности. 1 - Это радиус
  FPoints[0].X := 0;
  FPoints[0].Y := 0;
  FPoints[1].X := 0;
  FPoints[1].Y := 0;   }
end;

function TCircleFigure.FigureRect: TRectF;
begin
  Result := RectF(
    Self.X - Radius,
    Self.Y - Radius,
    Self.X + Radius,
    Self.Y + Radius
  );
end;

procedure TCircleFigure.Rotate(const AValue: Single);
var
  vLength: Single;
begin
  inherited;
  FCenter.X := FCenter.X * Cos(AValue) - FCenter.Y * Cos(AValue);
  FCenter.Y := FCenter.X * Sin(AValue) + FCenter.Y * Cos(AValue);

  FCircle.X := FCenter.X;
  FCircle.Y := FCenter.Y;
end;

procedure TCircleFigure.Scale(const AValue: TPointF);
begin
  inherited;
  FCircle.Radius := FCircle.Radius * AValue.X;

end;

procedure TCircleFigure.SetRadius(const Value: Single);
begin
  FCircle.Radius := Value;
end;

procedure TCircleFigure.Translate(const AValue: TPointF);
begin
  inherited;
  Self.FCenter := Self.FCenter + AValue;
  Self.FCircle.X := Self.FCenter.X;
  Self.FCircle.Y := Self.FCenter.Y;
end;

end.
