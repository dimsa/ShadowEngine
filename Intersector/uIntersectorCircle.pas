unit uIntersectorCircle;

interface

uses
  System.Types,
  uIntersectorFigure;

type
  TCircleFigure = class(TFigure)
  private
    FRadius: Double;
    function GetRadius: Double;
    procedure SetRadius(const Value: Double);
  public
    property Radius: Double read GetRadius write SetRadius;
    function IntersectWith(const AFigure: TFigure): Boolean; override;
    function BelongPoint(const AX, AY: Double): Boolean; override;
    function FigureRect: TRectF; override;
    constructor Create;
  end;

implementation

uses
  uIntersectorComparer;

{ TCircleFigure }

function TCircleFigure.BelongPoint(const AX, AY: Double): Boolean;
begin
  Result := (sqr(Self.Position.X - AX) + Sqr(Self.Position.Y - AY)) <= Sqr(Self.Radius);
end;

constructor TCircleFigure.Create;
begin

end;

function TCircleFigure.FigureRect: TRectF;
var
  vRes: TRectF;
begin
  vRes.Top := (Self.Position.Y - Radius);
  vRes.Bottom := (Self.Position.Y + Radius);
  vRes.Left := (Self.Position.X - Radius);
  vRes.Right := (Self.Position.X + Radius);

  Result := vRes;
end;

function TCircleFigure.GetRadius: Double;
begin
  Result := Self.Position.ScaleX * FRadius;
end;

function TCircleFigure.IntersectWith(const AFigure: TFigure): Boolean;
begin

end;

procedure TCircleFigure.SetRadius(const Value: Double);
begin
  FRadius := Value;
end;

end.
