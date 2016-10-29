unit uGeometryClasses;

interface

uses
   {$I 'Utils\DelphiCompatability.inc'} System.Types;

type
  TFigureType = (ftEllipse, ftRect, ftCircle, ftPoly);

  TPosition = record // Нужен для аниманиции спрайтов
    X, Y: Single;
    Rotate: Single;
    ScaleX, ScaleY: single;
    function Scale: TPointF; overload;
    procedure Scale(const AScale: TPointF); overload;
    procedure Scale(const AX, AY: Single); overload;
    function XY: TPointF; overload;
    procedure XY(const AXY: TPointF); overload;
    procedure XY(const AX, AY: Single); overload;
   // constructor Create(AX: Single = 0; AY: Single = 0; ARotate: Single = 0; AScaleX: Single = 1; AScaleY: Single = 1);// = (X: 0; Y: 0; Rotate: 0; ScaleX: 1; ScaleY: 1);
//
  end;

  {
  CJustifyPoints: array[TObjectJustify] of TRect = (
    (Left: -2; Top: -2; Right: 0; Bottom: 0), (Left: -1; Top: -2; Right: 1; Bottom: 0), (Left: 0; Top: -2; Right: 2; Bottom: 0),
    (Left: -2; Top: -1; Right: 0; Bottom: 1), (Left: -1; Top: -1; Right: 1; Bottom: 1), (Left: 0; Top: -1; Right: 2; Bottom: 1),
    (Left: -2; Top:  0; Right: 0; Bottom: 2), (Left: -1; Top:  0; Right: 1; Bottom: 2), (Left: 0; Top:  0; Right: 2; Bottom: 2));}

  TCircle = packed record
    X, Y: Single;
    Radius: Single;
  end;

  TEllipse = record
    X, Y: Single;
    R1, R2: Single; // Больший и меньший радиус. Или наоборот.
    Angle: Single; // Угол поворота эллипса
  end;

  TPolygon = System.Math.Vectors.TPolygon;

  TLine = record
    A, B: TPointF;
  end;

  const
    pi180 = 0.017453292519943295769236907684886; // (1/180) * pi
    Zero = 0.0;

  function NormalizeAngle(AAngle: Single): Single;

implementation

{ TPosition }

function TPosition.Scale: TPointF;
begin
  Result := PointF(ScaleX, ScaleY);
end;

procedure TPosition.Scale(const AScale: TPointF);
begin
  ScaleX := AScale.X;
  ScaleY := AScale.Y;
end;

{constructor TPosition.Create;
begin
  X := 0;
  Y := 0;
  Rotate := 0;
  ScaleX := 1;
  ScaleY := 1;
end; }

procedure TPosition.Scale(const AX, AY: Single);
begin
  ScaleX := AX;
  ScaleY := AY;
end;

function TPosition.XY: TPointF;
begin
  Result := PointF(X, Y);
end;

procedure TPosition.XY(const AXY: TPointF);
begin
  X := AXY.X;
  Y := AXY.Y;
end;

procedure TPosition.XY(const AX, AY: Single);
begin
  X := AX;
  Y := AY;
end;

{function TPosition.Zero: TPosition;
begin
  X := 0;
  Y := 0;
  Rotate := 0;
  ScaleX := 1;
  ScaleY := 1;
end;  }

function NormalizeAngle(AAngle: Single): Single;
begin
  if AAngle < - 180 then
  begin
    AAngle := AAngle + 360;
    Exit(NormalizeAngle(AAngle));
  end;

  if AAngle > 180 then
  begin
    AAngle := AAngle - 360;
    Exit(NormalizeAngle(AAngle));
  end;

  Result := AAngle;
end;

end.
