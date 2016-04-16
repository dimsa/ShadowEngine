unit uIntersectorClasses;

interface

uses
  System.Types;

type
  TPosition = record // Нужен для аниманиции спрайтов
    X, Y: Single;
    Rotate: Single;
    ScaleX, ScaleY: single;
  public
    function Scale: TPointF; overload;
    procedure Scale(const AScale: TPointF); overload;
    procedure Scale(const AX, AY: Single); overload;
    function XY: TPointF; overload;
    procedure XY(const AXY: TPointF); overload;
    procedure XY(const AX, AY: Single); overload;
  end;

  TCircle = packed record
    X, Y: Single;
    Radius: Single;
  end;

  TEllipse = record
    X, Y: Single;
    R1, R2: Single; // Больший и меньший радиус. Или наоборот.
    Angle: Single; // Угол поворота эллипса
  end;

  TLine = record
    A, B: TPointF;
  end;

  const
    pi180 = 0.017453292519943295769236907684886; // (1/180) * pi
    Zero = 0.0;

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

end.
