unit uIntersectorMethods;

interface

uses
  System.Types, System.Math, {$I 'Utils\DelphiCompatability.inc'}
  uGeometryClasses;

  function SqrDistance(const APoint1, APoint2: TPointF): Double; overload; // Находит сумму квадратов
  function SqrDistance(const AX1, AY1, AX2, AY2: Double): Double; overload; // Находит сумму квадратов
  function Distance(const APoint1, APoint2: TPointF): Double; overload; // Находит растояние между точками
  function Distance(const APoint: TPointF): Double; overload; // Находит растояние между точками, где первая в нуле
  function Distance(const AX1, AY1, AX2, AY2: Double): Double; overload; // Находит растояние между точками

    // Некоторые функции взяты или подсмотрены в FastGEO http://www.partow.net/projects/fastgeo/
  function IsPointInCircle(const APoint: TPointF; const AFigure: TCircle): Boolean;
  function IsPointInEllipse(const APoint: TPointF; const AFigure: TEllipse): Boolean; experimental;
  function IsPointInPolygon(const Point: TPointF; const Polygon: TPolygon): Boolean;

  function IsLineIntersectLine(const x1, y1, x2, y2, x3, y3, x4, y4: Double): Boolean; overload;
  function IsLineIntersectLine(const ALine1P1, ALine1P2, ALine2P1, ALine2P2: TPointF): Boolean; overload;
  function IsLineIntersectCircle(const APoint1, APoint2: TPointF; const AFigure: TCircle): Boolean;
  function IsLineIntersectPoly(const APoint1, APoint2: TPointF; const AFigure: TPolygon): Boolean;
  function IsPolyIntersectPoly(const Poly1, Poly2: TPolygon): Boolean;

    // This includes both situatution! If one figure in another and if they are intersect
  function CircleCircleCollide(const AFigure1, AFigure2: TCircle): Boolean;
  function CirclePolyCollide(const AFigure1: TPolygon; const AFigure2: TCircle): Boolean;
  function PolyPolyCollide(const AFigure1, AFigure2: TPolygon): Boolean;

  procedure Rotate(var AFigure: TPolygon; const AAngle: Single); overload;
  procedure Translate(var AFigure: TPolygon; const APoint: TPointF); overload;
  procedure Scale(var AFigure: TPolygon; const APoint: TPointF); overload;

  procedure Rotate(var AFigure: TCircle; const AAngle: Single); overload;
  procedure Translate(var AFigure: TCircle; const APoint: TPointF); overload;
  procedure Scale(var AFigure: TCircle; const APoint: TPointF); overload;
  function PolyFromRect(const ARect: TRectF): TPolygon;
  procedure AddPoint(var AFigure: TPolygon; const APoint: TPointF);
  procedure Clear(var AFigure: TPolygon);

//  function IsFiguresCollide(const AFigure1, AFigure2: TFigure): Boolean;

implementation

function PolyFromRect(const ARect: TRectF): TPolygon;
var
  vPoly: TPolygon;
begin
  SetLength(vPoly, 4);
  vPoly[0] := ARect.TopLeft;
  vPoly[1] := PointF(ARect.Right, ARect.Top);
  vPoly[2] := ARect.BottomRight;
  vPoly[3] := PointF(ARect.Left, ARect.Bottom);
  Result := vPoly;
end;

procedure Clear(var AFigure: TPolygon);
begin
  SetLength(AFigure, 0);
end;

procedure AddPoint(var AFigure: TPolygon; const APoint: TPointF);
begin
  SetLength(AFigure, Length(AFigure) + 1);
  AFigure[High(AFigure)] := APoint;
end;

procedure Rotate(var AFigure: TPolygon; const AAngle: Single); overload;
var
  i, vN: Integer;
  vTemp: Single;
begin
  vN := Length(AFigure);

  for i := 0 to vN do
  begin
    vTemp := AFigure[i].X;
    AFigure[i].X := (AFigure[i].X) * Cos(AAngle * pi180) - (AFigure[i].Y) * Sin(AAngle * pi180);
    AFigure[i].Y := (vTemp) * Sin(AAngle * pi180) + (AFigure[i].Y) * Cos(AAngle * pi180);
  end;
end;

procedure Translate(var AFigure: TPolygon; const APoint: TPointF); overload;
var
  i, vN: Integer;
begin
  vN := Length(AFigure) - 1;
  for i := 0 to vN do
    AFigure[i] := AFigure[i] + APoint;
end;

procedure Scale(var AFigure: TPolygon; const APoint: TPointF); overload;
var
  i, vN: Integer;
begin
  vN := Length(AFigure) - 1;
  for i := 0 to vN do
    AFigure[i] := AFigure[i] * APoint;
end;

procedure Rotate(var AFigure: TCircle; const AAngle: Single); overload;
var
  vTemp: Single;
begin
  vTemp := AFigure.X;
  AFigure.X := (AFigure.X) * Cos(AAngle * pi180) - (AFigure.Y) * Sin(AAngle * pi180);
  AFigure.Y := (vTemp) * Sin(AAngle * pi180) + (AFigure.Y) * Cos(AAngle * pi180);
end;

procedure Translate(var AFigure: TCircle; const APoint: TPointF); overload;
begin
  AFigure.X := AFigure.X + APoint.X;
  AFigure.Y := AFigure.Y + APoint.Y;
end;

procedure Scale(var AFigure: TCircle; const APoint: TPointF); overload;
begin
  AFigure.X := AFigure.X * APoint.X;
  AFigure.Y := AFigure.Y * APoint.Y;
end;


function CircleCircleCollide(const AFigure1,
  AFigure2: TCircle): Boolean;
begin
  Result :=
    (sqr(AFigure2.X - AFigure1.X)+sqr(AFigure2.Y - AFigure1.Y))
    <=
    Sqr(AFigure1.Radius + AFigure2.Radius);
end;

function CirclePolyCollide(const AFigure1: TPolygon; const AFigure2: TCircle): Boolean;
var
  i, vN: Integer;
begin
  if IsPointInPolygon(PointF(AFigure2.X, AFigure2.Y), AFigure1) then
    Exit(True);

  vN := Length(AFigure1) - 1;

  for i := 0 to vN do
    if IsPointInCircle(AFigure1[i], AFigure2) then
      Exit(True);
                 { TODO : Fix LineCircleIntersection }
  {for i := 0 to vN-1 do
    if IsLineIntersectCircle(AFigure1[i],AFigure1[i+1], AFigure2) then
      Exit(True);  }

  Result := False;
end;

function PolyPolyCollide(const AFigure1, AFigure2: TPolygon): Boolean;
begin
  if IsPolyIntersectPoly(AFigure1, AFigure2) then
    Exit(True);

  // If there are no intersection, so you have only two variants.
  // All points of figure1 in figure2 or vice versa.
  // So if one of point of polygon in another it is this situation

  if isPointInPolygon(AFigure1[0], AFigure2) then
    Exit(True);

  if isPointInPolygon(AFigure2[0], AFigure1) then
    Exit(True);

{ TODO : Add Line-Line Intersection of polygon }

  Result := False;

{  vN := Length(AFigure1) - 1;
  for i := 0 to vN do
    if isPointInPolygon(AFigure1[i], AFigure2)  then
      Exit(True);

  vN := Length(AFigure1) - 1;
  for i := 0 to vN do
    if isPointInPolygon(AFigure1[i], AFigure2)  then
      Exit(True);    }

end;

function Distance(const AX1, AY1, AX2, AY2: Double): Double;
begin
  Result := Sqrt(Sqr(AX2 - AX1) + Sqr(AY2 - AY1));
end;

function Distance(const APoint1, APoint2: TPointF): Double;
begin
  Result := Sqrt(Sqr(APoint2.X - APoint1.X) + Sqr(APoint2.Y - APoint1.Y));
end;

function Distance(const APoint: TPointF): Double;
begin
  Result := Sqrt((APoint.X * APoint.X) + (APoint.Y * APoint.Y));
end;

{function Instance: TIntersectionComparer;
begin
  if not Assigned(FInstance) then
    FInstance := Create;
  Result := FInstance;
end;}

function IsPolyIntersectPoly(const Poly1, Poly2: TPolygon): Boolean;
var
  i, j: Integer;
  Poly1Trailer, Poly2Trailer: Integer;
begin
  Result := False;
  if (Length(Poly1) < 3) or (Length(Poly2) < 3) then exit;
  Poly1Trailer := Length(Poly1) - 1;
  for i := 0 to Length(Poly1) - 1 do
  begin
    Poly2Trailer := Length(Poly2) - 1;
    for j := 0 to Length(Poly2) - 1 do
    begin
      if IsLineIntersectLine(Poly1[i],Poly1[Poly1Trailer],Poly2[j],Poly2[Poly2Trailer]) then
      begin
        Result := True;
        Exit;
      end;
      Poly2Trailer := j;
    end;
    Poly1Trailer := i;
  end;
end;

{function IsFiguresCollide(const AFigure1,AFigure2: TFigure): Boolean;
begin
  Result := False;
  if AFigure1 is TCircleFigure then
  begin
    if AFigure2 is TPolyFigure then
      Result := CirclePolyCollide(TPolyFigure(AFigure2).AsType, TCircleFigure(AFigure1).AsType);
    if AFigure2 is TCircleFigure then
      Result := CircleCircleCollide(TCircleFigure(AFigure1).AsType, TCircleFigure(AFigure2).AsType)
  end;

  if AFigure1 is TPolyFigure then
  begin
    if AFigure2 is TPolyFigure then
      Result := PolyPolyCollide(TPolyFigure(AFigure1).AsType, TPolyFigure(AFigure2).AsType);
    if AFigure2 is TCircleFigure then
      Result := CirclePolyCollide(TPolyFigure(AFigure1).AsType, TCircleFigure(AFigure2).AsType)
  end;
end; }

function IsLineIntersectCircle(const APoint1, APoint2: TPointF; const AFigure: TCircle): Boolean;
begin
  Result :=
    (
    ((AFigure.Radius * AFigure.Radius) * SqrDistance(
      APoint1.x - AFigure.x,
      APoint1.y - AFigure.y,
      APoint2.x - AFigure.x,
      APoint2.y - AFigure.y
    ) -
    Sqr(APoint1.x * APoint2.y - APoint2.x * APoint1.y))
    >= Zero);
end;

function IsLineIntersectLine(const x1, y1, x2, y2,
  x3, y3, x4, y4: Double): Boolean;
var
  UpperX, UpperY, LowerX, LowerY: Double;
  Ax, Bx, Cx: Double;
  Ay, By, Cy: Double;
  D, F, E: Double;
begin
  Result := false;

  Ax := x2 - x1;
  Bx := x3 - x4;

  if Ax < Zero then
  begin
    LowerX := x2;
    UpperX := x1;
  end
  else
  begin
    UpperX := x2;
    LowerX := x1;
  end;

  if Bx > Zero then
  begin
    if (UpperX < x4) or (x3 < LowerX) then
     Exit;
  end
  else if (Upperx < x3) or (x4 < LowerX) then
    Exit;

  Ay := y2 - y1;
  By := y3 - y4;

  if Ay < Zero then
  begin
    LowerY := y2;
    UpperY := y1;
  end
  else
  begin
    UpperY := y2;
    LowerY := y1;
  end;

  if By > Zero then
  begin
    if (UpperY < y4) or (y3 < LowerY) then
      Exit;
  end
  else if (UpperY < y3) or (y4 < LowerY) then
    Exit;

  Cx := x1 - x3;
  Cy := y1 - y3;
  d  := (By * Cx) - (Bx * Cy);
  f  := (Ay * Bx) - (Ax * By);

  if f > Zero then
  begin
    if (d < Zero) or (d > f) then
     Exit;
  end
  else if (d > Zero) or  (d < f) then
    Exit;

  e := (Ax * Cy) - (Ay * Cx);

  if f > Zero then
  begin
    if (e < Zero) or (e > f) then
      Exit;
  end
  else if(e > Zero) or (e < f) then
    Exit;

  Result := true;

  (*

  Simple method, yet not so accurate for certain situations and a little more
  inefficient (roughly 19.5%).
  Result := (
             ((Orientation(x1,y1, x2,y2, x3,y3) * Orientation(x1,y1, x2,y2, x4,y4)) <= 0) and
             ((Orientation(x3,y3, x4,y4, x1,y1) * Orientation(x3,y3, x4,y4, x2,y2)) <= 0)
            );
  *)

end;

function IsLineIntersectLine(const ALine1P1,
  ALine1P2, ALine2P1, ALine2P2: TPointF): Boolean;
begin
  Result := IsLineIntersectLine(
    ALine1P1.X, ALine1P1.Y, ALine1P2.X, ALine1P2.Y,
    ALine2P1.X, ALine2P1.Y, ALine2P2.X, ALine2P2.Y
  );
end;

function IsLineIntersectPoly(const APoint1,
  APoint2: TPointF; const AFigure: TPolygon): Boolean;
var
  i, n: Integer;
begin
  n := Length(AFigure) - 2;

  for i := 0 to n do
    if IsLineIntersectLine(APoint1, APoint2, AFigure[i], AFigure[i + 1]) then
      Exit(True);

  Result := IsPointInPolygon(APoint1, AFigure) or IsPointInPolygon(APoint2, AFigure);
end;

function IsPointInCircle(const APoint: TPointF; const AFigure: TCircle): Boolean;
begin
  Result :=
    (
      (APoint.X - AFigure.X) * (APoint.X - AFigure.X)
      +
      (APoint.Y - AFigure.Y) * (APoint.Y - AFigure.Y)
    ) <= AFigure.Radius * AFigure.Radius;
end;

function IsPointInEllipse(const APoint: TPointF; const AFigure: TEllipse): Boolean;
var
  vAngle: Single;
  vR2: Single; // Квадрат радиуса для горизонтального эллипса в точке между сравниваемой точкой и центром эллипса
begin
  vAngle := ArcTan2(APoint.X, APoint.Y); // Угол до точки от центра эллипса
  vR2 := (AFigure.R1 * Cos(vAngle)) * (AFigure.R2 * Sin(vAngle));
  Result :=
    (
      (APoint.X - AFigure.X) * (APoint.X - AFigure.X)
      +
      (APoint.Y - AFigure.Y) * (APoint.Y - AFigure.Y)
    ) <= vR2 * Cos(AFigure.Angle * pi180) + vR2 * Sin(AFigure.Angle * pi180)
     //  (AFigure.R1 * Cos(vAngle)) * (AFigure.R2 * Sin(vAngle));
     //  (AFigure.R1 * Cos(vAngle)) * (AFigure.R2 * Sin(vAngle));
end;

function IsPointInPolygon(const Point: TPointF;
  const Polygon: TPolygon): Boolean;
var
  i, j: Integer;
begin
  Result := False;
  if Length(Polygon) < 3 then Exit;
  j := Length(Polygon) - 1;
  for i := 0 to Length(Polygon) - 1 do
  begin
    if ((Polygon[i].y <= Point.y) and (Point.y < Polygon[j].y)) or    // an upward crossing
       ((Polygon[j].y <= Point.y) and (Point.y < Polygon[i].y)) then  // a downward crossing
    begin
      (* compute the edge-ray intersect @ the x-coordinate *)
      if ((Point.x - Polygon[i].x) < ((Polygon[j].x - Polygon[i].x) * (Point.y - Polygon[i].y) / (Polygon[j].y - Polygon[i].y))) then
        Result := not Result;
    end;
    j := i;
  end;
end;

function SqrDistance(const AX1, AY1, AX2,
  AY2: Double): Double;
begin
  Result := Sqr(AX2 - AX1) + Sqr(AY2 - AY1);
end;

function SqrDistance(const APoint1,
  APoint2: TPointF): Double;
begin
  Result := Sqr(APoint2.X - APoint1.X) + Sqr(APoint2.Y - APoint1.Y);
end;

end.
