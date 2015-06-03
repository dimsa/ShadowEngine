unit uIntersectorComparer;

interface

uses
  System.Types, System.Math,
  {$IFDEF VER290} System.Math.Vectors, {$ENDIF}
  uIntersectorFigure, uIntersectorCircle, uIntersectorRectangle,
  uIntersectorTriangle;

type
  TIntersectionComparer = class
  strict private
    class var FInstance: TIntersectionComparer;
    constructor Create;
  public
    class function Instance: TIntersectionComparer;

    class function SqrDistance(const APoint1, APoint2: TPointF): Double; overload; // Находит сумму квадратов
    class function SqrDistance(const AX1, AY1, AX2, AY2: Double): Double; overload; // Находит сумму квадратов
    class function Distance(const APoint1, APoint2: TPointF): Double; overload; // Находит растояние между точками
    class function Distance(const AX1, AY1, AX2, AY2: Double): Double; overload; // Находит растояние между точками

    // Некоторые функции взяты или подсмотрены в FastGEO http://www.partow.net/projects/fastgeo/
    class function IsPointInCircle(const APoint: TPointF; const AFigure: TCircleFigure): Boolean;
    class function IsPointInPolygon(const Point: TPointF; const Polygon: TPolygon): Boolean;

    class function IsLineIntersectLine(const x1, y1, x2, y2, x3, y3, x4, y4: Double): Boolean; overload;
    class function IsLineIntersectLine(const ALine1P1, ALine1P2, ALine2P1, ALine2P2: TPointF): Boolean; overload;
    class function IsLineIntersectCircle(const APoint1, APoint2: TPointF; const AFigure: TCircleFigure): Boolean;
    class function IsLineIntersectPoly(const APoint1, APoint2: TPointF; const AFigure: TPolygon): Boolean;
    class function IsPolyIntersectPoly(const Poly1, Poly2: TPolygon): Boolean; static;

    // This includes both situatution! If one figure in another and if they are intersect
    class function CircleCircleCollide(const AFigure1, AFigure2: TCircleFigure): Boolean;
    function IsFiguresCollide(const AFigure1, AFigure2: TFigure): Boolean; virtual;
  const
    Zero = 0.0;
  end;

implementation

{ TIntersectionComparer }

class function TIntersectionComparer.CircleCircleCollide(const AFigure1,
  AFigure2: TCircleFigure): Boolean;
begin
  Result :=
    (sqr(AFigure1.Position.X-AFigure2.Position.X)+sqr(AFigure1.Position.Y-AFigure2.Position.Y))
    <=
    (AFigure1.Radius + AFigure2.Radius);
end;

constructor TIntersectionComparer.Create;
begin

end;

class function TIntersectionComparer.Distance(const AX1, AY1, AX2, AY2: Double): Double;
begin
  Result := Sqrt((AX2 - AX1) + (AY2 - AY1));
end;

class function TIntersectionComparer.Distance(const APoint1, APoint2: TPointF): Double;
begin
  Result := Sqrt((APoint2.X - APoint1.X) + (APoint2.Y - APoint1.Y));
end;

class function TIntersectionComparer.Instance: TIntersectionComparer;
begin
  if not Assigned(FInstance) then
    FInstance := TIntersectionComparer.Create;
  Result := FInstance;
end;

class function TIntersectionComparer.IsPolyIntersectPoly(const Poly1, Poly2: TPolygon): Boolean;
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

function TIntersectionComparer.IsFiguresCollide(const AFigure1,
  AFigure2: TFigure): Boolean;
begin
  Result := False;
  if (AFigure1 is TCircleFigure) and (AFigure2 is TCircleFigure) then
    Result := CircleCircleCollide(TCircleFigure(AFigure1), TCircleFigure(AFigure2));
end;

class function TIntersectionComparer.IsLineIntersectCircle(
  const APoint1, APoint2: TPointF; const AFigure: TCircleFigure): Boolean;
//var
//  x1, y1, x2, y2: Double;
begin
  (*
    It is assumed that an intersection of a circle by a line
    is either a full intersection (2 points), partial intersection
    (1 point), or tangential.
    Anything else will Result in a false output.
  *)
 { Line[1].x - Circle.x;
  Line[1].y - Circle.y;
  Line[2].x - Circle.x;
  Line[2].y - Circle.y;     }
//  Result := GreaterThanOrEqual(((Circle.Radius * Circle.Radius) * LayDistance(x1,y1,x2,y2) - Sqr(x1 * y2 - x2 * y1)), Zero);
  Result :=
    (
    ((AFigure.Radius * AFigure.Radius) * SqrDistance(
      APoint1.x - AFigure.Position.x,
      APoint1.y - AFigure.Position.y,
      APoint2.x - AFigure.Position.x,
      APoint2.y - AFigure.Position.y
    ) -
    Sqr(APoint1.x * APoint2.y - APoint2.x * APoint1.y))
    >= Zero);
end;

class function TIntersectionComparer.IsLineIntersectLine(const x1, y1, x2, y2,
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

class function TIntersectionComparer.IsLineIntersectLine(const ALine1P1,
  ALine1P2, ALine2P1, ALine2P2: TPointF): Boolean;
begin
  Result := IsLineIntersectLine(
    ALine1P1.X, ALine1P1.Y, ALine1P2.X, ALine1P2.Y,
    ALine2P1.X, ALine2P1.Y, ALine2P2.X, ALine2P2.Y
  );
end;

class function TIntersectionComparer.IsLineIntersectPoly(const APoint1,
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

class function TIntersectionComparer.IsPointInCircle(const APoint: TPointF;
  const AFigure: TCircleFigure): Boolean;
begin
  Result :=
    (
      (APoint.X - AFigure.Position.X) * (APoint.X - AFigure.Position.X)
      +
      (APoint.Y - AFigure.Position.Y) * (APoint.Y - AFigure.Position.Y)
    ) <= AFigure.Radius * AFigure.Radius;
end;

class function TIntersectionComparer.IsPointInPolygon(const Point: TPointF;
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
      if (Point.x - Polygon[i].x < ((Polygon[j].x - Polygon[i].x) * (Point.y - Polygon[i].y) / (Polygon[j].y - Polygon[i].y))) then
        Result := not Result;
    end;
    j := i;
  end;
end;

class function TIntersectionComparer.SqrDistance(const AX1, AY1, AX2,
  AY2: Double): Double;
begin
  Result := (AX2 - AX1) + (AY2 - AY1);
end;

class function TIntersectionComparer.SqrDistance(const APoint1,
  APoint2: TPointF): Double;
begin
  Result := (APoint2.X - APoint1.X) + (APoint2.Y - APoint1.Y);
end;

end.


//begin
/// Checks if the two polygons are intersecting.
//bool IsPolygonsIntersecting(Polygon a, Polygon b)
//begin
 { for
    foreach (var polygon in new[] ( a, b ))
    begin
        for (int i1 = 0; i1 < polygon.Points.Count; i1++)
        begin
            i2 := (i1 + 1) mod polygon.Points.Count;
            var p1 := polygon.Points[i1];
            var p2 := polygon.Points[i2];

            var normal = new Point(p2.Y - p1.Y, p1.X - p2.X);

            double? minA = null, maxA = null;
            foreach (var p in a.Points)
            begin
                var projected = normal.X * p.X + normal.Y * p.Y;
                if ((minA = null) or (projected < minA)) then
                    minA := projected;
                if ((maxA = null) or (projected > maxA))
                    maxA := projected;
            end;

            double? minB = null, maxB = null;
            foreach (var p in b.Points)
            begin
                var projected = normal.X * p.X + normal.Y * p.Y;
                if ((minB = null) or (projected < minB))
                    minB := projected;
                if ((maxB = null) or (projected > maxB))
                    maxB := projected;
            end;

            if (maxA < minB || maxB < minA)
                return false;
        end;
    end;
    return true;  }
