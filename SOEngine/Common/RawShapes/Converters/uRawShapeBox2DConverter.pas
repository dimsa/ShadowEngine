unit uRawShapeBox2DConverter;

interface

uses
  UPhysics2D, UPhysics2DTypes, System.SysUtils,
  uGeometryClasses, uSoTypes,uRawShapes, uRawShapeBaseConverter;

type
  TRawShapeBox2DShapeConverter = class(TRawShapeConverter<Tb2Shape>)
  private
    class function B2PolyVerticesToPointArray(APoly: Tb2PolyVertices; const ACount: Integer): TArray<TPointF>;
    class function PointArrayToB2PolyVertices(APoly: TArray<TPointF>): Tb2PolyVertices;
    class function PointArrayToB2PolyNormalsVertices(APoly: TArray<TPointF>): Tb2PolyVertices;
  public
    class function ConvertFrom(const AObject: Tb2Shape): TRawShape; override;
    class function ConvertTo(const AShape: TRawShape): Tb2Shape; override;
  end;

implementation

{ TRawShapeBox2DShapeConverter }

class function TRawShapeBox2DShapeConverter.ConvertFrom(
  const AObject: Tb2Shape): TRawShape;
begin
  case AObject.GetType of
    e_circleShape:
      Result :=
        TRawCircle.Create(
          Tb2CircleShape(AObject).m_p.x,
          Tb2CircleShape(AObject).m_p.y,
          Tb2CircleShape(AObject).m_radius);

    e_polygonShape:
      Result :=
        TRawPoly.Create(
          B2PolyVerticesToPointArray(
            Tb2PolygonShape(AObject).m_vertices,
            Tb2PolygonShape(AObject).m_count)
        );
    else
      raise Exception.Create(Format(CConvertNotImplementedErrorMessage, [AObject.ClassName, Result.ClassName]));
  end;
end;

class function TRawShapeBox2DShapeConverter.ConvertTo(
  const AShape: TRawShape): Tb2Shape;
begin
   case AShape.FigureType of
    ftCircle:
    begin
      Result := Tb2CircleShape.Create;
      with Tb2CircleShape(Result) do
      begin
        m_p.x := AShape.GetData[0].X;
        m_p.y := AShape.GetData[0].Y;
        m_radius := AShape.GetData[1].X;
      end;
    end;
    ftPoly:
    begin
      Result := Tb2PolygonShape.Create;
      with Tb2PolygonShape(Result) do
      begin
        m_count := TRawPoly(AShape).Count;
        m_vertices := PointArrayToB2PolyVertices(AShape.GetData);
        m_normals := PointArrayToB2PolyNormalsVertices(AShape.GetData);
        m_centroid := b2Vec2_Zero;
      end;
    end;
    else
      raise Exception.Create(Format(CConvertNotImplementedErrorMessage, [AShape.ClassName, Result.ClassName]));
  end;
end;

class function TRawShapeBox2DShapeConverter.PointArrayToB2PolyNormalsVertices(
  APoly: TArray<TPointF>): Tb2PolyVertices;
var
  i, vNext: Integer;
  vDx, vDy, vL: Single;

begin
  // Suppose I have a line segment going from (x1,y1) to (x2,y2).
  // How do I calculate the normal vector perpendicular to the line?
  // if we define dx=x2-x1 and dy=y2-y1, then the normals are (-dy, dx) and (dy, -dx).

  for i := 0 to High(APoly) do
  begin
    vNext := i + 1;
    if vNext = High(APoly) then
      vNext := 0;
    vDx := APoly[vNext].X - APoly[i].X;
    vDy := APoly[vNext].Y - APoly[i].Y;
    vL := Sqrt(Sqr(vDx) + Sqr(vDy));

    if vL <> 0 then
    begin
      Result[i].x := -vDy / vL;
      Result[i].y := vDx / vL;
    end else
      Result[i] :=b2Vec2_Zero;
  end;
end;

class function TRawShapeBox2DShapeConverter.PointArrayToB2PolyVertices(
  APoly: TArray<TPointF>): Tb2PolyVertices;
var
  i: Integer;
begin
  //SetLength(Result, Length(APoly));
  for i := 0 to High(APoly) do
  begin
    Result[i].x := APoly[i].X;
    Result[i].y := APoly[i].Y;
  end;
end;

class function TRawShapeBox2DShapeConverter.B2PolyVerticesToPointArray(
  APoly: Tb2PolyVertices; const ACount: Integer): TArray<TPointF>;
var
  i: Integer;
begin
  SetLength(Result, ACount);
  for i := 0 to ACount - 1 do
    Result[i] := TPointF.Create(APoly[i].X, APoly[i].Y)
end;

end.
