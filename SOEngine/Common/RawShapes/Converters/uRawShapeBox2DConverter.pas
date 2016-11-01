unit uRawShapeBox2DConverter;

interface

uses
  UPhysics2D, System.SysUtils,
  uGeometryClasses, uSoTypes,uRawShapes, uRawShapeBaseConverter;

type
  TRawShapeBox2DShapeConverter = class(TRawShapeConverter<Tb2Shape>)
  private
    class function B2PolyVerticesToPointArray(APoly: Tb2PolyVertices; const ACount: Integer): TArray<TPointF>;
    class function PointArrayToB2PolyVertices(APoly: TArray<TPointF>): Tb2PolyVertices;
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
        m_vertices := PointArrayToB2PolyVertices(AShape.GetData);
        m_count := TRawPoly(AShape).Count;
      end;
    end;
    else
      raise Exception.Create(Format(CConvertNotImplementedErrorMessage, [AShape.ClassName, Result.ClassName]));
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
