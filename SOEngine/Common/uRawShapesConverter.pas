unit uRawShapesConverter;

interface

uses
  System.Types, System.SysUtils,
  uRawShapes, System.JSON, UPhysics2D, uNewFigure, uGeometryClasses, uJsonUtils;

type
  TRawShapeConverter<T> = class abstract
  protected const
    CConvertNotImplementedErrorMessage = 'Converting from %class1 to %class2 of some shape not implemented yet';
  public
    class function ConvertFrom(const AObject: T): TRawShape; virtual; abstract;
    class function ConvertTo(const AShape: TRawShape): T; virtual; abstract;
  end;

  TRawShapeJsonConverter = class(TRawShapeConverter<TJsonValue>)
  public
    class function ConvertFrom(const AObject: TJsonValue): TRawShape; override;
    class function ConvertTo(const AShape: TRawShape): TJsonValue; override;
  end;

  TRawShapeBox2DShapeConverter = class(TRawShapeConverter<Tb2Shape>)
  private
    class function B2PolyVerticesToPointArray(APoly: Tb2PolyVertices; const ACount: Integer): TArray<TPointF>;
    class function PointArrayToB2PolyVertices(APoly: TArray<TPointF>): Tb2PolyVertices;
  public
    class function ConvertFrom(const AObject: Tb2Shape): TRawShape; override;
    class function ConvertTo(const AShape: TRawShape): Tb2Shape; override;
  end;

  TRawShapeNewFigureConverter = class(TRawShapeConverter<TNewFigure>)
  public
    class function ConvertFrom(const AObject: TNewFigure): TRawShape; override;
    class function ConvertTo(const AShape: TRawShape): TNewFigure; override;
  end;


implementation

{ TRawShapeJsonConverter }

class function TRawShapeJsonConverter.ConvertFrom(
  const AObject: TJsonValue): TRawShape;
var
  vProp: TJSONValue;
  vType: TFigureType;
  vCenter: TPointF;
  vRadius: Single;
   vPoints: TArray<TPointF>;
begin
  if AObject.TryGetValue('Type', vProp) then
  begin
    vType := JsonToShapeType(vProp);

    case vType of
      ftCircle: begin
        if AObject.TryGetValue('Center', vProp) then
          vCenter := JsonToPointF(vProp);
        if AObject.TryGetValue('Radius', vProp) then
          vRadius := JsonToSingle(vProp);
      end;
      ftPoly: begin
        if AObject.TryGetValue('Points', vProp) then
          vPoints := JsonToPointFArray(vProp);
      end;
      else raise Exception.Create('Converting of this shape type from Json to RawShape not implemented yet ');
    end;
  end;


end;

class function TRawShapeJsonConverter.ConvertTo(const AShape: TRawShape): TJsonValue;
begin

end;

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

{ TRawShapeNewFigureConverter }

class function TRawShapeNewFigureConverter.ConvertFrom(
  const AObject: TNewFigure): TRawShape;
begin

end;

class function TRawShapeNewFigureConverter.ConvertTo(
  const AShape: TRawShape): TNewFigure;
begin

end;

end.
