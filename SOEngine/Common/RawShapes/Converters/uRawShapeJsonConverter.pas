unit uRawShapeJsonConverter;

interface

uses
  System.JSON,
  uSoTypes, uGeometryClasses, uRawShapes, uRawShapeBaseConverter, uJsonUtils;

type
  TRawShapeJsonConverter = class(TRawShapeConverter<TJsonValue>)
  public
    class function ConvertFrom(const AObject: TJsonValue): TRawShape; override;
    class function ConvertTo(const AShape: TRawShape): TJsonValue; override;
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

end.
