unit uRawShapeJsonConverter;

interface

uses
  System.JSON, System.SysUtils,
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
        Result := TRawCircle.Create(vCenter.X, vCenter.Y, vRadius);
      end;
      ftPoly: begin
        if AObject.TryGetValue('Points', vProp) then
          vPoints := JsonToPointFArray(vProp);
        Result := TRawPoly.Create(vPoints);
          SetLength(vPoints, 0)
      end;
      else raise Exception.Create('Converting of this shape type from Json to RawShape not implemented yet ');
    end;
  end;
end;

class function TRawShapeJsonConverter.ConvertTo(const AShape: TRawShape): TJsonValue;
var
  vObj: TJSONObject;
  vArr: TArray<TPointF>;
  vJArr: TJSONArray;
  VJStr: TJSONString;
  i: Integer;
begin
  case AShape.FigureType of
    ftCircle: begin
      vObj := TJSONObject.Create;
      vObj.AddPair('Type', 'Circle');
      vObj.AddPair('Center', FloatToStr(AShape.GetData[0].x) + ',' + FloatToStr(AShape.GetData[0].y));
      vObj.AddPair('Radius', FloatToStr(AShape.GetData[1].x));
      Result := vObj;
    end;
    ftPoly: begin
      vObj := TJSONObject.Create;
      vObj.AddPair('Type', 'Poly');
      vArr := AShape.GetData;
      vJArr := TJSONArray.Create;
      for i := 0 to High(vArr) do
      begin
        vJStr := TJSONString.Create(FloatToStr(vArr[i].X) + ',' + FloatToStr(vArr[i].Y));
        vJArr.AddElement(VJStr);
      end;
      vObj.AddPair('Points', vJArr);

      Result := vObj;
    end;
  end;
end;

end.
