unit uTestRawShapesContructors;

interface

uses
  System.JSON, System.SysUtils, System.Types,
  uRawShapes;

type
  TRawShapesContructor = class
  public
    class function CreateJsonCircle(const AX, AY, AR: Single): TJSONObject;
    class function CreateJsonPoly(const AArr: array of const): TJSONObject;
    class function CreateRawCircle(const AX, AY, AR: Single): TRawCircle;
    class function CreateRawPoly(const AArr: array of const): TRawPoly;
  end;

implementation

{ TRawShapesContructor }

class function TRawShapesContructor.CreateJsonCircle(const AX, AY,
  AR: Single): TJSONObject;
begin
  Result := TJSONObject.Create;
  with Result do begin
    AddPair('Type', 'Circle');
    AddPair('Center', FloatToStr(AX) + ', ' + FloatToStr(AY));
    AddPair('Radius', FloatToStr(AR));
  end;
end;

class function TRawShapesContructor.CreateJsonPoly(
  const AArr: array of const): TJSONObject;
var
  i, vL: Integer;
  vArr: TJSONArray;
  vS1, vS2: string;
begin
  Result := TJSONObject.Create;
  Result.AddPair('Type', 'Poly');
  vArr := TJSONArray.Create;
  vL := Length(AArr) div 2;
  for i := 0 to vL - 1 do
  begin
    vS1 := FloatToStr(AArr[i * 2].VInteger);
    vS2 := FloatToStr(AArr[i * 2 + 1].VInteger);
    vArr.Add(vS1 + ', ' + vS2);
  end;
  Result.AddPair('Points', vArr);
end;

class function TRawShapesContructor.CreateRawCircle(const AX, AY,
  AR: Single): TRawCircle;
begin
  Result := TRawCircle.Create(AX, AY, AR);
end;

class function TRawShapesContructor.CreateRawPoly(
  const AArr: array of const): TRawPoly;
var
  vArr: TArray<TPointF>;
  vL, i: Integer;
begin
  vL := Length(AArr) div 2;
  SetLength(vArr, vL);

 for i := 0 to vL - 1 do
  begin
    vArr[i].X := AArr[i * 2].VInteger;
    vArr[i].Y := AArr[i * 2].VInteger;
  end;

  Result := TRawPoly.Create(vArr);
end;

end.
