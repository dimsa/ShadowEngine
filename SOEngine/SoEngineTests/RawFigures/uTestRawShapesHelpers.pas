unit uTestRawShapesHelpers;

interface

uses
  System.JSON, System.SysUtils, System.Types, UPhysics2D,
  uRawShapes;

type
  TTestShapesHelpers = class
  public
    class function CreateJsonCircle(const AX, AY, AR: Single): TJSONObject;
    class function CreateJsonPoly(const AArr: array of const): TJSONObject;
    class function CreateRawCircle(const AX, AY, AR: Single): TRawCircle;
    class function CreateRawPoly(const AArr: array of const): TRawPoly;
    class function CreateBox2DCircle(const AX, AY, AR: Single): Tb2CircleShape;
    class function CreateBox2DPoly(const AArr: array of const): Tb2PolygonShape;
    class function IsPointArrayEquals(const AArr1, AArr2: TArray<TPointF>): Boolean;
  end;

implementation

{ TRawShapesContructor }

class function TTestShapesHelpers.CreateBox2DCircle(const AX, AY,
  AR: Single): Tb2CircleShape;
begin
  Result := Tb2CircleShape.Create;

  Result.m_p.x := AX;
  Result.m_p.y := AY;
  Result.m_radius := AR;
end;

class function TTestShapesHelpers.CreateBox2DPoly(
  const AArr: array of const): Tb2PolygonShape;
var
  vL: Integer;
  i: Integer;
begin
  Result := Tb2PolygonShape.Create;

  vL := Length(AArr) div 2;

  Result.m_count := vL;

  for i := 0 to vL - 1 do
  begin
    Result.m_vertices[i].x := AArr[i * 2].VExtended^;
    Result.m_vertices[i].y := AArr[i * 2 + 1].VExtended^;
  end;
end;

class function TTestShapesHelpers.CreateJsonCircle(const AX, AY,
  AR: Single): TJSONObject;
begin
  Result := TJSONObject.Create;
  with Result do begin
    AddPair('Type', 'Circle');
    AddPair('Center', FloatToStr(AX) + ', ' + FloatToStr(AY));
    AddPair('Radius', FloatToStr(AR));
  end;
end;

class function TTestShapesHelpers.CreateJsonPoly(
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

class function TTestShapesHelpers.CreateRawCircle(const AX, AY,
  AR: Single): TRawCircle;
begin
  Result := TRawCircle.Create(AX, AY, AR);
end;

class function TTestShapesHelpers.CreateRawPoly(
  const AArr: array of const): TRawPoly;
var
  vArr: TArray<TPointF>;
  vL, i: Integer;
begin
  vL := Length(AArr) div 2;
  SetLength(vArr, vL);

 for i := 0 to vL - 1 do
  begin
    if AArr[i * 2].VType = vtInteger then
      vArr[i].X := AArr[i * 2].VInteger
    else
      vArr[i].X := AArr[i * 2].VExtended^;

    if AArr[i * 2].VType = vtInteger then
      vArr[i].Y := AArr[i * 2 + 1].VInteger
    else
      vArr[i].Y := AArr[i * 2 + 1].VExtended^;
  end;

  Result := TRawPoly.Create(vArr);
end;

class function TTestShapesHelpers.IsPointArrayEquals(const AArr1,
  AArr2: TArray<TPointF>): Boolean;
var
  i: Integer;
begin
  if Length(AArr1) <> Length(AArr2) then
    Exit(False);

  for i := 0 to High(AArr1) do
    if (AArr1[i].X <> AArr2[i].X) or (AArr1[i].X <> AArr2[i].X)  then
      Exit(False);

  Result := True;
end;

end.
