unit uSSBFigure;

interface

uses
  System.Types, {$IFDEF VER290} System.Math.Vectors, {$ENDIF} System.Math,
  uNewFigure, uIntersectorMethods, System.JSON, System.SysUtils,
  uClasses;

type
//  TInterfacedFigure = class(TNewFigure, TInterfacedObject)

//  /end;

  TSSBFigure = class(TNewFigure, ISerializable)
  private
    FLockedIndex: Integer; // Номер запомненной точки в массиве
    FLockedPoint: TPointF; // Номер запомненной точки в массиве
  public
    function Serialize: TJSONObject;
    procedure Deserialize(const AJson: TJSONObject);
    function KeyPointLocal(const ATestPosition: TPointF; out AKeyPoint: TPointF; const ADistance: Double; const ALock: Boolean = false): Boolean; // Находит ближайшую к точке ATestPosition, находящуюся в на расстоянии не больше ADistance ключевую точку и возвращает её координаты в AKeyPoint. Если стоит ALock, то точка запоминается. True - если точка найдена
    procedure ChangeLockedPoint(const ANewPoint: TPointF);
    procedure UnlockPoint;
    constructor Create(const AKind: Byte); override;
  end;

implementation

{ TSSBFigure }

procedure TSSBFigure.ChangeLockedPoint(const ANewPoint: TPointF);
var
  vD: single;
begin
  if FLockedIndex <> -1 then
  begin
    if FKind = cfPoly then
      FData[FLockedIndex] := ANewPoint;
    if FKind = cfCircle then
    begin
      if FLockedIndex = 0 then
        FData[FLockedIndex] := ANewPoint;

      if FLockedIndex = 1 then
      begin
        vD := Distance(FData[0], ANewPoint); //Distance(FData[0], FLockedPoint) - Distance(FData[0], ANewPoint);
        FData[FLockedIndex] := PointF(vD, vD);
      end;

    end;



  end;

end;

constructor TSSBFigure.Create(const AKind: Byte);
begin
  inherited;
  FLockedIndex := -1;
end;

procedure TSSBFigure.Deserialize(const AJson: TJSONObject);
var
  vItem: TJSONValue;
  vCoord: TJSONObject;
  vArr: TJSONArray;
  vPoint: TPointF;
  vPolygon: TPolygon;
begin
  vArr := TJSONArray(AJson.GetValue('Data'));

  for vItem in vArr do
  begin
    vPoint.X := TJSONObject(vItem).GetValue('x').Value.ToSingle();
    vPoint.Y := TJSONObject(vItem).GetValue('y').Value.ToSingle();
    vPolygon.Add(vPoint);
  end;
  SetData(vPolygon);
end;

function TSSBFigure.KeyPointLocal(const ATestPosition: TPointF;
  out AKeyPoint: TPointF; const ADistance: Double;
  const ALock: Boolean): Boolean;
var
  vCenterToPoint, vCenterToRadius: Double;
  vArcTan: Double;
  vPoly: TPolygon;
  i: Integer;
begin
   case FKind of
    cfCircle:
      begin
        vCenterToPoint := Distance(ATestPosition, FData[0]);
        vCenterToRadius := FData[1].X;//Distance(PointF(0,0), FData[1]);
        if (FData[1].X - vCenterToPoint) < vCenterToPoint then
        begin
          if (vCenterToPoint <= FData[1].X + (ADistance)) and
           (vCenterToPoint >= FData[1].X - (ADistance))
          then
          begin
            vArcTan := ArcTan2(ATestPosition.Y - FData[0].Y, ATestPosition.X - FData[0].X );
            AKeyPoint := PointF(FData[0].X + vCenterToRadius * Cos(vArcTan), vCenterToRadius * Sin(vArcTan) + FData[0].Y);

            if ALock then
            begin
              FLockedIndex := 1;
              FLockedPoint := AKeyPoint;
            end;
///AKeyPoint := ATestPosition;

            Exit(True);
          end;
        end else
        begin
          if vCenterToPoint <= (ADistance) then
          begin
            AKeyPoint := FData[0];
            if ALock then
            begin
              FLockedIndex := 0;
              FLockedPoint := AKeyPoint;
            end;

            Exit(True);
          end;
        end;
      end;
    cfPoly:
      begin
        vPoly := Self.AsPoly;
        for i := 0 to vPoly.Count do
        begin
          if Distance(vPoly[i], ATestPosition) <= ADistance then
          begin
            AKeyPoint := vPoly[i];
            if ALock then
            begin
              FLockedIndex := i;
              FLockedPoint := AKeyPoint
            end;

            Exit(True);
          end;
        end;

      end;
  end;
  Result := False;
end;

function TSSBFigure.Serialize: TJSONObject;
var
  vObj, vCoord: TJSONObject;
  vArr: TJSONArray;
  i: Integer;
begin
  vObj := TJSONObject.Create;
  vArr := TJSONArray.Create;

  for i := 0 to Length(FData) - 1 do
  begin
    vCoord := TJSONObject.Create;
    vCoord.AddPair('x', FloatToStr(FData[i].X));
    vCoord.AddPair('y', FloatToStr(FData[i].Y));
    vArr.AddElement(vCoord);
  end;

  vObj.AddPair('Kind', IntToStr(Self.Kind));
  vObj.AddPair('Data', vArr);

  Result := vObj;
end;

procedure TSSBFigure.UnlockPoint;
begin
  FLockedIndex := -1;
end;

end.
