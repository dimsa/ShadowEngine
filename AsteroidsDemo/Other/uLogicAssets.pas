unit uLogicAssets;

interface

uses
  uSoObject, uSoTypes, uGeometryClasses, System.Math, uIntersectorMethods;

type
  TAcceleration = class
  private
    FDx, FDy, FDa: Single;
    procedure SetDa(const Value: Single);
    procedure SetDx(const Value: Single);
    procedure SetDy(const Value: Single);
  public
    property Dx: Single read FDx write SetDx;
    property Dy: Single read FDy write SetDy;
    property Da: Single read FDa write SetDa;
    constructor Create;
  end;

  TFireKoef = record
    Left, Right: Single;
  end;

procedure MovingByAcceleration(ASoObject: TSoObject);
procedure MovingToDestination(ASoObject: TSoObject);

implementation

uses
  uModel;

procedure MovingThroughSides(ASoObject, AWorld: TSoObject);
begin
  with ASoObject do begin
   if X < - Width then
     X := AWorld.Width + Width;

   if Y  < - Height then
     Y := AWorld.Height + Height;

   if X > AWorld.Width + Width  then
     X := - Width;

   if Y > AWorld.Height + Height then
     Y := - Height;
  end;
end;

procedure MovingByAcceleration(ASoObject: TSoObject);
var
  vAcceleration: TAcceleration;
begin
  with ASoObject do begin
    vAcceleration := ASoObject['Acceleration'].Val<TAcceleration>;
    X := X + vAcceleration.Dx;
    Y := Y + vAcceleration.Dy;
    Rotate := Rotate + vAcceleration.Da;
  end;
  MovingThroughSides(ASoObject, ASoObject['World'].Val<TSoObject>);
end;

function MakeTurnToDestination(const AShip: TSoObject; const ADir, ATurnRate: Single): TFireKoef;
begin
    if (ADir < -90) or (ADir > 90) then
    begin
      AShip.Rotate := AShip.Rotate - ATurnRate;
      Result.Left := 1;
      Result.Right := 0.4;
    end
    else begin
      AShip.Rotate := AShip.Rotate + ATurnRate;
      Result.Left := 0.4;
      Result.Right := 1;
    end;

    if ((Abs(ADir) > 165) and (Abs(ADir) < 180)) or ((Abs(ADir) > 0) and (Abs(ADir) < 15)) then
    begin
      Result.Left := 1;
      Result.Right := 1;
    end;
end;

procedure MovingToDestination(ASoObject: TSoObject);
var
  vAcceleration: TAcceleration;
  vDest: TList<TPointF>;
  vAngle, vDir: Single;
begin
  with ASoObject do begin
    vAcceleration := ASoObject['Acceleration'].Val<TAcceleration>;
    vDest := ASoObject['Destinations'].Val<TList<TPointF>>;

    X := X - (vAcceleration.DX * Cos((Rotate + 90) * pi180));
    Y := Y - (vAcceleration.DY * Sin((Rotate + 90) * pi180));

    if vDest.Count > 0 then
    begin

      vAngle := ArcTan2(vDest.First.Y - ASoObject.Y, vDest.First.X - ASoObject.x) / pi180;
      vDir := (vAngle - ASoObject.Rotate);

      if Distance(vDest.First, Center) <= vAcceleration.DX * 2 then
        vDest.Delete(0);

      MakeTurnToDestination(ASoObject, vDir, vAcceleration.Da);
    end;

    MovingThroughSides(ASoObject, ASoObject['World'].Val<TSoObject>);
  end;
end;

{ TAccelerate }

constructor TAcceleration.Create;
begin
  FDx := 1;
  FDy := 1;
  FDa := pi / 90;
end;

procedure TAcceleration.SetDa(const Value: Single);
begin
  FDa := Value;
end;

procedure TAcceleration.SetDx(const Value: Single);
begin
  FDx := Value;
end;

procedure TAcceleration.SetDy(const Value: Single);
begin
  FDy := Value;
end;

end.
