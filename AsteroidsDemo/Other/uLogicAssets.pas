unit uLogicAssets;

interface

uses
  uSoObject, uSoTypes, uGeometryClasses, System.Math, uIntersectorMethods, uSoObjectDefaultProperties,
  uSoColliderObjectTypes, uAcceleration;

type
  TFireKoef = record
    Left, Right: Single;
  end;

  TLogicAssets = class
  private
    class function MakeTurnToDestination(const AShip: TSoObject; const ADir, ATurnRate: Single): TFireKoef;
    class procedure MovingThroughSidesInner(ASoObject, AWorld: TSoObject);
  public
    class procedure MovingThroughSides(ASoObject: TSoObject);
    class procedure MovingByAcceleration(ASoObject: TSoObject);
    class procedure MovingToDestination(ASoObject: TSoObject);
    class procedure FollowTheShip(ASoObject: TSoObject);
    class procedure OnCollideAsteroid(ASender: TObject; AEvent: TObjectCollidedEventArgs);
    class procedure OnCollideShip(ASender: TObject; AEvent: TObjectCollidedEventArgs);
  end;

implementation

uses
  uModel, uSoSprite, uSoSound, uSoColliderObject;

{ TLogicAssets }

class procedure TLogicAssets.FollowTheShip(ASoObject: TSoObject);
var
  vShip: TSoObject;
begin
  vShip := ASoObject['Ship'].Val<TSoObject>;
  ASoObject.Center := vShip.Position.XY;
  ASoObject.Rotate := vShip.Position.Rotate;

  ASoObject[Rendition].Val<TSoSprite>.NextFrame;
  ASoObject[Rendition].Val<TSoSprite>.Opacity := 0.6 + Random(40) / 100;
end;

class function TLogicAssets.MakeTurnToDestination(const AShip: TSoObject;
  const ADir, ATurnRate: Single): TFireKoef;
var
  vTurnRate: Single;
begin
    vTurnRate := Min(Abs(ADir), ATurnRate);
    if (ADir < 0)  then
    begin
      AShip.Rotate := NormalizeAngle(AShip.Rotate - vTurnRate);
      Result.Left := 1;
      Result.Right := 0.4;
    end
    else begin
      AShip.Rotate := NormalizeAngle(AShip.Rotate + vTurnRate);
      Result.Left := 0.4;
      Result.Right := 1;
    end;

    if ((Abs(ADir) > 165) and (Abs(ADir) < 180)) or ((Abs(ADir) > 0) and (Abs(ADir) < 15)) then
    begin
      Result.Left := 1;
      Result.Right := 1;
    end;
end;

class procedure TLogicAssets.MovingByAcceleration(ASoObject: TSoObject);
var
  vAcceleration: TAcceleration;
begin
  with ASoObject do begin
    vAcceleration := ASoObject['Acceleration'].Val<TAcceleration>;
    X := X + vAcceleration.Dx;
    Y := Y + vAcceleration.Dy;
    Rotate := Rotate + vAcceleration.Da;
  end;
  MovingThroughSidesInner(ASoObject, ASoObject['World'].Val<TSoObject>);
end;

class procedure TLogicAssets.MovingThroughSidesInner(ASoObject, AWorld: TSoObject);
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

class procedure TLogicAssets.MovingThroughSides(ASoObject: TSoObject);
begin
  MovingThroughSidesInner(ASoObject, ASoObject['World'].Val<TSoObject>);
end;

class procedure TLogicAssets.MovingToDestination(ASoObject: TSoObject);
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

      vAngle := ArcTan2(vDest.First.Y - ASoObject.Y, vDest.First.X - ASoObject.X) / pi180;
      vDir := NormalizeAngle((vAngle + 90) - (Rotate));

      if Distance(vDest.Last, Center) <= Abs(vAcceleration.DX * 2) then
        vDest.Delete(0)
      else
        MakeTurnToDestination(ASoObject, vDir, vAcceleration.Da);
    end;

    MovingThroughSidesInner(ASoObject, ASoObject['World'].Val<TSoObject>);
  end;
end;

class procedure TLogicAssets.OnCollideAsteroid(ASender: TObject;
  AEvent: TObjectCollidedEventArgs);
begin
  TSoColliderObj(ASender).Subject[Sound].Val<TSoSound>.Play;
end;

class procedure TLogicAssets.OnCollideShip(ASender: TObject;
  AEvent: TObjectCollidedEventArgs);
begin
  TSoColliderObj(ASender).Subject[Sound].Val<TSoSound>.Play;
end;

end.
