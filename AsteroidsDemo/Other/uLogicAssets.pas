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
    class procedure OnTestMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    class procedure MovingThroughSides(ASoObject: TSoObject);
    class procedure MovingByAcceleration(ASoObject: TSoObject);
    class procedure MovingToDestination(ASoObject: TSoObject);
    class procedure FollowTheShip(ASoObject: TSoObject);
    class procedure OnCollideAsteroid(ASender: TObject; AEvent: TObjectCollidedEventArgs);
    class procedure OnCollideShip(ASender: TObject; AEvent: TObjectCollidedEventArgs);
  end;

implementation

uses
  uModel, uSoSprite, uSoSound, uSoColliderObject, uSoMouseHandler;

{ TLogicAssets }

class procedure TLogicAssets.FollowTheShip(ASoObject: TSoObject);
var
  vShip: TSoObject;
begin
  vShip := ASoObject['Ship'].Val<TSoObject>;

  with ASoObject do begin
    Position.Center := vShip.Position.Center;
    Position.Rotate := vShip.Position.Rotate;
  end;

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
      AShip.Position.Rotate := NormalizeAngle(AShip.Position.Rotate - vTurnRate);
      Result.Left := 1;
      Result.Right := 0.4;
    end
    else begin
      AShip.Position.Rotate := NormalizeAngle(AShip.Position.Rotate + vTurnRate);
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
    Position.X := Position.X + vAcceleration.Dx;
    Position.Y := Position.Y + vAcceleration.Dy;
    Position.Rotate := Position.Rotate + vAcceleration.Da;
  end;
  MovingThroughSidesInner(ASoObject, ASoObject['World'].Val<TSoObject>);
end;

class procedure TLogicAssets.MovingThroughSidesInner(ASoObject, AWorld: TSoObject);
begin
  with ASoObject do begin
   if Position.X < - Width then
     Position.X := AWorld.Width + Width;

   if Position.Y  < - Height then
     Position.Y := AWorld.Height + Height;

   if Position.X > AWorld.Width + Width  then
     Position.X := - Width;

   if Position.Y > AWorld.Height + Height then
     Position.Y := - Height;
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

    Position.X := Position.X - (vAcceleration.DX * Cos((Position.Rotate + 90) * pi180));
    Position.Y := Position.Y - (vAcceleration.DY * Sin((Position.Rotate + 90) * pi180));

    if vDest.Count > 0 then
    begin

      vAngle := ArcTan2(vDest.First.Y - Position.Y, vDest.First.X - Position.X) / pi180;
      vDir := NormalizeAngle((vAngle + 90) - (Position.Rotate));

      if Distance(vDest.Last, Position.Center) <= Abs(vAcceleration.DX * 2) then
        vDest.Delete(0)
      else
        MakeTurnToDestination(ASoObject, vDir, vAcceleration.Da);
    end;

    MovingThroughSidesInner(ASoObject, ASoObject['World'].Val<TSoObject>);
  end;
end;

class procedure TLogicAssets.OnCollideAsteroid(ASender: TObject; AEvent: TObjectCollidedEventArgs);
begin
  //TSoColliderObj(ASender).Subject[Sound].Val<TSoSound>.Play;
end;

class procedure TLogicAssets.OnCollideShip(ASender: TObject; AEvent: TObjectCollidedEventArgs);
begin
  //TSoColliderObj(ASender).Subject[Sound].Val<TSoSound>.Play;
end;

class procedure TLogicAssets.OnTestMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
var
  vObj: TSoObject;
begin
  vObj := TSoMouseHandler(Sender).Subject;

  vObj.Properties[Collider].Val<TSoColliderObj>.ApplyForce(0, -10000);
end;

end.
