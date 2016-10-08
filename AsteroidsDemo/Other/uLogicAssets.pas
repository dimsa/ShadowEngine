unit uLogicAssets;

interface

uses
  uSoObject;

procedure MovingThroughSides(ASoObject: TSoObject);

implementation

procedure MovingThroughSides(ASoObject: TSoObject);
var
  vWorld: TSoObject;
begin
  vWorld := ASoObject['World'].Val<TSoObject>;

  with ASoObject do begin
    X := X + ASoObject['Dx'].AsDouble;
    Y := Y + ASoObject['Dy'].AsDouble;
    Rotate := Rotate + ASoObject['Da'].AsDouble;

   if X < - Width then
     X := vWorld.Width + Width;

   if Y  < - Height then
     Y := vWorld.Height + Height;

   if X > vWorld.Width + Width  then
     X := - Width;

   if Y > vWorld.Height + Height then
     Y := - Height;
  end;
end;

end.
