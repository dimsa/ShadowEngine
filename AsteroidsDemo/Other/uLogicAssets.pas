unit uLogicAssets;

interface

uses
  uSoObject;

procedure MovingThroughSides(ASoObject: TSoObject);

implementation

procedure MovingThroughSides(ASoObject: TSoObject);
begin
  with ASoObject do begin
    X := X + ASoObject['Dx'].AsDouble;
    Y := Y + ASoObject['Dy'].AsDouble;
    Rotate := Rotate + ASoObject['Da'].AsDouble;

   if X < - ASoObject['Width'].AsDouble * 0.5 then
     X := ASoObject['WorldWidth'].AsDouble + ASoObject['Width'].AsDouble;

   if Y  < - ASoObject['Height'].AsDouble * 0.5 then
     Y := ASoObject['WorldHeight'].AsDouble + ASoObject['Height'].AsDouble;

   if X > ASoObject['WorldWidth'].AsDouble + ASoObject['Width'].AsDouble  then
     X := - ASoObject['Width'].AsDouble * 0.5;

   if Y > ASoObject['WorldHeight'].AsDouble + ASoObject['Height'].AsDouble then
     Y := - ASoObject['Height'].AsDouble * 0.5;
  end;
end;

end.
