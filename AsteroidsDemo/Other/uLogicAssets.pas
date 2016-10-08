unit uLogicAssets;

interface

uses
  uSoObject;

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

procedure MovingThroughSides(ASoObject: TSoObject);

implementation

procedure MovingThroughSides(ASoObject: TSoObject);
var
  vWorld: TSoObject;
begin
  vWorld := ASoObject['World'].Val<TSoObject>;

  with ASoObject do begin
    X := X + ASoObject['Acceleration'].Val<TAcceleration>.Dx;
    Y := Y + ASoObject['Acceleration'].Val<TAcceleration>.Dy;
    Rotate := Rotate + ASoObject['Acceleration'].Val<TAcceleration>.Da;

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
