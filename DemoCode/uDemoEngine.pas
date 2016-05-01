// В примере рассмотрен вариант с созданием наследника TEngine
{
  From the box, all what we need will do TEngine2D, but this inheritance is for
  changing of Background behavior and add Parallax.
  But realy, now, for this needs you can assign BackgroundBehavior Property
  }
unit uDemoEngine;

interface

uses
  System.Types, uEngine2D;

type
  TDemoEngine = class(TEngine2D)
  private
    FParalX, FParalY: Double;
    procedure ParallaxBackgroundBehavior;
  public
    constructor Create; override;
  end;

implementation

{ TDemoEngine }

constructor TDemoEngine.Create;
begin
  inherited;
  FParalX := 0;
  FParalY := 0;
  BackgroundBehavior := ParallaxBackgroundBehavior;
end;

procedure TDemoEngine.ParallaxBackgroundBehavior;
var
  vProporX, vProporY: Double;
begin
  with Self.Image do
  begin
    FParalX := FParalX + 0.5;
    FParalY := FParalY - 1;
    vProporX := (Bitmap.Width / Background.Width) * FParalX;
    vProporY := (Bitmap.Height / Background.Height) * FParalY;

    Bitmap.Canvas.DrawBitmap(
      BackGround,
      RectF(0 + FParalX, 0 + FParalY, Background.Width, Background.Height),
      RectF(0,0, Bitmap.Width-vProporX, Bitmap.Height-vProporY),
      1,
      true);

    Bitmap.Canvas.DrawBitmap(
      BackGround,
      RectF(0,0, 0 + FParalX, 0 + FParalY),
      RectF(Bitmap.Width-vProporX, Bitmap.Height-vProporY, Bitmap.Width, Bitmap.Height),
      1,
      true);

    Bitmap.Canvas.DrawBitmap(
      BackGround,
      RectF(0 + FParalX, 0, Background.Width, 0 + FParalY),
      RectF(0, Bitmap.Height-vProporY, Bitmap.Width-vProporX, Bitmap.Height),
      1,
      true);

    Bitmap.Canvas.DrawBitmap(
      BackGround,
      RectF(0, 0 + FParalY, 0 + FParalX, BackGround.height),
      RectF(Bitmap.Width-vProporX, 0, Bitmap.Width, Bitmap.Height-vProporY),
      1,
      true);

    if FParalX >= Background.Width then
      FParalX := 0;
    if FParalY >= Background.Height then
      FParalY := 0;
    if FParalX < 0 then
      FParalX := Background.Width;
    if FParalY < 0 then
      FParalY := Background.Height;
  end;
end;

end.



