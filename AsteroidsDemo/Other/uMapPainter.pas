unit uMapPainter;

interface

uses
  FMX.Graphics, System.Math.Vectors, System.Types, FMX.Objects,
  uWorldManager, uEngine2DClasses;

type
  TMapPainter = class
  private
    FWorldManager: TWorldManager;
    FBackground: TBitmap;
    FParalX, FParalY: Single;
    procedure ParallaxBackgroundBehavior(ASender: TObject; AImage: TImage);
  public
    constructor Create(const AWorldManager: TWorldManager; const ABackgroundPath: string);
  end;

implementation

{ TMapPainter }

constructor TMapPainter.Create(const AWorldManager: TWorldManager; const ABackgroundPath: string);
begin
  FWorldManager := AWorldManager;
  FBackground := TBitmap.CreateFromFile(ABackgroundPath);
  FWorldManager.OnPaintBackground := ParallaxBackgroundBehavior;
end;

procedure TMapPainter.ParallaxBackgroundBehavior(ASender: TObject; AImage: TImage);
var
  vProporX, vProporY: Double;
begin
  with AImage do
  begin
    FParalX := FParalX + 0.5;
    FParalY := FParalY - 1;
    vProporX := (Bitmap.Width / FBackground.Width) * FParalX;
    vProporY := (Bitmap.Height / FBackground.Height) * FParalY;

    Bitmap.Canvas.DrawBitmap(
      FBackGround,
      RectF(0 + FParalX, 0 + FParalY, FBackground.Width, FBackground.Height),
      RectF(0,0, Bitmap.Width-vProporX, Bitmap.Height-vProporY),
      1,
      true);

    Bitmap.Canvas.DrawBitmap(
      FBackGround,
      RectF(0,0, 0 + FParalX, 0 + FParalY),
      RectF(Bitmap.Width-vProporX, Bitmap.Height-vProporY, Bitmap.Width, Bitmap.Height),
      1,
      true);

    Bitmap.Canvas.DrawBitmap(
      FBackGround,
      RectF(0 + FParalX, 0, FBackground.Width, 0 + FParalY),
      RectF(0, Bitmap.Height-vProporY, Bitmap.Width-vProporX, Bitmap.Height),
      1,
      true);

    Bitmap.Canvas.DrawBitmap(
      FBackGround,
      RectF(0, 0 + FParalY, 0 + FParalX, FBackGround.height),
      RectF(Bitmap.Width-vProporX, 0, Bitmap.Width, Bitmap.Height-vProporY),
      1,
      true);

    if FParalX >= FBackground.Width then
      FParalX := 0;
    if FParalY >= FBackground.Height then
      FParalY := 0;
    if FParalX < 0 then
      FParalX := FBackground.Width;
    if FParalY < 0 then
      FParalY := FBackground.Height;
  end;
end;

end.
