unit uGame;

interface

uses
  FMX.Graphics, System.Math.Vectors, System.Types,
  uClasses, uEngine2DClasses, uWorldManager, uUnitManager, uEasyDevice;

type
  TGame = class
  private
    FParalX, FParalY: Single;
    FWorldManager: TWorldManager;
    FUnitManager: TUnitManager;
    FBackground: TBitmap;
    procedure ParallaxBackgroundBehavior(ASender: TObject; AImage: TAnonImage);
  public
    constructor Create(const AWorldManager: TWorldManager; const AUnitManager: TUnitManager);
  end;

implementation

{ TGame }

constructor TGame.Create(const AWorldManager: TWorldManager; const AUnitManager: TUnitManager);
begin
  FWorldManager := AWorldManager;
  FUnitManager :=  AUnitManager;

  //Prepairing of background
  FBackground := TBitmap.CreateFromFile(UniPath('../../../../art/back.jpg'));

  FWorldManager.OnPaintBackground := ParallaxBackgroundBehavior;
end;

procedure TGame.ParallaxBackgroundBehavior(ASender: TObject; AImage: TAnonImage);
var
  vProporX, vProporY: Double;
  m: TMatrix;
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
