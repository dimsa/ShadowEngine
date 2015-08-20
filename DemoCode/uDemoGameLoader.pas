unit uDemoGameLoader;

interface

uses
  SysUtils, System.Types, System.Math, System.Generics.Collections,
  {$IFDEF VER290} System.Math.Vectors, {$ENDIF}
  uEngine2D, uEngine2DSprite, uEngine2DObject, uDemoObjects, uIntersectorClasses,
  uEngine2DAnimation, uEngine2DStandardAnimations, uEngine2DClasses, uEngineFormatter,
  uNewFigure, uIntersectorMethods, uEasyDevice;

type
  TLoader = class
  private
    FEngine: TEngine2D;
  public
    function RandomAstroid: TLittleAsteroid;
    function BigAsteroid: TAsteroid;
    function CreateShip: TShip;
    function Explosion(const AX, AY, AAng: Double): TExplosion;
    function ExplosionAnimation(ASubject: TSprite): TSpriteAnimation;
    function OpacityAnimation(ASubject: TSprite; const AEndOpacity: Double): TOpacityAnimation;
    function ButtonAnimation(ASubject: TEngine2dObject; const AEndScale: Double):  TMouseDownMigrationAnimation;
    function Formatter(const ASubject: tEngine2DObject; const AText: String): TEngineFormatter;
    function LevelFormatText(const AX, AY: Integer): String;
    procedure CreateLifes(FLifes: TList<TSprite>; const ACount: Integer);
    class function ShipFlyAnimation(ASubject: TSprite; const APosition: TPosition): TAnimation;
    constructor Create(AEngine: TEngine2D);
  end;

  function MonitorScale: Single;
  function SpeedModScale: Single;

implementation

function MonitorScale: Single;
begin
  Result := (
    Sqrt(uEasyDevice.getDisplaySizeInPx.X * uEasyDevice.getDisplaySizeInPx.Y) * 0.1) * 0.005;
end;

function SpeedModScale: Single;
begin
  Result := (
    Sqrt(uEasyDevice.getDisplaySizeInPx.X * uEasyDevice.getDisplaySizeInPx.Y) * 0.1) / 100;
end;

{ TLoader }

function TLoader.BigAsteroid: TAsteroid;
var
  vSpr: TAsteroid;
  vFigure: TNewFigure;
  vCircle: TCircle;
begin
  vSpr := TAsteroid.Create(FEngine);
  vSpr.Parent := FEngine;
  vSpr.Resources := FEngine.Resources;
  vSpr.CurRes := 1;
  vSpr.Group := 'activeobject';
  vSpr.x := Random(FEngine.Width);
  vSpr.y := Random(FEngine.Height);
  vFigure := TNewFigure.Create(TNewFigure.cfCircle);
  vCircle.X := 0;
  vCircle.Y := 0;
  vCircle.Radius := 50;
  vFigure.SetData(vCircle);
  vSpr.Shape.AddFigure(vFigure);
  vSpr.ScaleMod := RandomRange(60, 140) / 100;


  FEngine.AddObject(vSpr); // Добавлять можно только так спрайты

  Result := vSpr;
end;

constructor TLoader.Create(AEngine: TEngine2D);
begin
  FEngine := AEngine;
end;

procedure TLoader.CreateLifes(FLifes: TList<TSprite>; const ACount: Integer);
var
  i: Integer;
  vSpr: TSprite;
begin
  for i := 1 to ACount do
  begin
    vSpr := TSprite.Create(FEngine);
    vSpr.Resources := FEngine.Resources;
    vSpr.Group := 'survival';
    vSpr.CurRes := FEngine.Resources.IndexOf('lifeicon');
    FEngine.AddObject(vSpr);
    vSpr.Visible := True;

    Formatter(vSpr,
      'width: engine.width * 0.05; widthifhor: engine.height * 0.05; top: height * 0.8;' +
      'left: 1.05 * width * ( 0.8 + ' +IntToStr(i - 1) +');').Format;
  end;
end;

function TLoader.CreateShip: TShip;
var
  vSpr: TShip;
  vShape: TNewFigure;
  vPoly: TPolygon;
  vCircle: TCircle;
  vPoly1, vPoly2, vPoly3: TNewFigure;
begin
  vSpr := TShip.Create(FEngine);
  vSpr.Resources := FEngine.Resources;
  vSpr.Group := 'ship';
  vSpr.x := 200;//Random(FEngine.Width);
  vSpr.y := 200;//Random(FEngine.Height);
  vSpr.Rotate := Random(360);

  vPoly1 := TNewFigure.CreatePoly;
  Clear(vPoly);
  AddPoint(vPoly, PointF(0, -165));
  AddPoint(vPoly, PointF(-50, -55));
  AddPoint(vPoly, PointF(50, -55));
  Scale(vPoly, PointF(0.9, 0.9));
  vPoly1.SetData(vPoly);

  vPoly2 := TNewFigure.CreatePoly;
  Clear(vPoly);
  AddPoint(vPoly, PointF(-95, -115));
  AddPoint(vPoly, PointF(-70, -85));
  AddPoint(vPoly, PointF(-55, -45));
  AddPoint(vPoly, PointF(-115, 20));
  Scale(vPoly, PointF(0.9, 0.9));
  vPoly2.SetData(vPoly);

  vPoly3 := TNewFigure.CreatePoly;
  Clear(vPoly);
  AddPoint(vPoly, PointF(95, -115));
  AddPoint(vPoly, PointF(70, -85));
  AddPoint(vPoly, PointF(55, -45));
  AddPoint(vPoly, PointF(115, 20));
  Scale(vPoly, PointF(0.9, 0.9));
  vPoly3.SetData(vPoly);

  vShape := TNewFigure.CreateCircle;// TCircleFigure.Create;
  vCircle.X := 0;
  vCircle.Y := 50;
  vCircle.Radius := 115;
  vShape.SetData(vCircle);

  vSpr.Shape.AddFigure(vPoly1);
  vSpr.Shape.AddFigure(vPoly2);
  vSpr.Shape.AddFigure(vPoly3);
  vSpr.Shape.AddFigure(vShape);
  vSpr.Visible := False;
  FEngine.AddObject('ship', vSpr); // Добавлять можно только так спрайты

  Result := vSpr;
end;

function TLoader.Explosion(const AX, AY, AAng: Double): TExplosion;
var
  vSpr: TExplosion;
begin
  vSpr := TExplosion.Create(FEngine);
  vSpr.Parent := FEngine;
  vSpr.Resources := FEngine.Resources;
  vSpr.CurRes := 9;
  vSpr.Group := 'activeobject';
  vSpr.x := AX;
  vSpr.y := AY;
  vSpr.Rotate := AAng;
  vSpr.Scale := 0.5;
  FEngine.AddObject(vSpr); // Добавлять можно только так спрайты

  Result := vSpr;
end;

function TLoader.ExplosionAnimation(ASubject: TSprite): TSpriteAnimation;
var
  vRes: TSpriteAnimation;
  vSlides: tIntArray;
  i: Integer;
begin
  SetLength(vSlides, 4);
  for i := 0 to High(vSlides) do
    vSlides[i] := i + 9;
  vRes := TSpriteAnimation.Create;
  vRes.Parent := fEngine;
  vRes.Slides := vSlides;
  vRes.TimeTotal := 250;
  vRes.Subject := ASubject;
  vRes.OnSetup := ASubject.SendToFront;
  vRes.OnDestroy := vRes.DeleteSubject;

  Result := vRes;
end;

function TLoader.Formatter(const ASubject: tEngine2DObject;
  const AText: String): TEngineFormatter;
begin
  Result := TEngineFormatter.Create(ASubject);
  Result.Text := AText;
  FEngine.FormatterList.Insert(0, Result);
end;

function TLoader.LevelFormatText(const AX, AY: Integer): String;
begin
  Result :=
    'left:  Engine.Width * 0.2     + ((Engine.Width * 0.8) / 4) * (0' + IntToStr(AX) + ');' +
    'top:  gamelogo.bottomborder + engine.height * 0.1 + (engine.height * 0.5) / 5 * (' + IntToStr(AY) + ');' +
    'width : ((Engine.Width * 0.8) / 5);' +
    'xifhor: engine.width * 0.6 + ((Engine.Width * 0.8) / 8) * (0' + IntToStr(AX) + ');' +
    'yifhor: gamelogo.y + 0.8*height  * (0' + IntToStr(AY - 2) + ' + 0.5);' +
    'widthifhor: ((Engine.Width * 0.8) / 10)'
end;

function TLoader.OpacityAnimation(ASubject: TSprite; const AEndOpacity: Double): TOpacityAnimation;
var
  vRes: TOpacityAnimation;
begin
  vRes := TOpacityAnimation.Create;
  vRes.Parent := fEngine;
  vRes.EndOpaque := AEndOpacity;
  vRes.TimeTotal := 800;
  vRes.Subject := ASubject;

  Result := vRes;
end;

function TLoader.RandomAstroid: TLittleAsteroid;
var
  vSpr: TLittleAsteroid;
begin
  vSpr := TLittleAsteroid.Create(FEngine);
  vSpr.Parent := FEngine;
  vSpr.Resources := FEngine.Resources;
  vSpr.CurRes := vSpr.Tip + 5;
  vSpr.Group := 'backobjects';
  vSpr.x := Random(FEngine.Width);
  vSpr.y := Random(FEngine.Height);
  vSpr.Rotate := Random(360);
  //vSpr.Scale := 0.3;
  FEngine.AddObject(vSpr); // Добавлять можно только так спрайты

  Result := vSpr;
end;

function TLoader.ButtonAnimation(ASubject: TEngine2dObject;
  const AEndScale: Double): TMouseDownMigrationAnimation;
var
  vRes:  TMouseDownMigrationAnimation;
  vPos: TPosition;
begin
  vRes := TMouseDownMigrationAnimation.Create;
  vRes.Parent := FEngine;
  vPos := ASubject.Position;
  vPos.ScaleX := AEndScale;
  vPos.ScaleY := AEndScale;
  vRes.EndPos := vPos;
  vRes.TimeTotal := 200;
  vRes.Subject := ASubject;

  Result := vRes;
end;

class function TLoader.ShipFlyAnimation(ASubject: TSprite; const APosition: TPosition): TAnimation;
var
  vRes: TMigrationAnimation;
begin
  vRes := TMigrationAnimation.Create;
  vRes.EndPos := APosition;
  vRes.TimeTotal := 500;
  vRes.Subject := ASubject;
  vRes.OnSetup := ASubject.SendToFront;

  Result := vRes;
end;

end.

