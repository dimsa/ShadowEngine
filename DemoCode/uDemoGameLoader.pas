unit uDemoGameLoader;

interface

uses
  SysUtils, System.Types,
  uEngine2D, uEngine2DSprite, uDemoObjects, uIntersectorClasses,
  uEngine2DAnimation, uEngine2DStandardAnimations, uEngine2DClasses,
  uIntersectorCircle, uIntersectorPoly;

type
  TLoader = class
  private
    FEngine: TEngine2D;
  public
    function RandomAstroid: TLittleAsteroid;
    function BigAstroid: TAsteroid;
    function CreateShip: TShip;
    class function ShipFlyAnimation(ASubject: TSprite; const APosition: TPosition): TAnimation;
    constructor Create(AEngine: TEngine2D);
  end;


implementation

{ TLoader }

function TLoader.BigAstroid: TAsteroid;
var
  vSpr: TAsteroid;
begin
  vSpr := TAsteroid.Create(FEngine);
  vSpr.Parent := FEngine;
  vSpr.Resources := FEngine.Resources;
  vSpr.Group := 'activeobject';
  vSpr.x := Random(FEngine.Width);
  vSpr.y := Random(FEngine.Height);
  vSpr.Rotate := Random(360);
  vSpr.Scale := 0.5;
  FEngine.AddObject(vSpr); // Добавлять можно только так спрайты

  Result := vSpr;
end;

constructor TLoader.Create(AEngine: TEngine2D);
begin
  FEngine := AEngine;
end;

function TLoader.CreateShip: TShip;
var
  vSpr: TShip;
  vShape: TCircleFigure;
  vPoly: TPolyFigure;
begin
  vSpr := TShip.Create(FEngine);
  vSpr.Parent := FEngine;
  vSpr.Resources := FEngine.Resources;
  vSpr.Group := 'activeobject';
  vSpr.x := 200;//Random(FEngine.Width);
  vSpr.y := 200;//Random(FEngine.Height);
  vSpr.Rotate := Random(360);
  vSpr.Scale := 0.5;
  vPoly := TPolyFigure.Create;
  vPoly.AddPoint(PointF(0,-200));
  vPoly.AddPoint(PointF(-150,200));
  vPoly.AddPoint(PointF(150,200));

  vPoly.X := 0;
  vPoly.Y := -100;
  vShape := TCircleFigure.Create;
  vShape.X := 0;
  vShape.Y := 100;
  vShape.Radius := 100;


  vSpr.Shape.AddFigure(vPoly);
  vSpr.Shape.AddFigure(vShape);
  FEngine.AddObject('ship', vSpr); // Добавлять можно только так спрайты

  Result := vSpr;
end;

function TLoader.RandomAstroid: TLittleAsteroid;
var
  vSpr: TLittleAsteroid;
begin
  vSpr := TLittleAsteroid.Create(FEngine);
  vSpr.Parent := FEngine;
  vSpr.Resources := FEngine.Resources;
  vSpr.Group := 'backobjects';
  vSpr.x := Random(FEngine.Width);
  vSpr.y := Random(FEngine.Height);
  vSpr.Rotate := Random(360);
  vSpr.Scale := 0.5;
  FEngine.AddObject(vSpr); // Добавлять можно только так спрайты

  Result := vSpr;
end;

class function TLoader.ShipFlyAnimation(ASubject: TSprite; const APosition: TPosition): TAnimation;
var
  vRes: TMigrationAnimation;
begin
  vRes := TMigrationAnimation.Create;
//  vRes.Parent := fEngine;
  vRes.EndPos := APosition;
  vRes.TimeTotal := 500;
  vRes.Subject := ASubject;
  vRes.OnSetup := ASubject.SendToFront;

  Result := vRes;
end;

end.
