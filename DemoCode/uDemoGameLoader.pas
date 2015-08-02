unit uDemoGameLoader;

interface

uses
  SysUtils, System.Types, System.Math,
  {$IFDEF VER290} System.Math.Vectors, {$ENDIF}
  uEngine2D, uEngine2DSprite, uDemoObjects, uIntersectorClasses,
  uEngine2DAnimation, uEngine2DStandardAnimations, uEngine2DClasses,
  uIntersectorCircle, uIntersectorPoly, uNewFigure, uIntersectorMethods;

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
  vFigure: TNewFigure;
  vCircle: TCircle;
begin
  vSpr := TAsteroid.Create(FEngine);
  vSpr.Parent := FEngine;
  vSpr.Resources := FEngine.Resources;
  vSpr.Group := 'activeobject';
  vSpr.x := Random(FEngine.Width);
  vSpr.y := Random(FEngine.Height);
  vSpr.Rotate := Random(360);
  vSpr.Scale := 0.5;

  vFigure := TNewFigure.Create(TNewFigure.cfCircle);
  vCircle.X := 0;
  vCircle.Y := 0;
  vCircle.Radius := 50;
  vFigure.SetData(vCircle);
  vSpr.Shape.AddFigure(vFigure);

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
  vShape: TNewFigure;
  vPoly: TPolygon;
  vCircle: TCircle;
  vPoly1, vPoly2, vPoly3: TNewFigure;

begin
  vSpr := TShip.Create(FEngine);
  vSpr.Parent := FEngine;
  vSpr.Resources := FEngine.Resources;
  vSpr.Group := 'activeobject';
  vSpr.x := 200;//Random(FEngine.Width);
  vSpr.y := 200;//Random(FEngine.Height);
  vSpr.Rotate := Random(360);
  vSpr.Scale := 0.5;

  vPoly1 := TNewFigure.CreatePoly;
  Clear(vPoly);
  AddPoint(vPoly, PointF(0, -200));
  AddPoint(vPoly, PointF(-50, 200));
  AddPoint(vPoly, PointF(50, 200));
  AddPoint(vPoly, PointF(80, -150));
  AddPoint(vPoly, PointF(120, -200));
  Translate(vPoly, PointF(100, 0));
  vPoly1.SetData(vPoly);

  vPoly2 := TNewFigure.CreatePoly;
  Clear(vPoly);
  AddPoint(vPoly, PointF(0,-200));
  AddPoint(vPoly, PointF(50,200));
  AddPoint(vPoly, PointF(-50,200));
  AddPoint(vPoly, PointF(-80,-150));
  AddPoint(vPoly, PointF(-120,-200));
  Translate(vPoly, PointF(-100, 0));
  vPoly2.SetData(vPoly);

  vPoly3 := TNewFigure.CreatePoly;
  Clear(vPoly);
  AddPoint(vPoly, PointF(0,300));
  AddPoint(vPoly, PointF(50, 0));
  AddPoint(vPoly, PointF(-50, 0));
  Translate(vPoly, PointF(0, 200));
  vPoly3.SetData(vPoly);

  vShape := TNewFigure.CreateCircle;// TCircleFigure.Create;
  vCircle.X := 0;
  vCircle.Y := 100;
  vCircle.Radius := 150;
  vShape.SetData(vCircle);

{  vSpr.Shape.AddFigure(vPoly1);
  vSpr.Shape.AddFigure(vPoly2);
  vSpr.Shape.AddFigure(vPoly3);}
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
