unit uDemoGameLoader;

interface

uses
  SysUtils,
  uEngine2D, uEngine2DSprite, uDemoObjects,
  uEngine2DAnimation, uEngine2DStandardAnimations, uEngine2DClasses;

type
  TLoader = class
  private
    FEngine: TEngine2D;
  public
    function RandomAstroid: TLittleAsteroid;
    function CreateShip: TShip;
    class function ShipFlyAnimation(ASubject: TSprite; const APosition: TPosition): TAnimation;
    constructor Create(AEngine: TEngine2D);

  end;


implementation

{ TLoader }

constructor TLoader.Create(AEngine: TEngine2D);
begin
  FEngine := AEngine;
end;

function TLoader.CreateShip: TShip;
var
  vSpr: TShip;
begin
  vSpr := TShip.Create(FEngine);
  vSpr.Parent := FEngine;
  vSpr.Resources := FEngine.Resources;
  vSpr.Group := 'activeobject';
  vSpr.x := 200;//Random(FEngine.Width);
  vSpr.y := 200;//Random(FEngine.Height);
  vSpr.Rotate := Random(360);
  vSpr.Scale := 0.5;
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
