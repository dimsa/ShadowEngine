unit uDemoObjects;

interface

uses
  FMX.Types, System.UITypes, System.Classes, System.Types, System.SysUtils, System.Math,
  uEngine2DSprite, uEngine2DAnimation, uEngine2DStandardAnimations, uEngine2DClasses,
  uEngine2DObject, uIntersectorClasses;

type

  TShipFire = class(TSprite)
  private
    FTip: Byte;
//    FShip: TShip;
  public
    property Tip: Byte read FTip write FTip; // Тип огня
//    property Ship: TShip read FShip write FShip; // Корабль, коотру принадлежит огонь
    procedure Repaint; override;
    constructor Create(newParent: pointer); override;
  end;

  TShip = class(TSprite)
  private
    FLeftFire: TShipFire;
    FRightFire: TShipFire;
    procedure MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single); virtual;
  public
    property LeftFire: TShipFire read FLeftFire write FLeftFire;
    property RightFire: TShipFire read FRightFire write FRightFire;
    procedure Repaint; override;
    constructor Create(newParent: pointer); override;
  end;

  TAsteroid = class(TSprite)
  private
    FDx, FDy, FDA: Double; // Сдвиги
    FNotChange: Integer; // Кол-во тиков, которое не будет изменяться направление при коллайдер
  public
    property DX: Double read FDx write FDx;
    property DY: Double read FDy write FDy;
    procedure Repaint; override;
    procedure Collide(const AObject: TEngine2DObject);
    constructor Create(newParent: pointer); override;
  end;

  TLittleAsteroid = class(TSprite)
  private
    FTip: Byte;
    FDx, FDY: Double; // Сдвиги
  public
    property Tip: Byte read FTip write FTip; // Тип астеройда
    procedure Repaint; override;
    constructor Create(newParent: pointer); override;
  end;

  TStar = class(TSprite)
  public
    procedure Repaint; override;
  end;

implementation

uses
  mainUnit,
  uEngine2D, uDemoGameLoader;

{ TShip }

constructor TShip.Create(newParent: pointer);
var
  vEngine: TEngine2D;
begin
  inherited;

  vEngine := newParent;

//  Self.Shape.AddFigure(TFigure)

  FLeftFire := TShipFire.Create(newParent);
  FLeftFire.Parent := newParent;
  FLeftFire.Resources := vEngine.Resources;
  FLeftFire.ScaleX := Self.ScaleX;
  FLeftFire.ScaleY := Self.ScaleY;
  FLeftFire.Opacity := 0.5;
  vEngine.AddObject(FLeftFire);

  FRightFire := TShipFire.Create(newParent);
  FRightFire.Parent := newParent;
  FRightFire.Resources := vEngine.Resources;
  FRightFire.ScaleX := -Self.ScaleX;
  FRightFire.ScaleY := Self.ScaleY;
  FRightFire.Opacity := 0.5;

  vEngine.AddObject(FRightFire);

  Self.OnMouseDown := Self.MouseDown;
end;

procedure TShip.MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
var
  vEngine: TEngine2D;
  vPos: TPosition;
  vFi, vEX, vEY, vDist: Double; // Координаты
  vAni: TAnimation;
begin
  vEngine := Parent;

  vEX := Self.x - X;
  vEY := Self.y - y;

  vDist := Sqrt(Sqr(vEX) + Sqr(vEY));
  vFi := ArcTan2(-vEY, -vEX);
  if vDist > 25 then
    vPos.Rotate := (Self.Rotate + ((vFi / Pi) * 180+270)) / 2
  else
    vPos.rotate := Self.Rotate;
  vPos.x := self.x + vEX * vDist * 0.05;// Self.x + Abs(vEX) * vDist * 0.1 * Self.ScaleX * Sin(vPos.rotate / 180 * pi);
  vPos.y := self.y + vEY * vDist * 0.05;//Self.y + Abs(vEY) * vDist * 0.1 * Self.ScaleY * Cos(vPos.rotate / 180 * pi);

  vPos.ScaleX := Self.ScaleX;
  vPos.ScaleY := Self.ScaleY;
  Self.Opacity := 1;

 // self.Rotate := 270;


  mainForm.caption := FloatToStr(vEX) + '~~' + FloatToStr(vEY) + '~~' + FloatToStr(vPos.Rotate);

  vAni := TLoader.ShipFlyAnimation(Self, vPos);
  vAni.Parent := vEngine;
  vEngine.AnimationList.Add(vAni);

end;

procedure TShip.Repaint;
begin
  curRes := 0;

  inherited;

  Self.Rotate := Self.Rotate + (random - 0.5) * 0.5;
  Self.X := Self.x + (random - 0.5) * 0.5;
  Self.Y := Self.y + (random - 0.5) * 0.5;

  FLeftFire.Rotate := Self.Rotate;
  FLeftFire.x := Self.x + (scW*0.15 - 0) * cos((Self.Rotate / 180) * pi) - (scH*0.7 - 0) * sin((Self.Rotate / 180) * pi);
  FLeftFire.y :=  Self.y + (scW*0.15 - 0) * sin((Self.Rotate / 180) * pi) + (scH*0.7 - 0) * cos((Self.Rotate / 180) * pi);

  FRightFire.Rotate := Self.Rotate;
  FRightFire.x := Self.x + (-scW*0.15 - 0) * cos((Self.Rotate / 180) * pi) - (scH*0.7 - 0) * sin((Self.Rotate / 180) * pi);
  FRightFire.y :=  Self.y + (-scW*0.15 - 0) * sin((Self.Rotate / 180) * pi) + (scH*0.7 - 0) * cos((Self.Rotate / 180) * pi);

end;

{ TAsteroid }

procedure TAsteroid.Collide(const AObject: TEngine2DObject);
var
  vArcTan: Extended;
  vDX: Single;
begin
  if FNotChange > 0 then
  begin
    FNotChange := 10;
    Exit;     
  end;
  vArcTan := ArcTan2(AObject.y - Self.y, AObject.x - Self.x);
  vDX := FDx;
{  FDX := Cos(vArcTan + Pi) * (FDX) - Sin(vArcTan + Pi) * (FDY);
  FDY := Sin(vArcTan + Pi) * (vDX) + Cos(vArcTan + Pi) * (FDY);}
  FDx := - FDx;
  FDy := - FDy;  
  AObject.x := AObject.x - FDx * Game.Speed * 2;
  AObject.y := AObject.y - FDy * Game.Speed * 2;  
  AObject.Rotate := AObject.Rotate + (vArcTan / pi180) * 0.01;
  FNotChange := 10;
end;

constructor TAsteroid.Create(newParent: pointer);
begin
  inherited;
  FDx := 200 * Random;
  FDy := 200 * Random;
  FDA := 180 * Random - 90;
  FNotChange := 0;
end;

procedure TAsteroid.Repaint;
begin
  curRes := 1;

  Self.x := Self.x + FDx * Game.Speed;
  Self.y := Self.y + FDy * Game.Speed;
  Self.Rotate := Self.Rotate + FDa * Game.Speed;

  if Self.Rotate >= 360 then
    Self.Rotate := 0;

  if Self.x > tEngine2d(Parent).Width + Self.scW then
    Self.x := -Self.scW
  else
    if Self.x < 0 - Self.scW then
      Self.x :=  tEngine2d(Parent).Width + Self.scW;

  if Self.y > tEngine2d(Parent).Height + Self.scH then
    Self.y := -Self.scH
  else
  if Self.y < 0 - Self.scH then
    Self.y := tEngine2d(Parent).Height + Self.scH;

  if FNotChange > 0 then
    Dec(FNotChange);

  inherited;
end;

{ TLittleAsteroid }

constructor TLittleAsteroid.Create(newParent: pointer);
begin
  inherited;
  FTip := Random(6);
  if FTip > 3 then
    FTip := 3; // Чтобы звезд побольше было

  FDX := Random * 350;
  FDY := Random * 350 ;
end;

procedure TLittleAsteroid.Repaint;
begin
  curRes := FTip + 5;
  Self.x := Self.x + FDx * Game.Speed;
  Self.y := Self.y + FDy * Game.Speed;

  if Self.x > tEngine2d(Parent).Width then
    Self.x := -1;

  if Self.y > tEngine2d(Parent).Height then
    Self.y := -1;
  inherited;

end;

{ TStar }

procedure TStar.Repaint;
begin
  inherited;

end;

{ TShipFire }

constructor TShipFire.Create(newParent: pointer);
begin
  inherited;
  FTip := Random(3);
end;

procedure TShipFire.Repaint;
begin
  inherited;
  FTip := Random(3);
  curRes := FTip + 2;
end;

end.
