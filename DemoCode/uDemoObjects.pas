unit uDemoObjects;

interface

uses
  FMX.Types, System.UITypes, System.Classes, System.Types, System.SysUtils, System.Math,
  System.Generics.Collections,
  uEngine2DSprite, uEngine2DText, uEngine2DAnimation, uEngine2DStandardAnimations, uEngine2DClasses,
  uEngine2DObject, uIntersectorClasses, uClasses;

type
  TShipFire = class(TSprite)
  private
    FTip: Byte;
  public
    property Tip: Byte read FTip write FTip; // Тип огня
    procedure Repaint; override;
    constructor Create(AParent: pointer); override;
  end;

  TShipLight = class(TSprite)
  public
    procedure Repaint; override;
  end;

  TMovingUnit = class(TSprite)
  protected
    FDx, FDy, FDA: Double; // Сдвиги
    FMaxDx, FMaxDy, FMaxDa: Single;
    FMonitorScale: Single;
    FSpeedModScale: Single;
    procedure SetScale(AValue: single); override;
  public
    property DX: Double read FDx write FDx;
    property DY: Double read FDy write FDy;
    property DA: Double read FDa write FDa;
    procedure SetMonitorScale(const AValue: Single);
    procedure SetSpeedModScale(const AValue: Single);
  end;

  TShip = class(TMovingUnit)
  private
    FParts: TList<TSprite>;// array[0..5] of TSprite;
    FLeftFire: TShipFire;
    FRightFire: TShipFire;
    FLeftFireCenter: TShipFire;
    FRightFireCenter: TShipFire;
    FShipLight: TShipLight;
    FDestination: TPosition;
    FDestinations: TList<TPosition>;
    procedure MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    function MinMax(const AValue: Single; const AMax: Single; const AMin: Single = 0): Single;
  protected
    procedure SetScale(AValue: single); override;
  public
    property Parts: TList<TSprite> read FParts;
    procedure SetOpacity(const AOpacity: Integer);
    property Destination: TPosition read FDestination write FDestination;
    procedure Repaint; override;
    procedure AddDestination(const APos: TPosition);
    constructor Create(AParent: pointer); override;
    destructor Destroy; override;
  end;

  TAsteroid = class(TMovingUnit)
  private
    FNotChange: Integer; // Кол-во тиков, которое не будет изменяться направление при коллайдер
  public
    procedure Repaint; override;
    function Collide(const AObject: TEngine2DObject): Boolean;
    constructor Create(AParent: pointer); override;
  end;

  TLittleAsteroid = class(TMovingUnit)
  private
    FTip: Byte;
  public
    property Tip: Byte read FTip write FTip; // Тип астеройда
    procedure Repaint; override;
    constructor Create(AParent: pointer); override;
  end;

  TExplosion = class(TSprite)
  end;

  TStar = class(TSprite)
  public
    procedure Repaint; override;
  end;

implementation

uses
  mainUnit,
  uEngine2D, uDemoGameLoader, uIntersectorMethods;

{ TShip }

procedure TShip.AddDestination(const APos: TPosition);
begin
  FDestinations.Add(APos);
end;

constructor TShip.Create(AParent: pointer);
var
  vEngine: TEngine2D;
begin
  inherited;

  vEngine := AParent;

  FLeftFire := TShipFire.Create(AParent);
  FLeftFire.Resources := vEngine.Resources;
  FLeftFire.Group := 'ship';
  FLeftFire.Justify := BottomCenter;
  FLeftFire.ScaleX := Self.ScaleX;
  FLeftFire.ScaleY := Self.ScaleY;
  FLeftFire.Opacity := 0.5;
  vEngine.AddObject(FLeftFire);

  FRightFire := TShipFire.Create(AParent);
  FRightFire.Resources := vEngine.Resources;
  FRightFire.Group := 'ship';
  FRightFire.Justify := BottomCenter;
  FRightFire.ScaleX := -Self.ScaleX;
  FRightFire.ScaleY := Self.ScaleY;
  FRightFire.Opacity := 0.5;
  vEngine.AddObject(FRightFire);

  FLeftFireCenter := TShipFire.Create(AParent);
  FLeftFireCenter.Resources := vEngine.Resources;
  FLeftFireCenter.Group := 'ship';
  FLeftFireCenter.Justify := BottomCenter;
  FLeftFireCenter.ScaleX := Self.ScaleX;
  FLeftFireCenter.ScaleY := Self.ScaleY;
  FLeftFireCenter.Opacity := 0.5;
  vEngine.AddObject(FLeftFireCenter);

  FRightFireCenter := TShipFire.Create(AParent);
  FRightFireCenter.Resources := vEngine.Resources;
  FRightFireCenter.ScaleX := -Self.ScaleX;
  FRightFireCenter.Group := 'ship';
  FRightFireCenter.Justify := BottomCenter;
  FRightFireCenter.ScaleY := Self.ScaleY;
  FRightFireCenter.Opacity := 0.5;
  vEngine.AddObject(FRightFireCenter);

  FShipLight := TShipLight.Create(AParent);
  FShipLight.Resources := vEngine.Resources;
  FShipLight.Group := 'ship';
  FShipLight.ScaleX := Self.ScaleX;
  FShipLight.ScaleY := Self.ScaleY;
  vEngine.AddObject(FShipLight);

  Self.OnMouseDown := Self.MouseDown;

  FParts := TList<TSprite>.Create;
  FParts.Add(Self);
  FParts.Add(FLeftFire);
  FParts.Add(FRightFire);
  FParts.Add(FLeftFireCenter);
  FParts.Add(FRightFireCenter);
  FParts.Add(FShipLight);

  DA := 16;
  Dx := 20;
  Dy := 20;

  FMaxDx := 20;
  FMaxDy := 20;

  FDestinations := TList<TPosition>.Create;
end;

destructor TShip.Destroy;
var
  i: Integer;
begin
  // Нулевой объект это сам корабль
  for i := 1 to FParts.Count - 1 do
    FParts[i].Free;

  FParts.Free;

  FDestinations.Free;

  inherited;
end;

function TShip.MinMax(const AValue, AMax, AMin: Single): Single;
begin
  Result := AValue;
  if AValue > AMax then
    Result := AMax;

  if AValue < 0 then
    Result := 0;
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
var
  vAngle: Single;
  vDir: Single;
  vKoef, vAnimKoef, vLeftKoef, vRightKoef, vAnimLeftKoef, vAnimRightKoef: Single;
  vNewX, vNewY: Single;
  vEngine: tEngine2d;
  vDA: Single;
begin
  vEngine := Parent;
  curRes := 0;

  inherited;

  vLeftKoef := 1;
  vRightKoef := 1;
  vAnimLeftKoef := vLeftKoef;
  vAnimRightKoef := vRightKoef;

 { if FDestinations.Count > 0 then
    FDestination := FDestinations[0]; }

  vKoef := Distance(Self.Center, FDestination.XY) / Self.w;
  if vKoef > 1 then
    vKoef := 1;

  if vKoef < 0.225 * SpeedModScale then
  begin
    vKoef := 0.225 * SpeedModScale;
  end;

  vAnimKoef := Min(1, vKoef * 2);

  if vKoef > 0.3 * (SpeedModScale) then
  begin
    vAngle := Self.Rotate;
    NormalizeAngle(vAngle);
    Self.Rotate := vAngle;

    vAngle := ArcTan2(FDestination.Y - Self.Y, FDestination.X - Self.x) / pi180;
    vDir := (vAngle - Self.Rotate);

    NormalizeAngle(vDir);

    if (vDir < -90) or (vDir > 90) then
    begin

      Self.Rotate := Self.Rotate - DA * vKoef * vEngine.EngineThread.Speed * FSpeedModScale;
      vLeftKoef := 1;
      vRightKoef := 0.4;
    end
    else begin
      Self.Rotate := Self.Rotate + DA * vKoef * vEngine.EngineThread.Speed * FSpeedModScale;
      vLeftKoef := 0.4;
      vRightKoef := 1;
    end;

    if ((Abs(vDir) > 165) and (Abs(vDir) < 180))  then
    begin
      vLeftKoef := 1;
      vRightKoef := 1;
    end;

    if ((Abs(vDir) > 0) and (Abs(vDir) < 15)) then
    begin
      vLeftKoef := 1;// (vAngle - Self.Rotate) / 180;
      vRightKoef := 1;// (S
    end;

    vAnimLeftKoef := Min(1, vLeftKoef * 2);
    vAnimRightKoef := Min(1, vRightKoef * 2);

    vNewX := Self.x - (Max(DX, 0.2) * Cos(Self.Rotate * pi180) * vKoef - DY * Sin(Self.Rotate * pi180) * vKoef);

    if Distance(FDestination.XY, PointF(vNewX, Self.y)) <
       Distance(FDestination.XY, Self.Center)
     then
       Self.x := Self.x - (DX * Cos(Self.Rotate * pi180) * vKoef - DY * Sin(Self.Rotate * pi180) * vKoef) * vEngine.EngineThread.Speed * FSpeedModScale;

    vNewY := Self.Y - (DX * Sin(Self.Rotate *  pi180) * vKoef + Max(DY, 0.2) * Cos(Self.Rotate * pi180) * vKoef);
    if Distance(FDestination.XY, PointF(Self.x, vNewY)) <
       Distance(FDestination.XY, Self.Center)
    then
      Self.Y := Self.Y - (DX * Sin(Self.Rotate *  pi180) * vKoef + DY * Cos(Self.Rotate * pi180) * vKoef) * vEngine.EngineThread.Speed * FSpeedModScale;
  end else
  begin
  //  FDestinations.Delete(0);
  end;

{  Self.x := Self.x - (DX * 0.2 * Cos(Self.Rotate * pi180) * vKoef - DY * 0.2 * Sin(Self.Rotate * pi180) * vKoef) * vEngine.EngineThread.Speed;
  Self.Y := Self.Y - (DX * 0.2 * Sin(Self.Rotate *  pi180) * vKoef + DY * 0.2 * Cos(Self.Rotate * pi180) * vKoef) * vEngine.EngineThread.Speed;}

//  Self.Rotate := Self.Rotate + (random - 0.5) * 0.5;
 // Self.X := Self.x + (random - 0.5) * 0.5;
  //Self.Y := Self.y + (random - 0.5) * 0.5;

  FLeftFire.Rotate := Self.Rotate;
  FLeftFire.ScalePoint := Self.ScalePoint * 2 * vKoef * vLeftKoef;
  FLeftFire.x := Self.x + (scW*0.15 - 0) * cos((Self.Rotate / 180) * pi) - (scH*0.4 - 0) * sin((Self.Rotate / 180) * pi);
  FLeftFire.y :=  Self.y + (scW*0.15 - 0) * sin((Self.Rotate / 180) * pi)+ (scH*0.4 - 0) * cos((Self.Rotate / 180) * pi);

  FRightFire.Rotate := Self.Rotate;
  FRightFire.ScalePoint := Self.ScalePoint * PointF(-2, 2) * vKoef * vRightKoef;
  FRightFire.x := Self.x + (-scW*0.15 - 0) * cos((Self.Rotate / 180) * pi) - (scH*0.4 - 0) * sin((Self.Rotate / 180) * pi);
  FRightFire.y :=  Self.y + (-scW*0.15 - 0) * sin((Self.Rotate / 180) * pi) + (scH*0.4 - 0) * cos((Self.Rotate / 180) * pi);

  FLeftFireCenter.Rotate := Self.Rotate;
  FLeftFireCenter.ScalePoint := Self.ScalePoint * 2 * vKoef;
  FLeftFireCenter.x := Self.x + (scW*0.0 - 0) * cos((Self.Rotate / 180) * pi) - (scH*0.3 - 0) * sin((Self.Rotate / 180) * pi);
  FLeftFireCenter.y :=  Self.y + (scW*0.0 - 0) * sin((Self.Rotate / 180) * pi) + (scH*0.3 - 0) * cos((Self.Rotate / 180) * pi);

  FRightFireCenter.Rotate := Self.Rotate;
  FRightFireCenter.ScalePoint := Self.ScalePoint * PointF(-2, 2) * vKoef;
  FRightFireCenter.x := Self.x + (-scW*0.0 - 0) * cos((Self.Rotate / 180) * pi) - (scH*0.3 - 0) * sin((Self.Rotate / 180) * pi);
  FRightFireCenter.y :=  Self.y + (-scW*0.0 - 0) * sin((Self.Rotate / 180) * pi) + (scH*0.3 - 0) * cos((Self.Rotate / 180) * pi);

  FShipLight.ScalePoint := Self.ScalePoint;
  FShipLight.Rotate := Self.Rotate;
  FShipLight.x := Self.x + (-scW*0.0 - 0) * cos((Self.Rotate / 180) * pi) - (scH * 0.4) * sin((Self.Rotate / 180) * pi);
  FShipLight.y :=  Self.y + (-scW*0.0 - 0) * sin((Self.Rotate / 180) * pi) + (scH * 0.4) * cos((Self.Rotate / 180) * pi);
  FShipLight.SendToFront;
 end;

procedure TShip.SetOpacity(const AOpacity: Integer);
var
  i: Integer;
begin
  Self.Opacity := AOpacity;
  for i := 0 to FParts.Count - 1 do
    FParts[i].Opacity := AOpacity;
end;

procedure TShip.SetScale(AValue: single);
var
  i: Integer;
begin
  inherited;
  for i := 1 to FParts.Count - 1 do
    FParts[i].Scale := AValue;
end;

{ TAsteroid }

function TAsteroid.Collide(const AObject: TEngine2DObject): Boolean;
var
  vArcTan, vArcTan2: Extended;
  vDX: Single;
  vLoader: TLoader;
  vAng: Double;
  vAni: TAnimation;
  vExp: TExplosion;
begin
  if FNotChange > 0 then
  begin
    FNotChange := 2;
    Exit(False);
  end;

  vArcTan := ArcTan2(AObject.y - Self.y, AObject.x - Self.x);
  vArcTan2 := ArcTan2(-FDy, FDx) + vArcTan;

  vDX := FDx;
  FDX := Cos(vArcTan2) * (FDX) - Sin(vArcTan2) * (FDY);
  FDY := Sin(vArcTan2) * (vDX) + Cos(vArcTan2) * (FDY);
  FDx := - FDx;
  FDy := - FDy;
  AObject.x := AObject.x - FDx * Game.Speed * 2 * FSpeedModScale;
  AObject.y := AObject.y - FDy * Game.Speed * 2 * FSpeedModScale;

  vLoader := TLoader.Create(FParent);
  vAng := vArcTan / pi180;

  vExp := vLoader.Explosion(
    Self.x + (Self.Shape.MaxRadius * 1) * Cos(vArcTan),
    Self.y + (Self.Shape.MaxRadius * 1) * Sin(vArcTan),
    vAng);

  vAni := vLoader.ExplosionAnimation(vExp);

  AObject.Rotate := AObject.Rotate + (vArcTan / pi180) * 0.02;
  FNotChange := 10;

  vAni.Parent := fParent;
  tEngine2d(fParent).AnimationList.Add(vAni);
  Result := True;
end;

constructor TAsteroid.Create(AParent: pointer);
begin
  inherited;

  FMaxDx := 10;
  FMaxDy := 10;
  FMaxDa := 6;

  FDx := 3 * Random;
  FDy := 3 * Random;
  FDA := 10 * Random - 5;
  FNotChange := 0;
end;

procedure TAsteroid.Repaint;
begin
  curRes := 1;

  inherited;

  Self.x := Self.x + FDx * Game.Speed * FSpeedModScale;
  Self.y := Self.y + FDy * Game.Speed * FSpeedModScale;
  Self.Rotate := Self.Rotate + FDa * Game.Speed * FSpeedModScale;

  if Self.Rotate >= 360 then
    Self.Rotate := 0;

  if Self.x >= tEngine2d(Parent).Width + Self.scW then
    Self.x := -Self.scW
  else
    if Self.x < 0 - Self.scW then
      Self.x := tEngine2d(Parent).Width;

  if Self.y >= tEngine2d(Parent).Height + Self.scH then
    Self.y := -Self.scH
  else
    if Self.y < 0 - Self.scH then
     Self.y := tEngine2d(Parent).Height;

  if FNotChange > 0 then
    Dec(FNotChange);
end;

{ TLittleAsteroid }

constructor TLittleAsteroid.Create(AParent: pointer);
begin
  inherited;
  FTip := Random(6);
  if FTip > 3 then
    FTip := 3; // Чтобы звезд побольше было

  FMaxDx := 20;
  FMaxDy := 20;
  FMaxDa := 10;

  FDX := Random * 12;
  FDY := Random * 12 ;
  FDA := 60 * Random - 30;
end;

procedure TLittleAsteroid.Repaint;
begin
  curRes := FTip + 5;
  inherited;

  Self.Rotate := Self.Rotate + FDa * Game.Speed * FSpeedModScale;

  if Self.Rotate >= 360 then
    Self.Rotate := 0;

  Self.x := Self.x + FDx * Game.Speed * FSpeedModScale;
  Self.y := Self.y + FDy * Game.Speed * FSpeedModScale;

  if Self.x > tEngine2d(Parent).Width then
    Self.x := -1;

  if Self.y > tEngine2d(Parent).Height then
    Self.y := -1;
end;

{ TStar }

procedure TStar.Repaint;
begin
  inherited;

end;

{ TShipFire }

constructor TShipFire.Create(AParent: pointer);
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

{ TShipLight }

procedure TShipLight.Repaint;
begin
  if Opacity > 0.1 then
    Opacity := 0.8 + Random * 0.2;
  CurRes := 13 + Random(2);
  inherited;
end;

{ TMovingUnit }

procedure TMovingUnit.SetMonitorScale(const AValue: Single);
begin
  FMonitorScale := AValue;
end;

procedure TMovingUnit.SetScale(AValue: single);
begin
  inherited;
 { DX := (DX / abs(DX)) * FMaxDx * AValue;
  DY := (DY / abs(DY)) * FMaxDy * AValue;
  DA := (DA / abs(DA)) * FMaxDa * AValue;  }
end;

procedure TMovingUnit.SetSpeedModScale(const AValue: Single);
begin
  FSpeedModScale := AValue;
end;

end.


