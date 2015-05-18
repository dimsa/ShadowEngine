unit uDemoObjects;

interface

uses
  uEngine2DSprite;

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
  public
    property LeftFire: TShipFire read FLeftFire write FLeftFire;
    property RightFire: TShipFire read FRightFire write FRightFire;
    procedure Repaint; override;
    constructor Create(newParent: pointer); override;
  end;

  TAsteroid = class(TSprite)
  public
    procedure Repaint; override;
  end;

  TLittleAsteroid = class(TSprite)
  private
    FTip: Byte;
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
  uEngine2D;

{ TShip }

constructor TShip.Create(newParent: pointer);
var
  vEngine: TEngine2D;
begin
  inherited;

  vEngine := newParent;

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
end;

procedure TShip.Repaint;
begin
  curRes := 0;

  inherited;

  Self.Rotate := Self.Rotate + random - 0.5;
  Self.X := Self.x + random - 0.5;
  Self.Y := Self.y + random - 0.5;

  FLeftFire.Rotate := Self.Rotate;
  FLeftFire.x := Self.x + (scW*0.15 - 0) * cos((Self.Rotate / 180) * pi) - (scH*0.7 - 0) * sin((Self.Rotate / 180) * pi);
  FLeftFire.y :=  Self.y + (scW*0.15 - 0) * sin((Self.Rotate / 180) * pi) + (scH*0.7 - 0) * cos((Self.Rotate / 180) * pi);

  FRightFire.Rotate := Self.Rotate;
  FRightFire.x := Self.x + (-scW*0.15 - 0) * cos((Self.Rotate / 180) * pi) - (scH*0.7 - 0) * sin((Self.Rotate / 180) * pi);
  FRightFire.y :=  Self.y + (-scW*0.15 - 0) * sin((Self.Rotate / 180) * pi) + (scH*0.7 - 0) * cos((Self.Rotate / 180) * pi);

end;

{ TAsteroid }

procedure TAsteroid.Repaint;
begin
  curRes := 3;
  inherited;

end;

{ TLittleAsteroid }

constructor TLittleAsteroid.Create(newParent: pointer);
begin
  inherited;
  FTip := Random(6);
  if FTip > 3 then
    FTip := 3; // Чтобы звезд побольше было
end;

procedure TLittleAsteroid.Repaint;
begin
  curRes := FTip + 5;
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
