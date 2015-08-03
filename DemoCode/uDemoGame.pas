// Демонстрация Shadow Engine 2D. Дмитрий Сорокин.

unit uDemoGame;

interface

uses
  FMX.Types, System.UITypes, System.Classes, System.Types,
  FMX.Objects, System.Generics.Collections, System.Math,
  uEasyDevice, uDemoEngine, uDemoGameLoader, uDemoObjects, uEngine2DObjectShape,
  uEngine2DAnimation, uIntersectorClasses;

type
  TDemoGame = class
  private
    FEngine: TDemoEngine;
    FBackObjects: TList<TLittleAsteroid>; // Летящие бэки
    FShip: TShip;
    FAsteroids: TList<TAsteroid>;
    function GetImage: TImage;
    procedure SetImage(const Value: TImage);
    procedure mouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; x, y: single);
    procedure FindCollide;
    function GetSpeed: Single;
  public
    property Image: TImage read GetImage write SetImage;
    property Speed: Single read GetSpeed;
    procedure Prepare;
    constructor Create;
  end;

implementation

{ TDemoGame }

constructor TDemoGame.Create;
begin
  FEngine := TDemoEngine.Create;
end;

procedure TDemoGame.FindCollide;
var
  i, vN: Integer;
begin
  vN := FAsteroids.Count - 1;
  for i := 0 to vN do
    if FShip.Shape.IsIntersectWith(FAsteroids[i].Shape) then
      FAsteroids[i].Collide(FShip);
end;

function TDemoGame.GetImage: TImage;
begin
  Result := FEngine.Image;
end;

function TDemoGame.GetSpeed: Single;
begin
  Result := FEngine.EngineThread.Speed;
end;

procedure TDemoGame.mouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; x, y: single);
var
  i, vL: Integer;
  vAni: TAnimation;
  vPos: TPosition;
begin
  fEngine.mouseDown(Sender, Button, Shift, x, y);

  vL := Length(fEngine.Downed) - 1;

  for i := 0 to vL do
    fEngine.sprites[fEngine.Downed[i]].OnMouseDown(Sender, Button, Shift, x, y);

  vPos.X := X;
  vPos.Y := Y;
  vPos.Rotate := (ArcTan2(y - FShip.y, x - FShip.x ) / Pi) * 180 + 90;
  if vPos.Rotate > 360 then
    vPos.Rotate := vPos.Rotate - 360;


  vPos.ScaleX := FShip.ScaleX;
  vPos.ScaleY := FShip.ScaleY;
  vAni := TLoader.ShipFlyAnimation(FShip, vPos);
  vAni.Parent := FEngine;
  FEngine.AnimationList.Add(vAni);
end;

procedure TDemoGame.Prepare;
var
  vLoader: TLoader;
  i: Integer;
begin
  FEngine.Resources.addResFromLoadFileRes('images.load');
  FEngine.Background.LoadFromFile(UniPath('back.jpg'));

  FBackObjects := TList<TLittleAsteroid>.Create;
  vLoader := TLoader.Create(FEngine);
  // Создаем астеройдное поле
  for i := 0 to 49 do
    FBackObjects.Add(vLoader.RandomAstroid);

  // Создаем корабль
  FShip := vLoader.CreateShip;

  FAsteroids := TList<TAsteroid>.Create;
  for i := 0 to 19 do
    FAsteroids.Add(vLoader.BigAstroid);

  FEngine.InBeginPaintBehavior := FindCollide;
  FEngine.Start;
end;

procedure TDemoGame.SetImage(const Value: TImage);
begin
  fEngine.init(Value);
  Value.OnMouseDown := Self.MouseDown;
  Value.OnMouseUp := fEngine.MouseUp;
end;

end.

