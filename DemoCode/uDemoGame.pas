// Демонстрация Shadow Engine 2D. Дмитрий Сорокин.

unit uDemoGame;

interface

uses
  FMX.Types, System.UITypes, System.Classes, System.Types,
  FMX.Objects, System.Generics.Collections, System.Math,
  uEasyDevice, uDemoEngine, uDemoGameLoader, uDemoObjects, uEngine2DObjectShape,
  uEngine2DAnimation, uIntersectorClasses, uDemoMenu;

type
  TDemoGame = class
  private
    FEngine: TDemoEngine;
    FBackObjects: TList<TLittleAsteroid>; // Летящие бэки
    FShip: TShip;
    FMenu: TGameMenu;
    FAsteroids: TList<TAsteroid>;
    function GetImage: TImage;
    procedure SetImage(const Value: TImage);
    procedure MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; x, y: single);
    procedure MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; x, y: single);
    procedure FindCollide;
    function GetSpeed: Single;
    procedure StartGame(ASender: TObject);
    procedure StatGame(ASender: TObject);
    procedure AboutGame(ASender: TObject);
    procedure ExitGame(ASender: TObject);
  public
    property Image: TImage read GetImage write SetImage;
    property Speed: Single read GetSpeed;
    procedure Prepare;
    procedure Resize(ASize: TPointF);
    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses
  FMX.Dialogs, uEngine2DSprite;

{ TDemoGame }

procedure TDemoGame.AboutGame(ASender: TObject);
begin

end;

constructor TDemoGame.Create;
begin
  FEngine := TDemoEngine.Create;
end;

destructor TDemoGame.Destroy;
var
  i, vN: Integer;
begin
  FMenu.Free;

  vN := FAsteroids.Count - 1;
  for i := 0 to vN do
    FAsteroids[i].Free;
  FAsteroids.Free;

  vN := FBackObjects.Count - 1;
  for i := 0 to vN do
    FBackObjects[i].Free;
  FBackObjects.Free;

  FShip.Free;

  inherited;
end;

procedure TDemoGame.ExitGame(ASender: TObject);
begin
  Halt;
end;

procedure TDemoGame.FindCollide;
var
  i, j, vN: Integer;
begin
  vN := FAsteroids.Count - 1;
  if Assigned(FShip) then
    if FShip.Visible then
    begin
      for i := 0 to vN do
        if FShip.Shape.IsIntersectWith(FAsteroids[i].Shape) then
          FAsteroids[i].Collide(FShip);
    end;

  for i := 0 to vN do
    for j := i + 1 to vN do
      if FAsteroids[i].Shape.IsIntersectWith(FAsteroids[j].Shape) then
      begin
        FAsteroids[i].Collide(FAsteroids[j]);
        FAsteroids[j].Collide(FAsteroids[i]);
      end;
end;

function TDemoGame.GetImage: TImage;
begin
  Result := FEngine.Image;
end;

function TDemoGame.GetSpeed: Single;
begin
  Result := FEngine.EngineThread.Speed;
end;

procedure TDemoGame.MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; x, y: single);
var
  i, vL: Integer;
  vAni: TAnimation;
  vPos: TPosition;
begin
  fEngine.MouseDown(Sender, Button, Shift, x, y);

  vL := Length(fEngine.Downed) - 1;

  for i := 0 to vL do
    fEngine.Sprites[fEngine.Downed[i]].OnMouseDown(fEngine.Sprites[fEngine.Downed[i]], Button, Shift, x, y);

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

procedure TDemoGame.MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; x, y: single);
var
  vL, i: Integer;
begin
  fEngine.MouseUp(Sender, Button, Shift, x, y);
  vL := Length(FEngine.Clicked) - 1;
  if vL >= 0 then
    for i := 0 to 0{vL} do
      fEngine.sprites[fEngine.Clicked[i]].OnClick(fEngine.Sprites[fEngine.Clicked[i]]);
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
  for i := 0 to 69 do
    FBackObjects.Add(vLoader.RandomAstroid);

  // Создаем корабль
  FShip := vLoader.CreateShip;

  FAsteroids := TList<TAsteroid>.Create;
  for i := 0 to 19 do
    FAsteroids.Add(vLoader.BigAstroid);

  FMenu := TGameMenu.Create(FEngine);
  FMenu.StartGame := StartGame;
  FMenu.AboutGame := AboutGame;
  FMenu.StatGame := StatGame;
  FMenu.ExitGame := ExitGame;
  FEngine.HideGroup('ship');

  FEngine.InBeginPaintBehavior := FindCollide;
  FEngine.Start;
end;

procedure TDemoGame.Resize(ASize: TPointF);
begin
{  FEngine.Width := Round(ASize.X);
  FEngine.Height := Round(ASize.Y);  }
  FEngine.DoTheFullWindowResize;
end;

procedure TDemoGame.SetImage(const Value: TImage);
begin
  fEngine.init(Value);
  Value.OnMouseDown := Self.MouseDown;
  Value.OnMouseUp := Self.MouseUp;

end;

procedure TDemoGame.StartGame(ASender: TObject);
var
  vSpr: TSprite;
  vLoader: TLoader;
begin
  FEngine.HideGroup('menu');
  FEngine.ShowGroup('ship');
  vLoader := TLoader.Create(FEngine);

  for vSpr in FShip.Parts do
  begin
    vSpr.Opacity := 0;
    FEngine.AnimationList.Add(
      vLoader.OpacityAnimation(vSpr, 1)
    );
  end;

  {FEngine.AnimationList.Add(
      vLoader.ScaleAnimation(vSpr, 1.6)
    );}

end;

procedure TDemoGame.StatGame(ASender: TObject);
begin

end;

end.


