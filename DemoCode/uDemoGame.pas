// Демонстрация Shadow Engine 2D. Дмитрий Сорокин.

unit uDemoGame;

interface

uses
  FMX.Types, System.UITypes, System.Classes, System.Types,
  FMX.Objects, System.Generics.Collections,
  uEasyDevice, uDemoEngine, uDemoGameLoader, uDemoObjects, uEngine2DObjectShape;

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
  public
    property Image: TImage read GetImage write SetImage;
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
  vShipFigure: TObjectShape;
  i, vN: Integer;
begin
//  vShipFigure := FShip.Shape.
Exit;
  vN := FAsteroids.Count - 1;
  for i := 0 to vN do
    if FShip.Shape.IsIntersectWith(FAsteroids[i].Shape) then
    begin
      FAsteroids[i].DX := - FAsteroids[i].DX;
      FAsteroids[i].DY := - FAsteroids[i].DY;
    end;

end;

function TDemoGame.GetImage: TImage;
begin
  Result := FEngine.Image;
end;

procedure TDemoGame.mouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; x, y: single);
var
  i, vL: Integer;
begin
  { Логика выбора карт }
  fEngine.mouseDown(Sender, Button, Shift, x, y);

  vL := Length(fEngine.Downed) - 1;

  for i := 0 to vL do
    fEngine.sprites[fEngine.Downed[i]].OnMouseDown(Sender, Button, Shift, x, y);
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
