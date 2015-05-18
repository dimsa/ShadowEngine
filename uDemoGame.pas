// Демонстрация Shadow Engine 2D. Дмитрий Сорокин.

unit uDemoGame;

interface

uses
  FMX.Objects, System.Generics.Collections,
  uEasyDevice, uDemoEngine, uDemoGameLoader, uDemoObjects;

type
  TDemoGame = class
  private
    FEngine: TDemoEngine;
    FBackObjects: TList<TLittleAsteroid>; // Летящие бэки
    FShip: TShip;
    FAsteroid: TList<TAsteroid>;
    function GetImage: TImage;
    procedure SetImage(const Value: TImage);
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

function TDemoGame.GetImage: TImage;
begin
  Result := FEngine.Image;
end;

procedure TDemoGame.Prepare;
var
  vLoader: TLoader;
  i, vN: Integer;
begin
  FEngine.Resources.addResFromLoadFileRes('images.load');
  FEngine.Background.LoadFromFile(UniPath('back.png'));

  FBackObjects := TList<TLittleAsteroid>.Create;
  vLoader := TLoader.Create(FEngine);
  // Создаем астеройдное поле
  for i := 0 to 47 do
    FBackObjects.Add(vLoader.RandomAstroid);

  // Создаем корабль
  FShip := vLoader.CreateShip;

  fEngine.Start;
end;

procedure TDemoGame.SetImage(const Value: TImage);
begin
  fEngine.init(Value);
  Value.OnMouseDown := fEngine.MouseDown;
  Value.OnMouseUp := fEngine.MouseUp;
end;

end.
