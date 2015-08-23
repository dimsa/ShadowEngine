// Демонстрация Shadow Engine 2D. Дмитрий Сорокин.

unit uDemoGame;

interface

uses
  FMX.Types, System.UITypes, System.Classes, System.Types, FMX.Graphics,
  FMX.Objects, System.Generics.Collections, System.Math, System.SysUtils,
  uEasyDevice, uDemoEngine, uDemoGameLoader, uDemoObjects, uEngine2DObjectShape,
  uEngine2DAnimation, uIntersectorClasses, uDemoMenu, uClasses, uEngine2DText,
  uEngine2DSprite, uEngineFormatter, uNamedList;

type
  TGameStatus = (gsMenu1, gsMenu2, gsMenu3, gsStatics, gsAbout, gsStoryMode, gsSurvivalMode, gsRelaxMode, gsGameOver);

  TGameParam = class
  private
    FLoader: TLoader; // ССылка на Loader
    FEngine: TDemoEngine; // Ссылка на движок. Получаем из FLoader
    FGameStatus: TGameStatus;
    FBackObjects: TList<TLittleAsteroid>; // Летящие бэки
    FAsteroids: TList<TAsteroid>;
    FShip: TShip;
    FLifes: TList<TSprite>;
    FPanels: TNamedList<TEngine2DText>;

    FValueableSeconds: Double; // Секунды, которые участвуют в подсчете очков
    FScorePoints: Integer; // Очки
    FSecToNextLevel: Double; // Секунды до перехода наследующий уровень
    FCollisions: Integer; // Столкновения с начала игры
    FSeconds: Single; // Секунды с начала игры

    function GetAsteroids: Integer;
    function GetCollisions: Integer;
    function GetLevel: Integer;
    function GetScore: Integer;
    function GetTime: Double;
    procedure SetLevel(const Value: Integer);
    procedure SetScore(const Value: Integer);
  public

    // Статитические данные игры
    property Score: Integer read GetScore write SetScore;
    property Time: Double read GetTime;
    procedure AddTime(const ADeltaTime: Double); // Добавляет дельту времени
    procedure AddCollision; // Добавляет одно столкновение

    property Level: Integer read GetLevel write SetLevel;
    property Collisions: Integer read GetCollisions;
    property AstroidCount: Integer read GetAsteroids;
    property Ship: TShip read FShip;
    property Asteroids: TList<TAsteroid> read FAsteroids;
    property Lifes: TList<TSprite> read FLifes;

    procedure BreakLife;
    function AddAsteroid(ASize, ASpeed: Byte): TAsteroid;
    procedure DeleteAsteroid(const ACount: Integer = 1);
    procedure DefineAsteroidCount(const ACount: Integer);
    procedure FixScore;

    procedure SetScaling(const AMonitorScale, ASpeedModScale: Double);
    procedure RenewPanels;
    procedure RestartGame(const AGameMode: TGameStatus);
    constructor Create(ALoader: TLoader);
    destructor Destroy;
  const
    // Величина параметров, скорости, размера и т.д.
    prSmall = 1;
    prMedium = 2;
    prBig = 3;
    prRandom = 0;
  end;

  TDemoGame = class
  private
    FEngine: TDemoEngine;
    FLoader: TLoader;
    FMenu: TGameMenu;
    FGameOverText: TEngine2DText;
    FGameStatus: TGameStatus;
    FGP: TGameParam;

    function GetImage: TImage;
    procedure SetImage(const Value: TImage);
    function GetSpeed: Single; // Берется из Engine2D чтобы на всех устройствах была одна скорость

    procedure MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; x, y: single);
    procedure MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; x, y: single);
    procedure MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Single);

    procedure StartGame(ASender: TObject);
    procedure SelectMode(ASender: TObject);
    procedure SelectLevel(ASender: TObject);
    procedure StartRelax(ASender: TObject);
    procedure StartSurvival(ASender: TObject);
    procedure StatGame(ASender: TObject);
    procedure AboutGame(ASender: TObject);
    procedure ExitGame(ASender: TObject);

    procedure DoGameTick;
    procedure FindCollide;

    function DestinationFromClick(const Ax, Ay: Single): TPosition;
    procedure SetGameStatus(const Value: TGameStatus);
    function GetDrawFigures: Boolean;
    procedure SetDrawFigures(const Value: Boolean);
  public
    property GameStatus: TGameStatus read FGameStatus write SetGameStatus;
    property Image: TImage read GetImage write SetImage;
    property Speed: Single read GetSpeed;
    property DrawFigures: Boolean read GetDrawFigures write SetDrawFigures;

    procedure Prepare;
    procedure Resize;
    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses
  FMX.Dialogs, uEngine2DObject;

{ TDemoGame }

procedure TDemoGame.AboutGame(ASender: TObject);
begin
  GameStatus := gsAbout;
end;

procedure TDemoGame.DoGameTick;
var
  vS: String;
  vDSec: Integer;
  vTmp: Double;
begin
  FindCollide;
  FGP.Ship.SendToFront;
  FMenu.SendToFront;

  vTmp := 1 / FEngine.EngineThread.FPS;
  FGP.AddTime(vTmp);
//  FGP.Time := FGP.Time + vTmp;

{  case GameStatus of
    gsStoryMode: ;
    gsSurvivalMode: begin
     // Asteroids := FAsteroids.Count;
      FGP.Score := Score + vDSec * FAsteroids.Count;

    end;
    gsRelaxMode: begin
      Score := Round((FSeconds / (FCollisions + 1)) * FAsteroids.Count);
    end;

  end; }

  FGP.RenewPanels;
end;

constructor TDemoGame.Create;
var
  vS: String;
begin
  FEngine := TDemoEngine.Create;
  FLoader := TLoader.Create(FEngine);
end;

function TDemoGame.DestinationFromClick(const Ax, Ay: Single): TPosition;
var
  vAngle: Single;
begin
  Result.XY(Ax, Ay);
  vAngle := (ArcTan2(Ay - FGP.Ship.y, Ax - FGP.Ship.x ) / Pi) * 180 + 90;
  NormalizeAngle(vAngle);
  Result.Rotate := vAngle;
  Result.Scale(FGP.Ship.ScalePoint);
end;

destructor TDemoGame.Destroy;
var
  i: Integer;
begin
  FMenu.Free;
  FLoader.Free;
  FEngine.Free;
  FGP.Free;

  inherited;
end;

procedure TDemoGame.ExitGame(ASender: TObject);
begin
  StopApplication;
  //Halt;
end;

procedure TDemoGame.FindCollide;
var
  i, j, vN: Integer;
begin
  vN := FGP.Asteroids.Count - 1;
  if Assigned(FGP) then
  with FGP do
  begin
    if Ship.Visible then
    begin
      for i := 0 to vN do
        if Ship.Shape.IsIntersectWith(Asteroids[i].Shape) then
          if Asteroids[i].Collide(Ship) then
          begin
            case GameStatus of
              gsStoryMode, gsSurvivalMode: begin
                BreakLife;
                if FGP.Lifes.Count <= 0 then
                begin
                  GameStatus := gsGameOver;
                  FGP.FixScore;
                end;
              end;
              gsRelaxMode: FGP.AddCollision;
            end;
          end;
    end;

  for i := 0 to vN do
    for j := i + 1 to vN do
      if Asteroids[i].Shape.IsIntersectWith(Asteroids[j].Shape) then
      begin
        Asteroids[i].Collide(Asteroids[j]);
        Asteroids[j].Collide(Asteroids[i]);
      end;
  end;
end;

function TDemoGame.GetDrawFigures: Boolean;
begin
  Result := FEngine.DrawFigures;
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
  i: Integer;
begin

  case GameStatus of
    gsGameOver: GameStatus := gsMenu1;
  end;

  fEngine.MouseDown(Sender, Button, Shift, x, y);

  for i := 0 to Length(fEngine.Downed) - 1 do
    fEngine.Sprites[fEngine.Downed[i]].OnMouseDown(fEngine.Sprites[fEngine.Downed[i]], Button, Shift, x, y);

  FGP.Ship.Destination := DestinationFromClick(x, y);
end;

procedure TDemoGame.MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Single);
begin
  if FEngine.IsMouseDowned then
    FGP.Ship.Destination := DestinationFromClick(x, y);
//    FShip.AddDestination(DestinationFromClick(x, y));
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
  i: Integer;
  vFormatter: TEngineFormatter;
 // vFont: TFont;
  vObj: TEngine2DObject;
begin
  FEngine.Resources.addResFromLoadFileRes('images.load');
  FEngine.Background.LoadFromFile(UniPath('back.jpg'));

  FGP := TGameParam.Create(FLoader);

  FGameOverText := TEngine2DText.Create(FEngine);
  FGameOverText.FontSize := 56;
  FGameOverText.Group := 'gameover';
  FGameOverText.Color :=  TAlphaColorRec.White;
  FGameOverText.TextRect := RectF(-150, -35, 150, 35);
  FGameOverText.Text := 'Game Over';
  FEngine.AddObject(FGameOverText);
  FLoader.Formatter(FGameOverText, 'left: engine.width * 0.5; top: engine.height * 0.5;').Format;

  FEngine.HideGroup('gameover');
  FEngine.HideGroup('stat');

  FMenu := TGameMenu.Create(FEngine);
  FMenu.StartGame := SelectMode;//StartGame;
  FMenu.AboutGame := AboutGame;
  FMenu.StatGame := StatGame;
  FMenu.ExitGame := ExitGame;
  FMenu.RelaxMode := StartRelax;
  FMenu.SurvivalMode := StartSurvival;
  FMenu.StoryMode := SelectLevel;
  FMenu.LevelSelect := SelectMode;
  FEngine.HideGroup('ship');
  FEngine.HideGroup('menu2');
  FEngine.HideGroup('menu3');
  FEngine.HideGroup('relaxmodemenu');

  FEngine.InBeginPaintBehavior := DoGameTick;
  FEngine.Start;
  FLoader.CreateComix;
end;

procedure TDemoGame.Resize;
begin
  FEngine.DoTheFullWindowResize;
  FGP.SetScaling(MonitorScale, SpeedModScale);
end;

procedure TDemoGame.SelectLevel(ASender: TObject);
begin
  Self.GameStatus := gsMenu3;
end;

procedure TDemoGame.SelectMode(ASender: TObject);
begin
  Self.GameStatus := gsMenu2;
end;

procedure TDemoGame.SetDrawFigures(const Value: Boolean);
begin
  FEngine.DrawFigures := Value;
end;

procedure TDemoGame.SetGameStatus(const Value: TGameStatus);
begin
  FGameStatus := Value;
  case FGameStatus of
    gsMenu1: begin FEngine.ShowGroup('menu1,menu'); FMenu.SendToFront; FEngine.HideGroup('gameover,relaxmodemenu,ship,menu2,about,statistics,menu3,relax,survival,story'); end;
    gsMenu2: begin FEngine.ShowGroup('menu2'); FEngine.HideGroup('menu1,menu3'); end;
    gsMenu3: begin FEngine.ShowGroup('menu3'); FEngine.HideGroup('menu2'); end;
    gsStatics: begin FEngine.ShowGroup('statistics'); FEngine.HideGroup('menu1') end;
    gsAbout: begin FEngine.ShowGroup('about'); FEngine.HideGroup('menu1') end;
    gsRelaxMode: begin FGP.RestartGame(Value); FEngine.ShowGroup('relaxmodemenu'); FEngine.HideGroup('menu2,menu'); end;
    gsSurvivalMode: begin FGP.RestartGame(Value);  FEngine.HideGroup('menu2'); FEngine.HideGroup('menu'); end;
    gsStoryMode: begin FGP.RestartGame(Value);  FEngine.HideGroup('menu3,menu'); end;
    gsGameOver: begin FLoader.ShipExplosionAnimation(FGP.Ship); FGP.Ship.Visible := False; FEngine.ShowGroup('gameover'); FGameOverText.SendToFront; end;
  end;
end;

procedure TDemoGame.SetImage(const Value: TImage);
begin
  fEngine.init(Value);
  Value.OnMouseDown := Self.MouseDown;
  Value.OnMouseUp := Self.MouseUp;
  Value.OnMouseMove := Self.MouseMove;
end;

procedure TDemoGame.StartGame(ASender: TObject);
var
  vSpr: TSprite;
begin
  FEngine.HideGroup('menu');
  FEngine.ShowGroup('ship');
  FLoader := TLoader.Create(FEngine);

  for vSpr in FGP.Ship.Parts do
  begin
    vSpr.Opacity := 0;
    FEngine.AnimationList.Add(
      FLoader.OpacityAnimation(vSpr, 1)
    );
  end;

{  FGP.Seconds := 0;
  FGP.Collisions := 0; }

  Self.GameStatus := gsStoryMode;
end;

procedure TDemoGame.StartRelax(ASender: TObject);
begin
  GameStatus := gsRelaxMode;
end;

procedure TDemoGame.StartSurvival(ASender: TObject);
begin
  GameStatus := gsSurvivalMode;
end;

procedure TDemoGame.StatGame(ASender: TObject);
begin
  GameStatus := gsStatics;
end;

{ TGameParam }

function TGameParam.AddAsteroid(ASize, ASpeed: Byte): TAsteroid;
var
  vAsteroid: TAsteroid;
  vSize, vSpeed: Double;
  vX, vY: Double;
begin

  if ASize = prRandom then
    ASize := Random(3) + 1;

  if ASpeed = prRandom then
    ASpeed := Random(3) + 1;

  vSize := ASize * 0.3 + 0.4;
  vSpeed := ASpeed * 1.2 + 3;

  vAsteroid := FLoader.DefinedBigAsteroids(vSize, vSpeed);

  FEngine.AddObject(vAsteroid);
  FAsteroids.Add(vAsteroid);

  // Астеройд появляется за гранью
  case Random(4) of
    0: begin  vX := - vAsteroid.wHalf; vY := Random(FEngine.Height) end;
    1: begin  vX := FEngine.Width + vAsteroid.wHalf; vY := Random(FEngine.Height) end;
    2: begin  vX := Random(FEngine.Width); vY := -vAsteroid.hHalf end;
    3: begin  vX := Random(FEngine.Width); vY := FEngine.Height + vAsteroid.hHalf end;
  end;

  FLoader.Formatter(vAsteroid, 'width: sqrt(engine.width * engine.height) * 0.2;').Format;

  vAsteroid.x := vX;
  vAsteroid.y := vY;

  SetScaling(MonitorScale, SpeedModScale);

end;

procedure TGameParam.AddCollision;
begin
  Inc(FCollisions);
end;

procedure TGameParam.AddTime(const ADeltaTime: Double);
var
  vS: string;
  vDSec, vDSec2: Integer;
begin
  if FGameStatus = gsGameOver then
    Exit;
  FSeconds := FSeconds + ADeltaTime;
  FValueableSeconds := FValueableSeconds + ADeltaTime;
  FSecToNextLevel := FSecToNextLevel + ADeltaTime;
  vDSec2 := Trunc(FSecToNextLevel / 15);
  if vDSec2 > 0 then
  begin
    AddAsteroid(0, 0);
    FSecToNextLevel := FSecToNextLevel - vDSec2 * 15;
  end;

  vDSec := Trunc(FValueableSeconds / 0.1);
  if vDSec > 0 then
    FValueableSeconds := FValueableSeconds - vDSec * 0.1;

   case FGameStatus of
    gsStoryMode: ;
    gsSurvivalMode:
      FScorePoints := FScorePoints + vDSec * FAsteroids.Count;
    gsRelaxMode:
      FScorePoints := Round((FSeconds / (FCollisions + 1)) * FAsteroids.Count);
  end;
end;

procedure TGameParam.BreakLife;
var
  vSpr: TSprite;
begin
  if FLifes.Count > 0 then
  begin
    vSpr := FLifes.Last;
    Floader.Parent.AnimationList.Add(FLoader.BreakLifeAnimation(vSpr));
    FLifes.Count := FLifes.Count - 1;
  end;
end;

constructor TGameParam.Create(ALoader: TLoader);
var
  i: Integer;
  vObj: tEngine2DObject;
begin
  FLifes := TList<TSprite>.Create;
  FPanels := TNamedList<TEngine2DText>.Create;
  FLoader := ALoader;
  FEngine := TDemoEngine(FLoader.Parent);
  FCollisions := 0;
  FSeconds := 0;

  FBackObjects := TList<TLittleAsteroid>.Create;
  // Создаем астеройдное поле
  for i := 0 to 39 do
  begin
    vObj := FLoader.RandomAstroid;
    FBackObjects.Add(TLittleAsteroid(vObj));
    FLoader.Formatter(vObj, 'width: sqrt(engine.width * engine.height) * 0.05; ')
  end;

  // Создаем корабль
  FShip := FLoader.CreateShip;
  FLoader.Formatter(FShip, 'width: sqrt(engine.width * engine.height) * 0.125;');

  FAsteroids := TList<TAsteroid>.Create;
  for i := 0 to 0 do
  begin
    vObj := FLoader.BigAsteroid;
    FAsteroids.Add(TAsteroid(vObj));
    FLoader.Formatter(vObj, 'width: sqrt(engine.width * engine.height) * 0.2;')
  end;
end;

procedure TGameParam.DefineAsteroidCount(const ACount: Integer);
begin
  if FAsteroids.Count > ACount then
    DeleteAsteroid(FAsteroids.Count - ACount);

  while FAsteroids.Count < ACount do
    AddAsteroid(0, 0);
end;

procedure TGameParam.DeleteAsteroid(const ACount: Integer);
var
  i: Integer;
  vSpr: TAsteroid;
begin
  for i := 0 to ACount - 1 do
    if FAsteroids.Count > 0 then
    begin
      vSpr := FAsteroids.Last;
      FAsteroids.Remove(vSpr);
      FEngine.DeleteObject(vSpr);
      vSpr.Free;
    end;
end;

destructor TGameParam.Destroy;
var
  i: Integer;
begin
 for i := 0 to FAsteroids.Count - 1 do
    FAsteroids[i].Free;
  FAsteroids.Free;

  for i := 0 to FBackObjects.Count - 1 do
    FBackObjects[i].Free;
  FBackObjects.Free;

  for i := 0 to FLifes.Count - 1 do
    FLifes[i].Free;
  FLifes.Free;

  for i := 0 to FPanels.Count - 1 do
    FPanels[i].Free;
  FPanels.Free;

  FShip.Free;
end;

procedure TGameParam.FixScore;
begin
  FGameStatus := gsGameOver;
end;

function TGameParam.GetAsteroids: Integer;
begin
  Result := 0;
  if FPanels.IsHere('asteroidsvalue') then
    Result := StrToInt(FPanels['asteroidsvalue'].Text);
end;

function TGameParam.GetCollisions: Integer;
begin
  Result := 0;
  if FPanels.IsHere('collisionsvalue') then
    Result := StrToInt(FPanels['collisionsvalue'].Text);
end;

function TGameParam.GetLevel: Integer;
begin
  Result := 0;
  if FPanels.IsHere('levelvalue') then
    Result := StrToInt(FPanels['levelvalue'].Text);
end;

function TGameParam.GetScore: Integer;
begin
  Result := 0;
  if FPanels.IsHere('scorevalue') then
    Result := StrToInt(FPanels['scorevalue'].Text);
end;

function TGameParam.GetTime: Double;
var
  vErr: Integer;
  vValue: Double;
begin
  Result := 0;
  if FPanels.IsHere('timevalue') then
  begin
    val(FPanels['timevalue'].Text, vValue, vErr);
    if vErr = 0 then
      Result := vValue;
  end;
end;

procedure TGameParam.RenewPanels;
var
  vS: string ;
begin
  Self.Score := FScorePoints;
  if FPanels.IsHere('timevalue') then
  begin
    Str(FSeconds:0:2, vS);
    FPanels['timevalue'].Text := vS;
  end;

  if FPanels.IsHere('collisionsvalue') then
    FPanels['collisionsvalue'].Text := IntToStr(FCollisions);
end;

procedure TGameParam.RestartGame(const AGameMode: TGameStatus);
begin

  FGameStatus := AGameMode;
  case AGameMode of
    gsRelaxMode: begin
      Self.FCollisions := 0;
      FLoader.CreateRelaxPanel(FPanels);
    end;
    gsSurvivalMode: begin
      FLoader.CreateLifes(FLifes, 3);
      FLoader.CreateSurvivalPanel(FPanels);
      DefineAsteroidCount(3);
    end;
    gsStoryMode: begin
      FLoader.CreateLifes(FLifes, 1);
      FLoader.CreateStoryPanel(FPanels);
    end;
  end;


  FLoader.Parent.DoTheFullWindowResize;
  Self.FSeconds := 0;
  Self.FScorePoints := 0;
  Self.FValueableSeconds := 0;
  Self.FShip.Show;
end;

procedure TGameParam.SetLevel(const Value: Integer);
begin
  if FPanels.IsHere('levelvalue') then
    FPanels['levelvalue'].Text := IntToStr(Value);
end;

procedure TGameParam.SetScaling(const AMonitorScale, ASpeedModScale: Double);
var
  i: Integer;
begin
  FShip.SetMonitorScale(AMonitorScale);
  FShip.SetSpeedModScale(ASpeedModScale);

  for i := 0 to FAsteroids.Count - 1 do
  begin
    FAsteroids[i].SetMonitorScale(AMonitorScale);
    FAsteroids[i].SetSpeedModScale(ASpeedModScale);
  end;

  for i := 0 to FBackObjects.Count - 1 do
  begin
    FBackObjects[i].SetMonitorScale(AMonitorScale);
    FBackObjects[i].SetSpeedModScale(ASpeedModScale);
  end;
end;

procedure TGameParam.SetScore(const Value: Integer);
begin
  if FPanels.IsHere('scorevalue') then
    FPanels['scorevalue'].Text := IntToStr(Value);
end;

end.



