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

  TDemoGame = class
  private
    FEngine: TDemoEngine;
    FBackObjects: TList<TLittleAsteroid>; // Летящие бэки
    FAsteroids: TList<TAsteroid>;
    FShip: TShip;
    FLoader: TLoader;


    FMenu: TGameMenu;
    FGameStatus: TGameStatus;

    FCollisions: Integer;
    FLifes: TList<TSprite>;
    FPanels: TNamedList<TEngine2DText>;
   // FCollisionsText, FSecondsText: TEngine2DText;
    FGameOverText: TEngine2DText;
    FSeconds: Single;

    function GetImage: TImage;
    procedure SetImage(const Value: TImage);
    procedure MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; x, y: single);
    procedure MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; x, y: single);
    procedure MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Single);
    procedure BeforePaintBehavior;
    procedure FindCollide;
    function GetSpeed: Single;
    procedure StartGame(ASender: TObject);
    procedure SelectMode(ASender: TObject);
    procedure SelectLevel(ASender: TObject);
    procedure StartRelax(ASender: TObject);
    procedure StartSurvival(ASender: TObject);
    procedure StatGame(ASender: TObject);
    procedure AboutGame(ASender: TObject);
    procedure ExitGame(ASender: TObject);

    procedure RestartGame;
    function DestinationFromClick(const Ax, Ay: Single): TPosition;
    procedure SetGameStatus(const Value: TGameStatus);
    procedure BreakLife;

    function GetAsteroids: Integer;
    function GetCollisions: Integer;
    function GetLevel: Integer;
    function GetScore: Integer;
    function GetTime: Double;
    procedure SetAsteroids(const Value: Integer);
    procedure SetCollisions(const Value: Integer);
    procedure SetLevel(const Value: Integer);
    procedure SetScore(const Value: Integer);
    procedure SetTime(const Value: Double);

    // Статитические данные игры
    property Score: Integer read GetScore write SetScore;
    property Time: Double read GetTime write SetTime;
    property Level: Integer read GetLevel write SetLevel;
    property Collisions: Integer read GetCollisions write SetCollisions;
    property Asteroids: Integer read GetAsteroids write SetAsteroids;
  public
    property GameStatus: TGameStatus read FGameStatus write SetGameStatus;
    property Image: TImage read GetImage write SetImage;
    property Speed: Single read GetSpeed;
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

procedure TDemoGame.BeforePaintBehavior;
var
  vS: String;
begin
  FindCollide;
  FShip.SendToFront;
  FMenu.SendToFront;

  FSeconds := FSeconds + 1/FEngine.EngineThread.FPS;

  case GameStatus of
    gsStoryMode: ;
    gsSurvivalMode: ;
    gsRelaxMode: begin
      Time := FSeconds;
      Collisions := FCollisions;
      Score := Round((FSeconds / (FCollisions + 1)) * FAsteroids.Count);
    end;

  end;
 { Str(FSeconds:0:2, vS);

  FSecondsText.Text := 'Секунд: ' + vS;
  FCollisionsText.Text := 'Столкновений: ' + IntToStr(FCollisions);  }


{  if Assigned(FShip) then
    FShip.FireToBack; }
end;

procedure TDemoGame.BreakLife;
var
  vSpr: TSprite;
begin
  if FLifes.Count > 0 then
  begin
    vSpr := FLifes.Last;
    FEngine.AnimationList.Add(FLoader.BreakLifeAnimation(vSpr));
    FLifes.Count := FLifes.Count - 1;
  end;

  if FLifes.Count <= 0 then
    GameStatus := gsGameOver;
end;

constructor TDemoGame.Create;
var
  vS: String;
begin
  FEngine := TDemoEngine.Create;
  FLoader := TLoader.Create(FEngine);
  FLifes := TList<TSprite>.Create;
  FPanels := TNamedList<TEngine2DText>.Create;
  FCollisions := 0;
  FSeconds := 0;
end;

function TDemoGame.DestinationFromClick(const Ax, Ay: Single): TPosition;
var
  vAngle: Single;
begin
  Result.XY(Ax, Ay);
  vAngle := (ArcTan2(Ay - FShip.y, Ax - FShip.x ) / Pi) * 180 + 90;
  NormalizeAngle(vAngle);
  Result.Rotate := vAngle;
  Result.Scale(FShip.ScalePoint);
end;

destructor TDemoGame.Destroy;
var
  i: Integer;
begin
  FMenu.Free;

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
  FLoader.Free;

  FEngine.Free;

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
  vN := FAsteroids.Count - 1;
  if Assigned(FShip) then
    if FShip.Visible then
    begin
      for i := 0 to vN do
        if FShip.Shape.IsIntersectWith(FAsteroids[i].Shape) then
          if FAsteroids[i].Collide(FShip) then
          begin
            case GameStatus of
              gsStoryMode, gsSurvivalMode: BreakLife;
              gsRelaxMode: Inc(FCollisions);
            end;
          end;
    end;

  for i := 0 to vN do
    for j := i + 1 to vN do
      if FAsteroids[i].Shape.IsIntersectWith(FAsteroids[j].Shape) then
      begin
        FAsteroids[i].Collide(FAsteroids[j]);
        FAsteroids[j].Collide(FAsteroids[i]);
      end;
end;

function TDemoGame.GetAsteroids: Integer;
begin
  Result := 0;
  if FPanels.IsHere('asteroidsvalue') then
    Result := StrToInt(FPanels['asteroidsvalue'].Text);
end;

function TDemoGame.GetCollisions: Integer;
begin
  Result := 0;
  if FPanels.IsHere('collisionsvalue') then
    Result := StrToInt(FPanels['collisionsvalue'].Text);
end;

function TDemoGame.GetImage: TImage;
begin
  Result := FEngine.Image;
end;

function TDemoGame.GetLevel: Integer;
begin
  Result := 0;
  if FPanels.IsHere('levelvalue') then
    Result := StrToInt(FPanels['levelvalue'].Text);
end;

function TDemoGame.GetScore: Integer;
begin
  Result := 0;
  if FPanels.IsHere('scorevalue') then
    Result := StrToInt(FPanels['scorevalue'].Text);
end;

function TDemoGame.GetSpeed: Single;
begin
  Result := FEngine.EngineThread.Speed;
end;

function TDemoGame.GetTime: Double;
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

  FShip.Destination := DestinationFromClick(x, y);
end;

procedure TDemoGame.MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Single);
begin
  if FEngine.IsMouseDowned then
    FShip.Destination := DestinationFromClick(x, y);
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
  for i := 0 to 5 do
  begin
    vObj := FLoader.BigAsteroid;
    FAsteroids.Add(TAsteroid(vObj));
    FLoader.Formatter(vObj, 'width: sqrt(engine.width * engine.height) * 0.2;')
  end;

 { vFont := TFont.Create;
  vFont.Style := [TFontStyle.fsBold];
  vFont.Size := 14;
  FCollisionsText := TEngine2DText.Create(FEngine);
  FCollisionsText.Group := 'relaxmodemenu';
  FCollisionsText.Font := vFont;
  FCollisionsText.TextRect := RectF(-100, -25, 100, 25);
  FCollisionsText.Color := TAlphaColorRec.Aqua;
  FSecondsText := TEngine2DText.Create(FEngine);
  FSecondsText.TextRect := RectF(-100, -25, 100, 25);
  FSecondsText.Font := vFont;
  FSecondsText.Group := 'relaxmodemenu';
  FSecondsText.Color := TAlphaColorRec.Aqua;

  FEngine.AddObject(FCollisionsText);
  FEngine.AddObject(FSecondsText);

  FLoader.Formatter(FCollisionsText, 'left: engine.width * 0.5; top: engine.height - height * 1.2');
  FLoader.Formatter(FSecondsText, 'left: engine.width * 0.5; top: engine.height - height * 0.6;');  }


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

  FEngine.InBeginPaintBehavior := BeforePaintBehavior;
  FEngine.Start;
end;

procedure TDemoGame.Resize;
var
  i: Integer;
begin
  FEngine.DoTheFullWindowResize;

  FShip.SetMonitorScale(MonitorScale);
  FShip.SetSpeedModScale(SpeedModScale);

  for i := 0 to FAsteroids.Count - 1 do
  begin
    FAsteroids[i].SetMonitorScale(MonitorScale);
    FAsteroids[i].SetSpeedModScale(SpeedModScale);
  end;

  for i := 0 to FBackObjects.Count - 1 do
  begin
    FBackObjects[i].SetMonitorScale(MonitorScale);
    FBackObjects[i].SetSpeedModScale(SpeedModScale);
  end;
end;

procedure TDemoGame.RestartGame;
begin
  case GameStatus of
    gsRelaxMode: begin
      Self.FCollisions := 0;
      FLoader.CreateRelaxPanel(FPanels);
    end;
    gsSurvivalMode: begin
      FLoader.CreateLifes(FLifes, 3);
      FLoader.CreateSurvivalPanel(FPanels);
    end;
    gsStoryMode: begin
      FLoader.CreateLifes(FLifes, 1);
      FLoader.CreateStoryPanel(FPanels);
    end;
  end;

  FEngine.DoTheFullWindowResize;
  Self.FSeconds := 0;
  Self.FShip.Show;
end;

procedure TDemoGame.SelectLevel(ASender: TObject);
begin
  Self.GameStatus := gsMenu3;
end;

procedure TDemoGame.SelectMode(ASender: TObject);
begin
  Self.GameStatus := gsMenu2;
end;

procedure TDemoGame.SetAsteroids(const Value: Integer);
begin

end;

procedure TDemoGame.SetCollisions(const Value: Integer);
begin
  if FPanels.IsHere('collisionsvalue') then
    FPanels['collisionsvalue'].Text := IntToStr(Value);
end;

procedure TDemoGame.SetGameStatus(const Value: TGameStatus);
begin
  FGameStatus := Value;
  case FGameStatus of
    gsMenu1: begin FEngine.ShowGroup('menu1'); FEngine.HideGroup('gameover'); FEngine.HideGroup('relaxmodemenu'); FEngine.ShowGroup('menu'); FEngine.HideGroup('ship'); FEngine.HideGroup('menu2'); FEngine.HideGroup('about'); FEngine.HideGroup('statistics'); FEngine.HideGroup('menu3'); end;
    gsMenu2: begin FEngine.ShowGroup('menu2'); FEngine.HideGroup('menu1');  FEngine.HideGroup('menu3'); end;
    gsMenu3: begin FEngine.ShowGroup('menu3'); FEngine.HideGroup('menu2'); end;
    gsStatics: begin FEngine.ShowGroup('statistics'); FEngine.HideGroup('menu1') end;
    gsAbout: begin FEngine.ShowGroup('about'); FEngine.HideGroup('menu1') end;
    gsRelaxMode: begin RestartGame; FEngine.ShowGroup('relaxmodemenu'); FEngine.HideGroup('menu2'); FEngine.HideGroup('menu'); end;
    gsSurvivalMode: begin RestartGame; FEngine.HideGroup('menu2'); FEngine.HideGroup('menu'); end;
    gsStoryMode: begin RestartGame; FEngine.HideGroup('menu3'); FEngine.HideGroup('menu'); end;
    gsGameOver: begin FLoader.ShipExplosionAnimation(FShip); FEngine.ShowGroup('gameover'); end;
  end;
end;

procedure TDemoGame.SetImage(const Value: TImage);
begin
  fEngine.init(Value);
  Value.OnMouseDown := Self.MouseDown;
  Value.OnMouseUp := Self.MouseUp;
  Value.OnMouseMove := Self.MouseMove;
end;

procedure TDemoGame.SetLevel(const Value: Integer);
begin
  if FPanels.IsHere('levelvalue') then
    FPanels['levelvalue'].Text := IntToStr(Value);
end;

procedure TDemoGame.SetScore(const Value: Integer);
begin
  if FPanels.IsHere('scorevalue') then
    FPanels['scorevalue'].Text := IntToStr(Value);
end;

procedure TDemoGame.SetTime(const Value: Double);
var
  vS: string;
begin
  if FPanels.IsHere('timevalue') then
  begin
    Str(Value:0:2, vS);
    FPanels['timevalue'].Text := vS;
  end;
end;

procedure TDemoGame.StartGame(ASender: TObject);
var
  vSpr: TSprite;
begin
  FEngine.HideGroup('menu');
  FEngine.ShowGroup('ship');
  FLoader := TLoader.Create(FEngine);

  for vSpr in FShip.Parts do
  begin
    vSpr.Opacity := 0;
    FEngine.AnimationList.Add(
      FLoader.OpacityAnimation(vSpr, 1)
    );
  end;

  FSeconds := 0;
  FCollisions := 0;

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

end.



