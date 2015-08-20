// Демонстрация Shadow Engine 2D. Дмитрий Сорокин.

unit uDemoGame;

interface

uses
  FMX.Types, System.UITypes, System.Classes, System.Types, FMX.Graphics,
  FMX.Objects, System.Generics.Collections, System.Math, System.SysUtils,
  uEasyDevice, uDemoEngine, uDemoGameLoader, uDemoObjects, uEngine2DObjectShape,
  uEngine2DAnimation, uIntersectorClasses, uDemoMenu, uClasses, uEngine2DText,
  uEngine2DSprite, uEngineFormatter;

type
  TGameStatus = (gsMenu1, gsMenu2, gsMenu3, gsStatics, gsAbout, gsStoryMode, gsSurvivalMode, gsRelaxMode);

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
    FCollisionsText, FSecondsText: TEngine2DText;
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

  Str(FSeconds:0:2, vS);
  FSeconds := FSeconds + 1/FEngine.EngineThread.FPS;
  FSecondsText.Text := 'Секунд: ' + vS;
  FCollisionsText.Text := 'Столкновений: ' + IntToStr(FCollisions);


{  if Assigned(FShip) then
    FShip.FireToBack; }
end;

constructor TDemoGame.Create;
var
  vS: String;
begin
  FEngine := TDemoEngine.Create;
  FLoader := TLoader.Create(FEngine);
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
            Inc(FCollisions);
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
  i: Integer;
begin
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
  vFont: TFont;
  vObj: TEngine2DObject;
begin
  FEngine.Resources.addResFromLoadFileRes('images.load');
  FEngine.Background.LoadFromFile(UniPath('back.jpg'));

  FBackObjects := TList<TLittleAsteroid>.Create;
  FLoader := TLoader.Create(FEngine);
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

  vFont := TFont.Create;
  vFont.Style := [TFontStyle.fsBold];
  vFont.Size := 14;
  FCollisionsText := TEngine2DText.Create(FEngine);
  FCollisionsText.Group := 'relaxmodemenu';
  FCollisionsText.Font := vFont;
  FCollisionsText.TextRec := RectF(-100, -25, 100, 25);
  FCollisionsText.Color := TAlphaColorRec.Aqua;
  FSecondsText := TEngine2DText.Create(FEngine);
  FSecondsText.TextRec := RectF(-100, -25, 100, 25);
  FSecondsText.Font := vFont;
  FSecondsText.Group := 'relaxmodemenu';
  FSecondsText.Color := TAlphaColorRec.Aqua;

  FEngine.AddObject(FCollisionsText);
  FEngine.AddObject(FSecondsText);

  FLoader.Formatter(FCollisionsText, 'left: engine.width * 0.5; top: engine.height - height * 1.2');
  FLoader.Formatter(FSecondsText, 'left: engine.width * 0.5; top: engine.height - height * 0.6;');

  FEngine.HideGroup('stat');

  FMenu := TGameMenu.Create(FEngine);
  FMenu.StartGame := SelectMode;//StartGame;
  FMenu.AboutGame := AboutGame;
  FMenu.StatGame := StatGame;
  FMenu.ExitGame := ExitGame;
  FMenu.RelaxMode := StartRelax;
  FMenu.SurvivalMode := StartRelax;
  FMenu.StoryMode := SelectLevel;
  FMenu.LevelSelect := SelectMode;
  FEngine.HideGroup('ship');
  FEngine.HideGroup('menu2');
  FEngine.HideGroup('menu3');
  FEngine.HideGroup('relaxmodemenu');

  FEngine.InBeginPaintBehavior := BeforePaintBehavior;
  FEngine.Start;
  FLoader.Free;
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
    gsRelaxMode: Self.FCollisions := 0;
    gsSurvivalMode: begin
    FLoader.CreateLifes(FLifes, 3);
//    Self.FLives := 3;
    end;
    //  Self.FLives.
  end;

  Self.FSeconds := 0;
end;

procedure TDemoGame.SelectLevel(ASender: TObject);
begin
  Self.GameStatus := gsMenu3;
end;

procedure TDemoGame.SelectMode(ASender: TObject);
begin
  Self.GameStatus := gsMenu2;
end;

procedure TDemoGame.SetGameStatus(const Value: TGameStatus);
begin
  FGameStatus := Value;
  case FGameStatus of
    gsMenu1: begin FEngine.ShowGroup('menu1'); FEngine.HideGroup('relaxmodemenu'); FEngine.ShowGroup('menu'); FEngine.HideGroup('ship'); FEngine.HideGroup('menu2'); FEngine.HideGroup('about'); FEngine.HideGroup('statistics'); FEngine.HideGroup('menu3'); end;
    gsMenu2: begin FEngine.ShowGroup('menu2'); FEngine.HideGroup('menu1');  FEngine.HideGroup('menu3'); end;
    gsMenu3: begin FEngine.ShowGroup('menu3'); FEngine.HideGroup('menu2'); end;
    gsStatics: begin FEngine.ShowGroup('statistics'); FEngine.HideGroup('menu1') end;
    gsAbout: begin FEngine.ShowGroup('about'); FEngine.HideGroup('menu1') end;
    gsRelaxMode: begin RestartGame; FEngine.ShowGroup('ship'); FEngine.ShowGroup('relaxmodemenu'); FEngine.HideGroup('menu2'); FEngine.HideGroup('menu'); end;
    gsSurvivalMode: begin RestartGame; FEngine.ShowGroup('ship'); FEngine.HideGroup('menu2'); FEngine.HideGroup('menu'); end;
    gsStoryMode: begin RestartGame; FEngine.ShowGroup('ship'); FEngine.HideGroup('menu3'); FEngine.HideGroup('menu'); end;
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



