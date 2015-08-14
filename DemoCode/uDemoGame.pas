// Демонстрация Shadow Engine 2D. Дмитрий Сорокин.

unit uDemoGame;

interface

uses
  FMX.Types, System.UITypes, System.Classes, System.Types, FMX.Graphics,
  FMX.Objects, System.Generics.Collections, System.Math, System.SysUtils,
  uEasyDevice, uDemoEngine, uDemoGameLoader, uDemoObjects, uEngine2DObjectShape,
  uEngine2DAnimation, uIntersectorClasses, uDemoMenu, uClasses, uEngine2DText,
  uEngineFormatter;

type
  TDemoGame = class
  private
    FEngine: TDemoEngine;
    FBackObjects: TList<TLittleAsteroid>; // Летящие бэки
    FShip: TShip;
    FCollisions: Integer;
    FSeconds: Single;
    FCollisionsText, FSecondsText: TEngine2DText;
    FMenu: TGameMenu;
    FAsteroids: TList<TAsteroid>;

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
    procedure StatGame(ASender: TObject);
    procedure AboutGame(ASender: TObject);
    procedure ExitGame(ASender: TObject);
    function DestinationFromClick(const Ax, Ay: Single): TPosition;
  public
    property Image: TImage read GetImage write SetImage;
    property Speed: Single read GetSpeed;
    procedure Prepare;
    procedure Resize;
    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses
  FMX.Dialogs, uEngine2DSprite, uEngine2DObject;

{ TDemoGame }

procedure TDemoGame.AboutGame(ASender: TObject);
begin

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
begin
  FEngine := TDemoEngine.Create;
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

  FEngine.Free;

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
  vLoader: TLoader;
  i: Integer;
  vFormatter: TEngineFormatter;
  vFont: TFont;
  vObj: TEngine2DObject;
begin
  FEngine.Resources.addResFromLoadFileRes('images.load');
  FEngine.Background.LoadFromFile(UniPath('back.jpg'));

  FBackObjects := TList<TLittleAsteroid>.Create;
  vLoader := TLoader.Create(FEngine);
  // Создаем астеройдное поле
  for i := 0 to 39 do
  begin
    vObj := vLoader.RandomAstroid;
    FBackObjects.Add(TLittleAsteroid(vObj));
    vFormatter := TEngineFormatter.Create(vObj);
    vFormatter.Text := 'width: sqrt(engine.width * engine.height) * 0.05; ';
    FEngine.FormatterList.Add(vFormatter);
  end;

  // Создаем корабль
  FShip := vLoader.CreateShip;
  vFormatter := TEngineFormatter.Create(FShip);
  vFormatter.Text := 'width: sqrt(engine.width * engine.height) * 0.125;';
  FEngine.FormatterList.Add(vFormatter);

  FAsteroids := TList<TAsteroid>.Create;
  for i := 0 to 5 do
  begin
    vObj := vLoader.BigAsteroid;
    FAsteroids.Add(TAsteroid(vObj));

    vFormatter := TEngineFormatter.Create(vObj);
    vFormatter.Text := 'width: sqrt(engine.width * engine.height) * 0.2;';
    FEngine.FormatterList.Add(vFormatter);
  end;

  vFont := TFont.Create;
  vFont.Style := [TFontStyle.fsBold];
  vFont.Size := 14;
  FCollisionsText := TEngine2DText.Create(FEngine);
  FCollisionsText.Group := 'stat';
  FCollisionsText.Font := vFont;
  FCollisionsText.TextRec := RectF(-100, -25, 100, 25);
  FCollisionsText.Color := TAlphaColorRec.Aqua;
  FSecondsText := TEngine2DText.Create(FEngine);
  FSecondsText.TextRec := RectF(-100, -25, 100, 25);
  FSecondsText.Font := vFont;
  FSecondsText.Group := 'stat';
  FSecondsText.Color := TAlphaColorRec.Aqua;

  FEngine.AddObject(FCollisionsText);
  FEngine.AddObject(FSecondsText);

  vFormatter := TEngineFormatter.Create(FCollisionsText);
  vFormatter.Parent := FEngine;
  vFormatter.Text := 'left: engine.width * 0.5; top: engine.height - height * 1.2';
  FEngine.FormatterList.Add(vFormatter);

  vFormatter := TEngineFormatter.Create(FSecondsText);
  vFormatter.Parent := FEngine;
  vFormatter.Text := 'left: engine.width * 0.5; top: engine.height - height * 0.6;';
  FEngine.FormatterList.Add(vFormatter);

  FEngine.HideGroup('stat');




  FMenu := TGameMenu.Create(FEngine);
  FMenu.StartGame := StartGame;
  FMenu.AboutGame := AboutGame;
  FMenu.StatGame := StatGame;
  FMenu.ExitGame := ExitGame;
  FEngine.HideGroup('ship');

  FEngine.InBeginPaintBehavior := BeforePaintBehavior;
  FEngine.Start;
  vLoader.Free;

  FEngine.Repaint;
    //Self.Resize(getDisplaySizeInPx);
//  FEngine.InBeginPaintBehavior :=
end;

procedure TDemoGame.Resize;
var
  i: Integer;
begin
  FEngine.DoTheFullWindowResize;
 // FShip.Scale := MonitorScale;
  FShip.SetMonitorScale(MonitorScale);
  FShip.SetSpeedModScale(SpeedModScale);
  for i := 0 to FAsteroids.Count - 1 do
  begin
  //  FAsteroids[i].Scale := MonitorScale * 1.5;
    FAsteroids[i].SetMonitorScale(MonitorScale);
    FAsteroids[i].SetSpeedModScale(SpeedModScale);
  end;

  for i := 0 to FBackObjects.Count - 1 do
  begin
    //FBackObjects[i].Scale := MonitorScale*1.5;
    FBackObjects[i].SetMonitorScale(MonitorScale);
    FBackObjects[i].SetSpeedModScale(SpeedModScale);
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

  FSeconds := 0;
  FCollisions := 0;

  FEngine.ShowGroup('stat');

  {FEngine.AnimationList.Add(
      vLoader.ScaleAnimation(vSpr, 1.6)
    );}

end;

procedure TDemoGame.StatGame(ASender: TObject);
begin

end;

end.



