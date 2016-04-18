unit uDemoGameLoader;

interface

uses
  SysUtils, System.Types, System.Math, System.Generics.Collections, System.UITypes,
  FMX.Graphics, {$I 'Utils\DelphiCompatability.inc'}
  uEngine2D, uEngine2DSprite, uEngine2DObject, uDemoObjects, uIntersectorClasses,
  uEngine2DAnimation, uEngine2DStandardAnimations, uEngine2DClasses, uEngineFormatter,
  uEngine2DText, uNamedList, uEngine2DShape, uEngine2DManager,
  uNewFigure, uIntersectorMethods, uEasyDevice, uClasses;

type
  TLoader = class
  private
    FCreator: tEngine2DObjectCreator;
    FBackground: TBitmap;
  protected
    function FastText(const AName: string; AFont: TFont; const AColor: TAlphaColor = TAlphaColorRec.White; const AGroup: string = ''; const AJustify: TObjectJustify = Center): TEngine2DText;
    procedure ClearAndDestroyPanel(FPanel: TNamedList<tEngine2DText>);
    procedure Comix1Create(var AComixText: TEngine2DText);
    procedure Comix2Create(var AComixText: TEngine2DText);
    procedure Comix3Create;
  public
    function RandomAstroid: TLittleAsteroid;
    function BigAsteroid: TAsteroid;
    function DefinedBigAsteroids(const ASize, ASpeed: Double): TAsteroid;
    function CreateShip: TShip;
    function Explosion(const AX, AY, AAng: Double): TExplosion;
    function ExplosionAnimation(ASubject: TSprite): TSpriteAnimation;
    function OpacityAnimation(ASubject: TSprite; const AEndOpacity: Double): TOpacityAnimation;
    function ScaleAnimation(ASubject: TSprite; const AEndScale: Double): TMigrationAnimation;
    function BreakLifeAnimation(ASubject: TSprite): TOpacityAnimation;
    procedure ShipExplosionAnimation(ASubject: TShip);
    function ButtonAnimation(ASubject: TEngine2dObject; const AEndScale: Double):  TMouseDownMigrationAnimation;
    function Formatter(const ASubject: tEngine2DObject; const AText: String): TEngineFormatter;
    procedure CreateLifes(FLifes: TList<TSprite>; const ACount: Integer);
    procedure CreateSurvivalPanel(FPanel: TNamedList<tEngine2DText>);
    procedure CreateRelaxPanel(FPanel: TNamedList<tEngine2DText>);
    procedure CreateStoryPanel(FPanel: TNamedList<tEngine2DText>);
    procedure CreateComix(var AText1, AText2: TEngine2dText); // Во входных параметрах тексты с комиксов, чтобы изменять их
    class function ShipFlyAnimation(ASubject: TSprite; const APosition: TPosition): TAnimation;

    constructor Create(ACreator: TEngine2DObjectCreator; ABackGround: TBitmap);
  end;

  function MonitorScale: Single;
  function SpeedModScale: Single;

implementation

function MonitorScale: Single;
begin
  Result := (
    (uEasyDevice.getDisplaySizeInPx.X + uEasyDevice.getDisplaySizeInPx.Y) * 0.5) * 0.001;
end;

function SpeedModScale: Single;
begin
  Result := (
   ((uEasyDevice.getDisplaySizeInPx.X + uEasyDevice.getDisplaySizeInPx.Y) * 0.5) * 0.001) / getScreenScale;
end;

{ TLoader }

function TLoader.BigAsteroid: TAsteroid;
var
  vSpr: TAsteroid;
  vFigure: TNewFigure;
  vCircle: TCircle;
begin
  vSpr := TAsteroid.Create(FCreator);
  vSpr.Group := 'activeobject';
  vSpr.x := Random(FCreator.EngineWidth);
  vSpr.y := Random(FCreator.EngineHeight);
  vFigure := TNewFigure.Create(TNewFigure.cfCircle);
  vCircle.X := 0;
  vCircle.Y := 0;
  vCircle.Radius := 50;
  vFigure.SetData(vCircle);
  vSpr.Shape.AddFigure(vFigure);
  vSpr.ScaleMod := RandomRange(60, 140) / 100;

  vSpr := TAsteroid(FCreator.Add(vSpr)); // Добавлять можно только так спрайты
  vSpr.CurRes := 1;

  Result := vSpr;
end;

procedure TLoader.ClearAndDestroyPanel(FPanel: TNamedList<tEngine2DText>);
var
  i: Integer;
  vObj: tEngine2DObject;
begin
  for i := 0 to FPanel.Count - 1 do
  begin
    vObj := FPanel[i];
    FCreator.RemoveObject(vObj);
    vObj.Free;
  end;
  FPanel.Clear;
end;

procedure TLoader.Comix1Create(var AComixText: TEngine2DText);
var
  vFig: TEngine2DShape;
  vTxt: TEngine2DText;
  vGroup: string;
begin
  vGroup := 'comix1';

  vFig := TFillRect.Create;
  vFig.Group := vGroup;
  vFig.FigureRect := RectF(-200, -200, 200, 200);
  vFig.Brush.Bitmap.Bitmap.Assign(FBackground);
  vFig.Brush.Kind := TBrushKind.Bitmap;
  vFig.Pen.Thickness := 4;
  vFig.Pen.Color := RGBColor(62 , 6, 30, 255).Color;
  FCreator.Add(vFig);

  FCreator.Formatter(vFig, 'comix1back', [], 0);
  vFig.Visible := True;
  vFig.Opacity := 1;

  FCreator.AutoSprite('planetcomix', 'comix1planet', [], vGroup);
  FCreator.AutoSprite('shipcomix', 'comix1ship', [], vGroup);
  FCreator.AutoSprite('captain1', 'comix1captain', [], vGroup);

  vTxt := FCreator.Text('monolog1').Config(
    'Great! I so close to the destination planet! I need only 3 minutes to reach it.',
    TAlphaColorRec.White,
    vGroup,
    CenterRight);
  vTxt.WordWrap := True;
  vTxt.TextRect := RectF(-200, -100, 200, 100);
  vTxt.FontSize := 36;
  FCreator.Formatter(vTxt, 'comix1text', [], 0).Format;

  AComixText := vTxt;
end;

procedure TLoader.Comix2Create(var AComixText: TEngine2DText);
var
  vFig: TEngine2DShape;
  vGroup: string;
  vTxt: TEngine2DText;
begin
  vGroup := 'comix2';
  vFig := TFillRect.Create;
  vFig.Group := vGroup;
  vFig.FigureRect := RectF(-200, -200, 200, 200);
  vFig.Brush.Bitmap.Bitmap.Assign(FBackground);
  vFig.Brush.Kind := TBrushKind.Bitmap;
  vFig.Pen.Thickness := 4;
  vFig.Pen.Color := RGBColor(62 , 6, 30, 255).Color;
  FCreator.Add(vFig);

  FCreator.Formatter(vFig, 'comix2back', [], 0);
  vFig.Visible := True;
  vFig.Opacity := 1;

  FCreator.AutoSprite('planetcomix', 'comix2planet', [], vGroup);
  FCreator.AutoSprite('shipcomix', 'comix2ship', [], vGroup);
  FCreator.AutoSprite('captain2', 'comix2captain', [], vGroup);
  FCreator.AutoSprite('asteroidcomix', 'comix2asteroids', [], vGroup);

  vTxt := FCreator.Text('monolog2').Config(
    'Usually asteroids are at a very long distance from each other. But here are 5 of them within easy reach! And they are moving towards me at the high speed!',
    TAlphaColorRec.White,
    vGroup,
    CenterRight);
  vTxt.WordWrap := True;
  vTxt.TextRect := RectF(-260, -150, 260, 150);
  vTxt.FontSize := 36;

  FCreator.Formatter(vTxt, 'comix2text', [], 0).Format;;
  AComixText := vTxt;
end;

procedure TLoader.Comix3Create;
var
  vFig: TEngine2DShape;
  vGroup: string;
  vTxt: TEngine2DText;
begin
  vGroup := 'comix3';
  vFig := TFillRect.Create;
  vFig.Group := vGroup;
  vFig.FigureRect := RectF(-200, -200, 200, 200);
  vFig.Brush.Bitmap.Bitmap.Assign(FBackground);
  vFig.Brush.Kind := TBrushKind.Bitmap;
  vFig.Pen.Thickness := 4;
  vFig.Pen.Color := RGBColor(62 , 6, 30, 255).Color;
  FCreator.Add(vFig);

  FCreator.Formatter(vFig, 'comix3back', [], 0);
  vFig.Visible := True;
  vFig.Opacity := 1;

  FCreator.AutoSprite('planetcomix', 'comix3planet', [], vGroup);
  FCreator.AutoSprite('shipcomix', 'comix3ship', [], vGroup);
  FCreator.AutoSprite('captain3', 'comix3captain', [], vGroup);
  FCreator.AutoSprite('asteroidcomix', 'comix3asteroids', [], vGroup);
  FCreator.AutoSprite('arrow', 'comix3arrow', [], vGroup);

  vTxt := FCreator.Text('monolog3').Config('Your destination', TAlphaColorRec.White, vGroup);
  vTxt.WordWrap := True;
  vTxt.TextRect := RectF(-150, -50, 150, 50);
  vTxt.FontSize := 36;
  FCreator.Formatter(vTxt, 'comix3text', [], 0).Format;
end;

constructor TLoader.Create(ACreator: TEngine2DObjectCreator; ABackGround: TBitmap);
begin
  FBackground := ABackground;
  FCreator := ACreator;
end;

procedure TLoader.CreateComix(var AText1, AText2: TEngine2dText);
begin
  Comix1Create(AText1);
  Comix2Create(AText2);
  Comix3Create;
  FCreator.HideGroup('comix1, comix2, comix3');
end;

procedure TLoader.CreateLifes(FLifes: TList<TSprite>; const ACount: Integer);
var
  i: Integer;
  vSpr: TSprite;
begin
  for i := FLifes.Count to ACount - 1 do
  begin
    vSpr := FCreator.Sprite.Config('lifeicon', 'survival', TObjectJustify.Center);
    vSpr.Visible := True;
    FCreator.Formatter(vSpr, 'lifes', [i], 0).Format;
    FLifes.Add(vSpr)
  end;
end;

procedure TLoader.CreateRelaxPanel(FPanel: TNamedList<tEngine2DText>);
var
  vText: TEngine2DText;
  vFont: TFont;
  vPrimaryColor: TAlphaColor;
  i: Integer;
  vGroup: String;
begin
  vGroup := 'relax';
  ClearAndDestroyPanel(FPanel);
  vFont := TFont.Create;
  vFont.Style := [TFontStyle.fsBold];
  vFont.Size := 14;
  vPrimaryColor := TAlphaColorRec.White;

  vText := FastText('time', vFont, vPrimaryColor, vGroup, CenterLeft);
  vText.Text := 'Time:';
  FPanel.Add('time', vText);
  FCreator.Formatter(vText,'panelleftside', [1]).Format;
  vText := FastText('timevalue', vFont, vPrimaryColor, vGroup, CenterRight);
  vText.Text := '0';
  FPanel.Add('timevalue', vText);
  FCreator.Formatter(vText,'panelrightside', [1]).Format;

  vText := FastText('collisions', vFont, vPrimaryColor, vGroup, CenterLeft);
  vText.Text := 'Collisions:';
  FPanel.Add('collisions', vText);
  FCreator.Formatter(vText,'panelleftside', [2]).Format;
  vText := FastText('collisionsvalue', vFont, vPrimaryColor, vGroup, CenterRight);
  vText.Text := '0';
  FPanel.Add('collisionsvalue', vText);
  FCreator.Formatter(vText,'panelrightside', [2]).Format;

  vText := FastText('score', vFont, vPrimaryColor, vGroup, CenterLeft);
  vText.Text := 'Score:';
  FPanel.Add('score', vText);
  FCreator.Formatter(vText,'panelleftside', [3]).Format;
  vText := FastText('scoresvalue', vFont, vPrimaryColor, vGroup, CenterRight);
  vText.Text := '0';
  FPanel.Add('scorevalue', vText);
  FCreator.Formatter(vText,'panelrightside', [3]).Format;

  for i := 0 to FPanel.Count - 1 do
    FPanel[i].Opacity := 0.5;

  FCreator.ShowGroup(vGroup);
end;

procedure TLoader.CreateStoryPanel(FPanel: TNamedList<tEngine2DText>);
var
  vText: TEngine2DText;
  vFont: TFont;
  vPrimaryColor, vSecondaryColor: TAlphaColor;
  i: Integer;
  vGroup: String;
begin
  vGroup := 'story';
  ClearAndDestroyPanel(FPanel);
  vFont := TFont.Create;
  vFont.Style := [TFontStyle.fsBold];
  vFont.Size := 14;
  vPrimaryColor := TAlphaColorRec.White;
  vSecondaryColor := TAlphaColorRec.Lightgray;

  vText := FastText('level', vFont, vSecondaryColor, vGroup, CenterLeft);
  vText.Text := 'Level:';
  FPanel.Add('level', vText);
  FCreator.Formatter(vText,'panelleftside', [1]).Format;
  vText := FastText('levelvalue', vFont, vSecondaryColor, vGroup, CenterRight);
  vText.Text := '0';
  FPanel.Add('levelvalue', vText);
  FCreator.Formatter(vText,'panelrightside', [1]).Format;

  vText := FastText('time', vFont, vPrimaryColor, vGroup, CenterLeft);
  vText.Text := 'Time left:';
  FPanel.Add('time', vText);
  FCreator.Formatter(vText,'panelleftside', [2]).Format;
  vText := FastText('timevalue', vFont, vPrimaryColor, vGroup, CenterRight);
  vText.Text := '0';
  FPanel.Add('timevalue', vText);
  FCreator.Formatter(vText,'panelrightside', [2]).Format;

  for i := 0 to FPanel.Count - 1 do
    FPanel[i].Opacity := 0.5;

  FCreator.ShowGroup(vGroup);
end;

procedure TLoader.CreateSurvivalPanel(FPanel: TNamedList<tEngine2DText>);
var
  vText: TEngine2DText;
  vFont: TFont;
  vPrimaryColor: TAlphaColor;
  i: Integer;
  vGroup: String;
begin
  vGroup := 'survival';
  ClearAndDestroyPanel(FPanel);
  vFont := TFont.Create;
  vFont.Style := [TFontStyle.fsBold];
  vFont.Size := 14;
  vPrimaryColor := TAlphaColorRec.White;

  vText := FastText('time', vFont, vPrimaryColor, vGroup, CenterLeft);
  vText.Text := 'Time:';
  FPanel.Add('time', vText);
  FCreator.Formatter(vText,'panelleftside', [1]).Format;
  vText := FastText('timevalue', vFont, vPrimaryColor, vGroup, CenterRight);
  vText.Text := '0';
  FPanel.Add('timevalue', vText);
  FCreator.Formatter(vText, 'panelrightside', [1]).Format;

  vText := FastText('score', vFont, vPrimaryColor, vGroup, CenterLeft);
  vText.Text := 'Score:';
  FPanel.Add('score', vText);
  FCreator.Formatter(vText,'panelleftside', [2]).Format;
  vText := FastText('scoresvalue', vFont, vPrimaryColor, vGroup, CenterRight);
  vText.Text := '0';
  FPanel.Add('scorevalue', vText);
  FCreator.Formatter(vText, 'panelrightside', [2]).Format;

  for i := 0 to FPanel.Count - 1 do
    FPanel[i].Opacity := 0.5;

  FCreator.ShowGroup(vGroup);
end;

function TLoader.DefinedBigAsteroids(const ASize, ASpeed: Double): TAsteroid;
var
  vSpr: TAsteroid;
  vFigure: TNewFigure;
  vCircle: TCircle;
  vAng: Double;
begin
  vSpr := TAsteroid.Create(FCreator);
  vSpr.Group := 'activeobject';
  vSpr.x := Random(FCreator.EngineWidth);
  vSpr.y := Random(FCreator.EngineHeight);
  vFigure := TNewFigure.Create(TNewFigure.cfCircle);
  vCircle.X := 0;
  vCircle.Y := 0;
  vCircle.Radius := 50;
  vFigure.SetData(vCircle);
  vSpr.Shape.AddFigure(vFigure);
  vSpr.ScaleMod := ASize;

  vAng := Random(360) + random;

  vSpr.DX := ASpeed * Cos(vAng * pi180);
  vSpr.DY := ASpeed * Sin(vAng * pi180);

  Result := vSpr;
end;

function TLoader.CreateShip: TShip;
var
  vSpr: TShip;
  vShape: TNewFigure;
  vPoly: TPolygon;
  vCircle: TCircle;
  vPoly1, vPoly2, vPoly3: TNewFigure;
begin
  vSpr := TShip.Create(FCreator);
  vSpr.Group := 'ship';
  vSpr.x := 200;
  vSpr.y := 200;
  vSpr.Rotate := Random(360);

  vPoly1 := TNewFigure.Create(TNewFigure.cfPoly);
  Clear(vPoly);
  AddPoint(vPoly, PointF(0, -165));
  AddPoint(vPoly, PointF(-50, -55));
  AddPoint(vPoly, PointF(50, -55));
  Scale(vPoly, PointF(0.9, 0.9));
  vPoly1.SetData(vPoly);

  vPoly2 := TNewFigure.Create(TNewFigure.cfPoly);
  Clear(vPoly);
  AddPoint(vPoly, PointF(-95, -115));
  AddPoint(vPoly, PointF(-70, -85));
  AddPoint(vPoly, PointF(-55, -45));
  AddPoint(vPoly, PointF(-115, 20));
  Scale(vPoly, PointF(0.9, 0.9));
  vPoly2.SetData(vPoly);

  vPoly3 := TNewFigure.Create(TNewFigure.cfPoly);
  Clear(vPoly);
  AddPoint(vPoly, PointF(95, -115));
  AddPoint(vPoly, PointF(70, -85));
  AddPoint(vPoly, PointF(55, -45));
  AddPoint(vPoly, PointF(115, 20));
  Scale(vPoly, PointF(0.9, 0.9));
  vPoly3.SetData(vPoly);

  vShape := TNewFigure.Create(TNewFigure.cfCircle);
  vCircle.X := 0;
  vCircle.Y := 50;
  vCircle.Radius := 115;
  vShape.SetData(vCircle);

  vSpr.Shape.AddFigure(vPoly1);
  vSpr.Shape.AddFigure(vPoly2);
  vSpr.Shape.AddFigure(vPoly3);
  vSpr.Shape.AddFigure(vShape);
  vSpr.Visible := False;

  FCreator.Add(vSpr, 'ship').Config(-1,'ship', TObjectJustify.Center); // Добавлять можно только так спрайты

  Result := vSpr;
end;

function TLoader.Explosion(const AX, AY, AAng: Double): TExplosion;
var
  vSpr: TExplosion;
begin
  vSpr := TExplosion.Create;

  vSpr.Group := 'activeobject';
  vSpr.x := AX;
  vSpr.y := AY;
  vSpr.Rotate := AAng;
  vSpr.Scale := 0.5;
  FCreator.Add(vSpr);
  vSpr.CurRes := 9;

  Result := vSpr;
end;

function TLoader.ExplosionAnimation(ASubject: TSprite): TSpriteAnimation;
var
  vRes: TSpriteAnimation;
  vSlides: tIntArray;
  i: Integer;
begin
  SetLength(vSlides, 4);
  for i := 0 to High(vSlides) do
    vSlides[i] := i + 9;
  vRes := TSpriteAnimation.Create;
  vRes.Slides := vSlides;
  vRes.TimeTotal := 250;
  vRes.Subject := ASubject;
  vRes.OnSetup := ASubject.SendToFront;
  vRes.OnDestroy := vRes.DeleteSubject;

  Result := vRes;
end;

function TLoader.FastText(const AName: string; AFont: TFont;
  const AColor: TAlphaColor; const AGroup: string; const AJustify: TObjectJustify): TEngine2DText;
begin
  Result := TEngine2DText.Create;
  Result.Group := AGroup;
  Result.Font := AFont;
  Result.Color := AColor;
  Result.Justify := AJustify;
  Result.TextRect := RectF(-50, -7, 50, 7);
  FCreator.Add(Result, AName);
end;

function TLoader.Formatter(const ASubject: tEngine2DObject;
  const AText: String): TEngineFormatter;
begin
  Result := FCreator.Formatter(ASubject, AText);
end;

function TLoader.OpacityAnimation(ASubject: TSprite; const AEndOpacity: Double): TOpacityAnimation;
var
  vRes: TOpacityAnimation;
begin
  vRes := TOpacityAnimation.Create;
  vRes.EndOpaque := AEndOpacity;
  vRes.TimeTotal := 800;
  vRes.Subject := ASubject;

  Result := vRes;
end;

function TLoader.RandomAstroid: TLittleAsteroid;
var
  vSpr: TLittleAsteroid;
begin
  vSpr := TLittleAsteroid.Create(FCreator);
  vSpr.Group := 'backobjects';
  vSpr.x := Random(FCreator.EngineWidth);
  vSpr.y := Random(FCreator.EngineHeight);
  vSpr.Rotate := Random(360);
  FCreator.Add(vSpr); // Добавлять можно только так спрайты
  vSpr.CurRes := vSpr.Tip + 5;

  Result := vSpr;
end;

function TLoader.BreakLifeAnimation(ASubject: TSprite): TOpacityAnimation;
var
  vRes: TOpacityAnimation;
begin
  vRes := TOpacityAnimation.Create;
  vRes.EndOpaque := 0;
  vRes.StartOpaque := ASubject.Opacity;
  vRes.TimeTotal := 500;
  vRes.Subject := ASubject;
  vRes.OnDestroy := vRes.DeleteSubject;

  Result := vRes;
end;

function TLoader.ButtonAnimation(ASubject: TEngine2dObject;
  const AEndScale: Double): TMouseDownMigrationAnimation;
var
  vRes:  TMouseDownMigrationAnimation;
  vPos: TPosition;
begin
  vRes := TMouseDownMigrationAnimation.Create;
  vPos := ASubject.Position;
  vPos.ScaleX := AEndScale;
  vPos.ScaleY := AEndScale;
  vRes.EndPos := vPos;
  vRes.TimeTotal := 200;
  vRes.Subject := ASubject;

  Result := vRes;
end;

function TLoader.ScaleAnimation(ASubject: TSprite;
  const AEndScale: Double): TMigrationAnimation;
var
  vRes: TMigrationAnimation;
  vPos: TPosition;
begin
  vRes := TMigrationAnimation.Create;
  vPos := ASubject.Position;
  vPos.ScaleX := AEndScale;
  vPos.ScaleY := AEndScale;
  vPos.Rotate := vPos.Rotate - 360;
  vRes.EndPos := vPos;
  vRes.TimeTotal := 250;
  vRes.Subject := ASubject;

  Result := vRes;
end;

procedure TLoader.ShipExplosionAnimation(ASubject: TShip);
var
  vRes: TMigrationAnimation;
begin
  vRes := ScaleAnimation(ASubject, 0.01);
  vRes.OnDestroy := ASubject.Hide;
  FCreator.Add(vRes);
end;

class function TLoader.ShipFlyAnimation(ASubject: TSprite; const APosition: TPosition): TAnimation;
var
  vRes: TMigrationAnimation;
begin
  vRes := TMigrationAnimation.Create;
  vRes.EndPos := APosition;
  vRes.TimeTotal := 500;
  vRes.Subject := ASubject;
  vRes.OnSetup := ASubject.SendToFront;

  Result := vRes;
end;

end.
