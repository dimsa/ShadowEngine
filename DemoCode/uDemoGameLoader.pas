unit uDemoGameLoader;

interface

uses
  SysUtils, System.Types, System.Math, System.Generics.Collections, System.UITypes,
  FMX.Graphics,
  {$IFDEF VER290} System.Math.Vectors, {$ENDIF}
  uEngine2D, uEngine2DSprite, uEngine2DObject, uDemoObjects, uIntersectorClasses,
  uEngine2DAnimation, uEngine2DStandardAnimations, uEngine2DClasses, uEngineFormatter,
  uEngine2DText, uNamedList, uEngine2DShape,
  uNewFigure, uIntersectorMethods, uEasyDevice, uClasses;

type
  TLoader = class
  private
    FEngine: TEngine2D;
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
    function Sprite(const AResource: string = ''; const AGroup: string = ''; const AName: string = ''): TSprite;
    procedure CreateLifes(FLifes: TList<TSprite>; const ACount: Integer);
    procedure CreateSurvivalPanel(FPanel: TNamedList<tEngine2DText>);
    procedure CreateRelaxPanel(FPanel: TNamedList<tEngine2DText>);
    procedure CreateStoryPanel(FPanel: TNamedList<tEngine2DText>);
    procedure CreateComix(var AText1, AText2: TEngine2dText); // Во входных параметрах тексты с комиксов, чтобы изменять их
    class function ShipFlyAnimation(ASubject: TSprite; const APosition: TPosition): TAnimation;

    property Parent: tEngine2d read FEngine;

    constructor Create(AEngine: TEngine2D);
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
  vSpr := TAsteroid.Create(FEngine);
  vSpr.Parent := FEngine;
  vSpr.Resources := FEngine.Resources;
  vSpr.CurRes := 1;
  vSpr.Group := 'activeobject';
  vSpr.x := Random(FEngine.Width);
  vSpr.y := Random(FEngine.Height);
  vFigure := TNewFigure.Create(TNewFigure.cfCircle);
  vCircle.X := 0;
  vCircle.Y := 0;
  vCircle.Radius := 50;
  vFigure.SetData(vCircle);
  vSpr.Shape.AddFigure(vFigure);
  vSpr.ScaleMod := RandomRange(60, 140) / 100;


  FEngine.AddObject(vSpr); // Добавлять можно только так спрайты

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
    FEngine.DeleteObject(vObj);
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

  vFig := TFillRect.Create(FEngine);
  vFig.Group := vGroup;
  vFig.FigureRect := RectF(-200, -200, 200, 200);
  vFig.Brush.Bitmap.Bitmap.Assign(FEngine.Background);
  vFig.Brush.Kind := TBrushKind.bkBitmap;
  vFig.Pen.Thickness := 4;
  vFig.Pen.Color := RGBColor(62 , 6, 30, 255).Color;
  FEngine.AddObject(vFig);

  FEngine.New.Formatter(vFig, 'comix1back', [], 0);
  vFig.Visible := True;
  vFig.Opacity := 1;

  FEngine.New.AutoSprite('planetcomix', 'comix1planet', [], vGroup);
  FEngine.New.AutoSprite('shipcomix', 'comix1ship', [], vGroup);
  FEngine.New.AutoSprite('captain1', 'comix1captain', [], vGroup);

  vTxt := FEngine.New.Text(
    'Great! I so close to the destination planet! I need only 3 minutes to reach it.',
    TAlphaColorRec.White,
    'monolog1',
    vGroup,
    CenterRight);
  vTxt.WordWrap := True;
  vTxt.TextRect := RectF(-200, -100, 200, 100);
  vTxt.FontSize := 36;
  FEngine.New.Formatter(vTxt, 'comix1text', [], 0).Format;

  AComixText := vTxt;
end;

procedure TLoader.Comix2Create(var AComixText: TEngine2DText);
var
  vFig: TEngine2DShape;
  vGroup: string;
  vTxt: TEngine2DText;
begin
  vGroup := 'comix2';
  vFig := TFillRect.Create(FEngine);
  vFig.Group := vGroup;
  vFig.FigureRect := RectF(-200, -200, 200, 200);
  vFig.Brush.Bitmap.Bitmap.Assign(FEngine.Background);
  vFig.Brush.Kind := TBrushKind.bkBitmap;
  vFig.Pen.Thickness := 4;
  vFig.Pen.Color := RGBColor(62 , 6, 30, 255).Color;
  FEngine.AddObject(vFig);

  FEngine.New.Formatter(vFig, 'comix2back', [], 0);
  vFig.Visible := True;
  vFig.Opacity := 1;

  FEngine.New.AutoSprite('planetcomix', 'comix2planet', [], vGroup);
  FEngine.New.AutoSprite('shipcomix', 'comix2ship', [], vGroup);
  FEngine.New.AutoSprite('captain2', 'comix2captain', [], vGroup);
  FEngine.New.AutoSprite('asteroidcomix', 'comix2asteroids', [], vGroup);

  vTxt := FEngine.New.Text(
    'Usually asteroids are at a very long distance from each other. But here are 5 of them within easy reach! And they are moving towards me at the high speed!',
    TAlphaColorRec.White,
    'monolog2',
    vGroup,
    CenterRight);
  vTxt.WordWrap := True;
  vTxt.TextRect := RectF(-260, -150, 260, 150);
  vTxt.FontSize := 36;

  FEngine.New.Formatter(vTxt, 'comix2text', [], 0).Format;;
  AComixText := vTxt;
end;

procedure TLoader.Comix3Create;
var
  vFig: TEngine2DShape;
  vGroup: string;
  vTxt: TEngine2DText;
begin
  vGroup := 'comix3';
  vFig := TFillRect.Create(FEngine);
  vFig.Group := vGroup;
  vFig.FigureRect := RectF(-200, -200, 200, 200);
  vFig.Brush.Bitmap.Bitmap.Assign(FEngine.Background);
  vFig.Brush.Kind := TBrushKind.bkBitmap;
  vFig.Pen.Thickness := 4;
  vFig.Pen.Color := RGBColor(62 , 6, 30, 255).Color;
  FEngine.AddObject(vFig);

   FEngine.New.Formatter(vFig, 'comix3back', [], 0);
  vFig.Visible := True;
  vFig.Opacity := 1;

  FEngine.New.AutoSprite('planetcomix', 'comix3planet', [], vGroup);
  FEngine.New.AutoSprite('shipcomix', 'comix3ship', [], vGroup);
  FEngine.New.AutoSprite('captain3', 'comix3captain', [], vGroup);
  FEngine.New.AutoSprite('asteroidcomix', 'comix3asteroids', [], vGroup);
  FEngine.New.AutoSprite('arrow', 'comix3arrow', [], vGroup);

  vTxt := FEngine.New.Text('Your destination', TAlphaColorRec.White, 'monolog3', vGroup);
  vTxt.WordWrap := True;
  vTxt.TextRect := RectF(-150, -50, 150, 50);
  vTxt.FontSize := 36;
  FEngine.New.Formatter(vTxt, 'comix3text', [], 0).Format;
end;

constructor TLoader.Create(AEngine: TEngine2D);
begin
  FEngine := AEngine;
end;

procedure TLoader.CreateComix(var AText1, AText2: TEngine2dText);
begin
  Comix1Create(AText1);
  Comix2Create(AText2);
  Comix3Create;
  FEngine.HideGroup('comix1, comix2, comix3');
end;

procedure TLoader.CreateLifes(FLifes: TList<TSprite>; const ACount: Integer);
var
  i: Integer;
  vSpr: TSprite;
begin
  for i := FLifes.Count to ACount - 1 do
  begin
    vSpr := TSprite.Create(FEngine);
    vSpr.Resources := FEngine.Resources;
    vSpr.Group := 'survival';
    vSpr.CurRes := FEngine.Resources.IndexOf('lifeicon');
    FEngine.AddObject(vSpr);
    vSpr.Visible := True;

    FEngine.New.Formatter(vSpr, 'lifes', [i], 0).Format;
    FLifes.Add(vSpr)
  end;
end;

procedure TLoader.CreateRelaxPanel(FPanel: TNamedList<tEngine2DText>);
var
  vText: TEngine2DText;
  vFont: TFont;
  vPrimaryColor, vSecondaryColor: TAlphaColor;
  i: Integer;
  vGroup: String;
begin
  vGroup := 'relax';
  ClearAndDestroyPanel(FPanel);
  vFont := TFont.Create;
  vFont.Style := [TFontStyle.fsBold];
  vFont.Size := 14;
  vPrimaryColor := TAlphaColorRec.White;
  vSecondaryColor := TAlphaColorRec.Lightgray;


  vText := FastText('time', vFont, vPrimaryColor, vGroup, CenterLeft);
  vText.Text := 'Time:';
  FPanel.Add('time', vText);
  FEngine.New.Formatter(vText,'panelleftside', [1]).Format;
  vText := FastText('timevalue', vFont, vPrimaryColor, vGroup, CenterRight);
  vText.Text := '0';
  FPanel.Add('timevalue', vText);
  FEngine.New.Formatter(vText,'panelrightside', [1]).Format;

  vText := FastText('collisions', vFont, vPrimaryColor, vGroup, CenterLeft);
  vText.Text := 'Collisions:';
  FPanel.Add('collisions', vText);
  FEngine.New.Formatter(vText,'panelleftside', [2]).Format;
  vText := FastText('collisionsvalue', vFont, vPrimaryColor, vGroup, CenterRight);
  vText.Text := '0';
  FPanel.Add('collisionsvalue', vText);
  FEngine.New.Formatter(vText,'panelrightside', [2]).Format;

  vText := FastText('score', vFont, vPrimaryColor, vGroup, CenterLeft);
  vText.Text := 'Score:';
  FPanel.Add('score', vText);
  FEngine.New.Formatter(vText,'panelleftside', [3]).Format;
  vText := FastText('scoresvalue', vFont, vPrimaryColor, vGroup, CenterRight);
  vText.Text := '0';
  FPanel.Add('scorevalue', vText);
  FEngine.New.Formatter(vText,'panelrightside', [3]).Format;

  for i := 0 to FPanel.Count - 1 do
    FPanel[i].Opacity := 0.5;

  FEngine.ShowGroup(vGroup);
end;

procedure TLoader.CreateStoryPanel(FPanel: TNamedList<tEngine2DText>);
var
  vText: TEngine2DText;
  vFont: TFont;
  vPrimaryColor, vSecondaryColor: TAlphaColor;
//  vLeft, vRight: String;
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
  FEngine.New.Formatter(vText,'panelleftside', [1]).Format;
  vText := FastText('levelvalue', vFont, vSecondaryColor, vGroup, CenterRight);
  vText.Text := '0';
  FPanel.Add('levelvalue', vText);
  FEngine.New.Formatter(vText,'panelrightside', [1]).Format;

  vText := FastText('time', vFont, vPrimaryColor, vGroup, CenterLeft);
  vText.Text := 'Time left:';
  FPanel.Add('time', vText);
  FEngine.New.Formatter(vText,'panelleftside', [2]).Format;
  vText := FastText('timevalue', vFont, vPrimaryColor, vGroup, CenterRight);
  vText.Text := '0';
  FPanel.Add('timevalue', vText);
  FEngine.New.Formatter(vText,'panelrightside', [2]).Format;

  for i := 0 to FPanel.Count - 1 do
    FPanel[i].Opacity := 0.5;

  FEngine.ShowGroup(vGroup);
end;

procedure TLoader.CreateSurvivalPanel(FPanel: TNamedList<tEngine2DText>);
var
  vText: TEngine2DText;
  vFont: TFont;
  vPrimaryColor, vSecondaryColor: TAlphaColor;
//  vLeft, vRight: String;
  i: Integer;
  vGroup: String;
begin
  vGroup := 'survival';
  ClearAndDestroyPanel(FPanel);
  vFont := TFont.Create;
  vFont.Style := [TFontStyle.fsBold];
  vFont.Size := 14;
  vPrimaryColor := TAlphaColorRec.White;
  vSecondaryColor := TAlphaColorRec.Lightgray;

  vText := FastText('time', vFont, vPrimaryColor, vGroup, CenterLeft);
  vText.Text := 'Time:';
  FPanel.Add('time', vText);
  FEngine.New.Formatter(vText,'panelleftside', [1]).Format;
  vText := FastText('timevalue', vFont, vPrimaryColor, vGroup, CenterRight);
  vText.Text := '0';
  FPanel.Add('timevalue', vText);
  FEngine.New.Formatter(vText, 'panelrightside', [1]).Format;

  vText := FastText('score', vFont, vPrimaryColor, vGroup, CenterLeft);
  vText.Text := 'Score:';
  FPanel.Add('score', vText);
  FEngine.New.Formatter(vText,'panelleftside', [2]).Format;
  vText := FastText('scoresvalue', vFont, vPrimaryColor, vGroup, CenterRight);
  vText.Text := '0';
  FPanel.Add('scorevalue', vText);
  FEngine.New.Formatter(vText, 'panelrightside', [2]).Format;

  for i := 0 to FPanel.Count - 1 do
    FPanel[i].Opacity := 0.5;

  FEngine.ShowGroup(vGroup);
end;

function TLoader.DefinedBigAsteroids(const ASize, ASpeed: Double): TAsteroid;
var
  vSpr: TAsteroid;
  vFigure: TNewFigure;
  vCircle: TCircle;
  vAng: Double;
begin
  vSpr := TAsteroid.Create(FEngine);
  vSpr.Parent := FEngine;
  vSpr.Resources := FEngine.Resources;
  vSpr.CurRes := 1;
  vSpr.Group := 'activeobject';
  vSpr.x := Random(FEngine.Width);
  vSpr.y := Random(FEngine.Height);
  vFigure := TNewFigure.Create(TNewFigure.cfCircle);
  vCircle.X := 0;
  vCircle.Y := 0;
  vCircle.Radius := 50;
  vFigure.SetData(vCircle);
  vSpr.Shape.AddFigure(vFigure);
  vSpr.ScaleMod := ASize; //RandomRange(60, 140) / 100;

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
  vSpr := TShip.Create(FEngine);
  vSpr.Resources := FEngine.Resources;
  vSpr.Group := 'ship';
  vSpr.x := 200;//Random(FEngine.Width);
  vSpr.y := 200;//Random(FEngine.Height);
  vSpr.Rotate := Random(360);

  vPoly1 := TNewFigure.CreatePoly;
  Clear(vPoly);
  AddPoint(vPoly, PointF(0, -165));
  AddPoint(vPoly, PointF(-50, -55));
  AddPoint(vPoly, PointF(50, -55));
  Scale(vPoly, PointF(0.9, 0.9));
  vPoly1.SetData(vPoly);

  vPoly2 := TNewFigure.CreatePoly;
  Clear(vPoly);
  AddPoint(vPoly, PointF(-95, -115));
  AddPoint(vPoly, PointF(-70, -85));
  AddPoint(vPoly, PointF(-55, -45));
  AddPoint(vPoly, PointF(-115, 20));
  Scale(vPoly, PointF(0.9, 0.9));
  vPoly2.SetData(vPoly);

  vPoly3 := TNewFigure.CreatePoly;
  Clear(vPoly);
  AddPoint(vPoly, PointF(95, -115));
  AddPoint(vPoly, PointF(70, -85));
  AddPoint(vPoly, PointF(55, -45));
  AddPoint(vPoly, PointF(115, 20));
  Scale(vPoly, PointF(0.9, 0.9));
  vPoly3.SetData(vPoly);

  vShape := TNewFigure.CreateCircle;// TCircleFigure.Create;
  vCircle.X := 0;
  vCircle.Y := 50;
  vCircle.Radius := 115;
  vShape.SetData(vCircle);

  vSpr.Shape.AddFigure(vPoly1);
  vSpr.Shape.AddFigure(vPoly2);
  vSpr.Shape.AddFigure(vPoly3);
  vSpr.Shape.AddFigure(vShape);
  vSpr.Visible := False;
  FEngine.AddObject(vSpr, 'ship'); // Добавлять можно только так спрайты

  Result := vSpr;
end;

function TLoader.Explosion(const AX, AY, AAng: Double): TExplosion;
var
  vSpr: TExplosion;
begin
  vSpr := TExplosion.Create(FEngine);
  vSpr.Parent := FEngine;
  vSpr.Resources := FEngine.Resources;
  vSpr.CurRes := 9;
  vSpr.Group := 'activeobject';
  vSpr.x := AX;
  vSpr.y := AY;
  vSpr.Rotate := AAng;
  vSpr.Scale := 0.5;
  FEngine.AddObject(vSpr); // Добавлять можно только так спрайты

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
  vRes.Parent := fEngine;
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
  Result := TEngine2DText.Create(FEngine);
  Result.Group := AGroup;
  Result.Font := AFont;
  Result.Color := AColor;
  Result.Justify := AJustify;
  Result.TextRect := RectF(-50, -7, 50, 7);
  FEngine.AddObject(Result, AName);
end;

function TLoader.Formatter(const ASubject: tEngine2DObject;
  const AText: String): TEngineFormatter;
begin
  Result := TEngineFormatter.Create(ASubject);
  Result.Text := AText;
  FEngine.FormatterList.Insert(0, Result);
end;

function TLoader.OpacityAnimation(ASubject: TSprite; const AEndOpacity: Double): TOpacityAnimation;
var
  vRes: TOpacityAnimation;
begin
  vRes := TOpacityAnimation.Create;
  vRes.Parent := fEngine;
  vRes.EndOpaque := AEndOpacity;
  vRes.TimeTotal := 800;
  vRes.Subject := ASubject;

  Result := vRes;
end;

function TLoader.RandomAstroid: TLittleAsteroid;
var
  vSpr: TLittleAsteroid;
begin
  vSpr := TLittleAsteroid.Create(FEngine);
  vSpr.Parent := FEngine;
  vSpr.Resources := FEngine.Resources;
  vSpr.CurRes := vSpr.Tip + 5;
  vSpr.Group := 'backobjects';
  vSpr.x := Random(FEngine.Width);
  vSpr.y := Random(FEngine.Height);
  vSpr.Rotate := Random(360);
  //vSpr.Scale := 0.3;
  FEngine.AddObject(vSpr); // Добавлять можно только так спрайты

  Result := vSpr;
end;

function TLoader.BreakLifeAnimation(ASubject: TSprite): TOpacityAnimation;
var
  vRes: TOpacityAnimation;
begin
  vRes := TOpacityAnimation.Create;
  vRes.Parent := fEngine;
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
  vRes.Parent := FEngine;
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
  vRes.Parent := fEngine;
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
  FEngine.AnimationList.Add(vRes);
  (*for i := 0 to ASubject.Parts.Count - 1 do
  begin
  {  vRes := TOpacityAnimation.Create;
    vRes.Parent := fEngine;
    vRes.Subject := ASubject;
    vRes.EndOpaque := 0;
    vRes.StartOpaque := ASubject.Parts[i].Opacity;
    vRes.TimeTotal := 500;
    vRes.OnDestroy := ASubject.Hide;   }

   { ScaleAnimation(ASubject.Parts[i], )
    vRes := TSAnimation.Create;
    vRes.Parent := fEngine;
    vRes.Subject := ASubject;
    vRes.EndOpaque := 0;
    vRes.StartOpaque := ASubject.Parts[i].Opacity;
    vRes.TimeTotal := 500;
    vRes.OnDestroy := ASubject.Hide;

    FEngine.AnimationList.Add(vRes); }
  end;*)
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

function TLoader.Sprite(const AResource, AGroup, AName: string): TSprite;
begin
  Result := TSprite.Create(FEngine);
  Result.Resources := FEngine.Resources;
  if AResource <> '' then
    Result.CurRes := FEngine.Resources.IndexOf(AResource);
  Result.Group := AGroup;
  FEngine.AddObject(Result, AName);
end;

end.
