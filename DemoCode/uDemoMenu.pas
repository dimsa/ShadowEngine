unit uDemoMenu;

interface

uses
  System.Types, System.UITypes, System.Math,  System.Generics.Collections,
  {$IFDEF VER290} System.Math.Vectors, {$ENDIF} System.SysUtils, System.Classes,
  FMX.Dialogs,
  uEngine2DObject, uEngine2DText, uEngine2DSprite, uEngineFormatter,
  uIntersectorMethods, uClasses, uEngine2DClasses;

type

  TGameButton = class
  private
    FParent: Pointer; // Engine2d
    FBack: TSprite;
    FText: TEngine2DText;
    FName: String;
    FOnClick: TVCLProcedure;
    FGroup: string;
    function GetText: String;
    procedure SetText(const Value: String);
    procedure SetOnClick(const Value: TVCLProcedure);
    procedure MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure SetGroup(const Value: string);
    function GetFontSize: Single;
    procedure SetFontSize(const Value: Single);
  public
    property BackSprite: TSprite read FBack;
    property FontSize: Single read GetFontSize write SetFontSize;
    property OnClick: TVCLProcedure read FOnClick write SetOnClick;
    property Group: string read FGroup write SetGroup;
    constructor Create(const AName: string; const AParent: Pointer; const ASpriteBackName: string = 'button');
    destructor Destroy; override;
    procedure SendToFront;
    property Text: String read GetText write SetText;
    property Name: String read FName write FName;
  end;

  TGameMenu = class
  private
    FParent: Pointer; // Engine2d
    FMaxLevel: Integer; // Максимальный уровень до которого дошел игрок
    FList: TList<TGameButton>; // Кнопки перехода по страницам меню
    FStatGame: TVCLProcedure;
    FExitGame: TVCLProcedure;
    FAboutGame: TVCLProcedure;
    FStartGame: TVCLProcedure;
    FGameLogo: TSprite;
    // Меню выбора уровня
    FLevelMenu: TList<TGameButton>; // Кнопки выбора уровня
    FNextPage, FPrevPage: TGameButton; // Листалка страниц выбора уровня
    FLevelPage: Integer; // Текущая страница выбора уровня
    procedure CreateMenu1; // Первое меню из четырех кнопок
    procedure CreateMenu2; // Выбор типа игры
    procedure CreateMenu3; // Выбор уровня в сторимоде
    procedure CreateAbout; // Создает Эбаут сооотвественно
    procedure SetAboutGame(const Value: TVCLProcedure);
    procedure SetExitGame(const Value: TVCLProcedure);
    procedure SetStartGame(const Value: TVCLProcedure);
    procedure SetStatGame(const Value: TVCLProcedure);
    procedure SetRelaxMode(const Value: TVCLProcedure);
    procedure SetStoryMode(const Value: TVCLProcedure);
    procedure SetSurvivalMode(const Value: TVCLProcedure);
    procedure SetLevelSelect(const Value: TVCLProcedure);
  public
    property StartGame: TVCLProcedure write SetStartGame;
    property AboutGame: TVCLProcedure write SetAboutGame;
    property StatGame: TVCLProcedure write SetStatGame;
    property ExitGame: TVCLProcedure write SetExitGame;
    property RelaxMode: TVCLProcedure write SetRelaxMode;
    property StoryMode: TVCLProcedure write SetStoryMode;
    property LevelSelect: TVCLProcedure write SetLevelSelect;
    property SurvivalMode: TVCLProcedure write SetSurvivalMode;
    procedure SendToFront;
    procedure Add(const AButton: TGameButton);
    constructor Create(const AParent: Pointer);
    destructor Destroy; override;
  end;


implementation

uses
  uEngine2D, uEngine2DObjectShape, uNewFigure, uDemoGameLoader;

{ TGameButton }

{procedure TGameButton.AddToEngine(const AFormatterText: String);
var
  vEngine: tEngine2d;
  vFormatter: TEngineFormatter;
  vFigure: TNewFigure;
  vPoly: TPolygon;
  vFText: String;
begin
  vEngine := FParent;
  vFormatter := TEngineFormatter.Create(FBack);
  vFormatter.Parent := vEngine;
  vFText := AFormatterText; //''ft: engine.width * 0.5; top: engine.height * 0.2; width: engine.width * 0.5';
  vFormatter.Text := vFText;
  vEngine.FormatterList.Add(vFormatter);
end;}

constructor TGameButton.Create(const AName: string; const AParent: Pointer; const ASpriteBackName: string = 'button');
var
  vEngine: tEngine2d;
  vFormatter: TEngineFormatter;
  vFText: string;
  vPoly: TPolygon;
  vFigure: TNewFigure;
begin
  FParent := AParent;
  FName := AName;

  vEngine := AParent;
  FBack := TSprite.Create(vEngine);
  FBack.Parent := vEngine;
  FBack.Resources := vEngine.Resources;
  FBack.CurRes := vEngine.Resources.IndexOf(ASpriteBackName);

  FText := TEngine2DText.Create(vEngine);
  FText.TextRect := RectF(-125, -35, 125, 35);
  FText.FontSize := 32;
  FText.Color := TAlphaColorRec.White;

  FText.Justify := Center;

  FBack.OnMouseDown := MouseDown;

  vEngine.AddObject(FName, FBack);
  vEngine.AddObject(FText);

  vFormatter := TEngineFormatter.Create(FText);
  vFormatter.Parent := vEngine;
  vFText := 'left: ' + Self.FName + '.left; top: ' + Self.FName + '.top; min-width:'  + Self.FName + '.width * 0.8; width: ' + Self.FName + '.width; max-width: ' + Self.FName + '.width;';
  vFormatter.Text := vFText;
  vEngine.FormatterList.Add(vFormatter);

  vPoly := PolyFromRect(RectF(0, 0, FBack.w, FBack.h));
  Translate(vPoly, -PointF(FBack.wHalf, FBack.hHalf));
  vFigure := TNewFigure.CreatePoly;
  vFigure.SetData(vPoly);
  FBack.Shape.AddFigure(vFigure);

  vFormatter.Format;
end;

destructor TGameButton.Destroy;
var
  vEngine: tEngine2d;
begin
  vEngine := FParent;
  vEngine.DeleteObject(FText);
  FText.Free;
  vEngine.DeleteObject(FBack);
  FBack.Free;
  inherited;
end;

function TGameButton.GetFontSize: Single;
begin
  FText.FontSize;
end;

function TGameButton.GetText: String;
begin
  Result := FText.Text;
end;

procedure TGameButton.MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
var
  vLoader: TLoader;

begin
  vLoader := TLoader.Create(FParent);
  tEngine2d(FParent).AnimationList.ClearForSubject(FBack);
  tEngine2d(FParent).AnimationList.Add(
    vLoader.ButtonAnimation(FBack, FBack.ScaleX * 0.8)
  );

  tEngine2d(FParent).AnimationList.ClearForSubject(FText);
  tEngine2d(FParent).AnimationList.Add(
    vLoader.ButtonAnimation(FText, FText.ScaleX * 0.8)
  );
  vLoader.Free;
end;

procedure TGameButton.SendToFront;
begin
  FBack.SendToFront;
  FText.SendToFront;
end;

procedure TGameButton.SetFontSize(const Value: Single);
begin
  FText.FontSize := Value;
end;

procedure TGameButton.SetGroup(const Value: string);
begin
  FGroup := Value;
  FBack.Group := FGroup;
  FText.Group := FGroup;
end;

procedure TGameButton.SetOnClick(const Value: TVCLProcedure);
begin
  FOnClick := Value;
  FBack.OnClick := FOnClick;
//  FText.OnClick := FOnClick;
end;

procedure TGameButton.SetText(const Value: String);
begin
  Ftext.Text := Value;
end;

{ TGameMenu }

procedure TGameMenu.Add(const AButton: TGameButton);
begin
end;

constructor TGameMenu.Create(const AParent: Pointer);
var
  vBut: TGameButton;
  vEngine: tEngine2d;
  vFormatter: TEngineFormatter;
begin
  FParent := AParent;
  vEngine := FParent;
  FLevelPage := 1;

  FGameLogo := TSprite.Create(vEngine);
  FGameLogo.Resources := vEngine.Resources;
  FGameLogo.Group := 'menu';
  FGameLogo.CurRes := vEngine.Resources.IndexOf('gamelogo');
  vEngine.AddObject('gamelogo', FGameLogo);

  vFormatter := TEngineFormatter.Create(FGameLogo);
  vFormatter.Text := 'left: engine.width * 0.5; top: engine.height * 0.21; width: engine.width * 0.8; max-height: engine.height * 0.40;' +
  'xifhor: engine.width * 0.25; widthifhor: engine.width * 0.4; yifhor: engine.height * 0.5;';
  vEngine.FormatterList.Add(vFormatter);

  FList := TList<TGameButton>.Create;
  FLevelMenu := TList<TGameButton>.Create;
  CreateMenu1;
  CreateMenu2;
  CreateMenu3;
  CreateAbout;

end;

procedure TGameMenu.CreateAbout;
var
  vText: TEngine2DText;
  vFormatter: TEngineFormatter;
  vEngine: tEngine2d;
begin
  vEngine := FParent;
  vText := TEngine2DText.Create(vEngine);
  vText.TextRect := RectF(-250, -100, 250, 100);
  vText.FontSize := 28;
  vText.Group := 'about';
  vText.Color :=  TAlphaColorRec.White;
  vText.Text :=
    'Asteroids vs You' + #13 +
    'ver. 0.7 beta' + #13 + #13 +
    'Game about confrontation of' + #13 + 'Humankind and Asteroids';
  vText.Group := 'about';
  vEngine.AddObject('aboutcaption', vText);
  vFormatter := TEngineFormatter.Create(vText);
  vFormatter.Text := 'left: engine.width * 0.5; top: gamelogo.bottomborder + engine.height * 0.15; width: engine.width * 0.8;';
  vEngine.FormatterList.Add(vFormatter);
  vFormatter.Format;

  vText := TEngine2DText.Create(vEngine);
  vText.TextRect := RectF(-250, -100, 250, 100);
  vText.FontSize := 16;
  vText.Color :=  TAlphaColorRec.Gray;
  vText.Text :=
  'It''s opensource project ' + #13 +
  'Written with Delphi, using Firemonkey ' + #13 +
  'Game uses ShadowEngine (SO Engine) by Dmitriy Sorokin' + #13 + #13 +
  'Project on GitHub: ' + #13 +
  'https://github.com/dimsa/ShadowEngine' +  #13 + #13 +
  'Thanks to everyone who helped with Game and Engine!';
  vText.Group := 'about';
  vEngine.AddObject('aboutdescription', vText);

  vFormatter := TEngineFormatter.Create(vText);
  vFormatter.Text := 'left: engine.width * 0.5; top: aboutcaption.bottomborder + engine.height * 0.1; width: engine.width * 0.8;';
  vEngine.FormatterList.Add(vFormatter);
  vFormatter.Format;
  vEngine.HideGroup('about');
end;

procedure TGameMenu.CreateMenu1;
var
  vFormatter: TEngineFormatter;
  vEngine: tEngine2d;
  vBut: TGameButton;
  i: Integer;
begin
  vEngine := FParent;
  for i := 1 to 4 do
  begin
    vBut := TGameButton.Create('button' + IntToStr(i), vEngine);
    case i of
      1: vBut.Text := 'Start Game';
      2: vBut.Text := 'Statistics';
      3: vBut.Text := 'About';
      4: vBut.Text := 'Exit';
    end;
    vBut.Group := 'menu1';
    FList.Add(vBut);

    vFormatter := TEngineFormatter.Create(vBut.BackSprite);
    vFormatter.Text := 'left: engine.width * 0.5; top: engine.height * 0.36 + engine.height * 0.14 * ' +
      IntToStr(FList.Count) +'; width: engine.width * 0.8; max-height: engine.height * 0.12; max-width: 0 + gamelogo.width;' +
      'xifhor: engine.width * 0.75; widthifhor: engine.width * 0.4; yifhor: engine.height * 0.2 *' + IntToStr(FList.Count);
    vEngine.FormatterList.Insert(0, vFormatter);
  end;
end;

procedure TGameMenu.CreateMenu2;
var
  vFormatter: TEngineFormatter;
  vEngine: tEngine2d;
  vBut: TGameButton;
  i: Integer;
begin
  vEngine := FParent;
  for i := 1 to 3 do
  begin
    vBut := TGameButton.Create('button' + IntToStr(i + 4), vEngine);
    case i of
      1: vBut.Text := 'Story Mode';
      2: vBut.Text := 'Survival Mode';
      3: vBut.Text := 'Relax Mode';
    end;
    vBut.Group := 'menu2';
    FList.Add(vBut);

    vFormatter := TEngineFormatter.Create(vBut.BackSprite);
    vFormatter.Text := 'left: engine.width * 0.5; top: engine.height * 0.36 + engine.height * 0.14 * ' +
      IntToStr(i) +'; width: engine.width * 0.8; max-height: engine.height * 0.12; max-width: 0 + gamelogo.width;' +
      'xifhor: engine.width * 0.75; widthifhor: engine.width * 0.4; yifhor: engine.height * 0.2 *' + IntToStr(i);
    vEngine.FormatterList.Insert(0, vFormatter);
  end;
end;

procedure TGameMenu.CreateMenu3;
var
  vF: TextFile;
  i, j, vN: Integer;
  vBut: TGameButton;
  vX, vY: Integer;
  vEngine: tEngine2d;
  vLoader: TLoader;
begin
  vEngine := FParent;
  vLoader := TLoader.Create(vEngine);

  FMaxLevel := 1;

  FLevelPage := Trunc((FMaxLevel + 1) / 16);

  FNextPage := TGameButton.Create('nextpagebut', vEngine);
  FNextPage.Text := 'Next';
  FNextPage.Group := 'menu3';
  FNextPage.FontSize := 36;

  if FLevelPage <  Trunc((FMaxLevel + 1) / 16) then
    FNextPage.FBack.CurRes := vEngine.Resources.IndexOf('ltlbutenabled')
  else
    FNextPage.FBack.CurRes := vEngine.Resources.IndexOf('ltlbutdisabled') ;

  FPrevPage := TGameButton.Create('prevpagebut', vEngine);
  FPrevPage.Text := 'Prev';
  FPrevPage.Group := 'menu3';
  FPrevPage.FontSize := 36;
  if FLevelPage > 0 then
    FPrevPage.FBack.CurRes := vEngine.Resources.IndexOf('ltlbutenabled')
  else
    FPrevPage.FBack.CurRes := vEngine.Resources.IndexOf('ltlbutdisabled');

  vLoader.Formatter(FPrevPage.FBack, vLoader.LevelFormatText(0, 4)).Format;
  vLoader.Formatter(FNextPage.FBack, vLoader.LevelFormatText(3, 4)).Format;

  vN := 16;

    for i := 0 to vN - 1 do
    begin
      vBut := TGameButton.Create('levelbut' + IntToStr(i + 1), vEngine, 'ltlbutenabled');
      vBut.Text := IntToStr((i + 1) + (vN * (FLevelPage)));
      vBut.FontSize := 36;
      vBut.Group := 'menu3';
      FLevelMenu.Add(vBut);
      vX := i mod 4;
      vY := Trunc(i / 4);
      vLoader.Formatter(vBut.BackSprite, vLoader.LevelFormatText(vX, vY)).Format;
    end;

  vLoader.Free;
end;

destructor TGameMenu.Destroy;
var
  i, vN: Integer;
begin
 vN := FList.Count - 1;
  for i := 0 to vN do
    FList[i].Free;
  FList.Free;

  inherited;
end;

procedure TGameMenu.SendToFront;
var
  vBut: TGameButton;
begin
  for vBut in FList do
    vBut.SendToFront;
end;

procedure TGameMenu.SetAboutGame(const Value: TVCLProcedure);
begin
  FList[2].OnClick := Value;
end;

procedure TGameMenu.SetExitGame(const Value: TVCLProcedure);
begin
  FList[3].OnClick := Value;
end;

procedure TGameMenu.SetLevelSelect(const Value: TVCLProcedure);
begin
//  FList[4].OnClick := Value;
end;

procedure TGameMenu.SetRelaxMode(const Value: TVCLProcedure);
begin
  FList[6].OnClick := Value;
end;

procedure TGameMenu.SetStartGame(const Value: TVCLProcedure);
begin
  FList[0].OnClick := Value;
end;

procedure TGameMenu.SetStatGame(const Value: TVCLProcedure);
begin
  FList[1].OnClick := Value;
end;

procedure TGameMenu.SetStoryMode(const Value: TVCLProcedure);
begin
  FList[4].OnClick := Value;
end;

procedure TGameMenu.SetSurvivalMode(const Value: TVCLProcedure);
begin
  FList[5].OnClick := Value;
end;

{ TMenuSpriteElement }

{procedure TMenuSpriteElement.Repaint;
begin
  Self.SendToFront;
  inherited;
end; }

{ TMenuTextElement }

{procedure TMenuTextElement.Repaint;
begin
  Self.SendToFront;
  inherited;
end; }

end.


