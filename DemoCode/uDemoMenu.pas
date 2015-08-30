unit uDemoMenu;

interface

uses
  System.Types, System.UITypes, System.Math,  System.Generics.Collections,
  {$IFDEF VER290} System.Math.Vectors, {$ENDIF} System.SysUtils, System.Classes,
  FMX.Dialogs,
  uEngine2DObject, uEngine2DText, uEngine2DSprite, uEngineFormatter, uEngine2DShape,
  uIntersectorMethods, uClasses, uEngine2DClasses;

type

  TButtonBack = class(TSprite)
  private
    FLink: TEngine2DText;
  public
    property Link: TEngine2DText read FLink write FLink;
  end;

  TGameButton = class
  private
    FParent: Pointer; // Engine2d
    FBack: TButtonBack;
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
    property BackSprite: TButtonBack read FBack;
    property FontSize: Single read GetFontSize write SetFontSize;
    property OnClick: TVCLProcedure read FOnClick write SetOnClick;
    property Group: string read FGroup write SetGroup;
    constructor Create(const AName: string; const AParent: Pointer; const ASpriteBackName: string = 'button');
    destructor Destroy; override;
    procedure SendToFront;
    property Text: String read GetText write SetText;
    property Name: String read FName write FName;
  end;

  TYesNoMenu = class
  private
    FEngine: Pointer;
    FYes, FNo: TGameButton;
    FText: TEngine2DText;
    FBack: TEngine2DShape;
    FId: string; // Если используется несколько меню, то это идентификатор
    function GetText: string;
    procedure SetText(const Value: string);
    function GetOnNo: TVCLProcedure;
    function GetOnYes: TVCLProcedure;
    procedure SetOnNo(const Value: TVCLProcedure);
    procedure SetOnYes(const Value: TVCLProcedure);
  public
    property OnYes: TVCLProcedure read GetOnYes write SetOnYes;
    property OnNo: TVCLProcedure read GetOnNo write SetOnNo;
    property Text: string read GetText write SetText;
    constructor Create(const AId, AGroup: string; AParent: Pointer);
    destructor Destroy; override;
  end;

  TGameMenu = class
  private
    FParent: Pointer; // Engine2d
    FMaxLevel: Integer; // Максимальный уровень до которого дошел игрок
    FList: TList<TGameButton>; // Кнопки перехода по страницам меню
    FSelectLevel: TList<TGameButton>;
    FGameLogo: TSprite;
    FNextLevelMenu, FRetryLevelMenu: TYesNoMenu;
    // Меню выбора уровня
    FLevelMenu: TList<TGameButton>; // Кнопки выбора уровня
    FNextPage, FPrevPage: TGameButton; // Листалка страниц выбора уровня
    FCurPage: Integer; // Текущая страница выбора уровня
    FDoLevelSelect: TVCLProcedure;
    procedure CreateMenu1; // Первое меню из четырех кнопок
    procedure CreateMenu2; // Выбор типа игры
    procedure CreateMenu3; // Выбор уровня в сторимоде
    procedure CreateAbout; // Создает Эбаут сооотвественно
    procedure DoNothing(ASender: TObject);
    procedure CreateStatistics; // Создаёт статистику
    procedure NextLevelPage(ASender: TObject);
    procedure PrevLevelPage(ASender: TObject);
    procedure SetAboutGame(const Value: TVCLProcedure);
    procedure SetExitGame(const Value: TVCLProcedure);
    procedure SetStartGame(const Value: TVCLProcedure);
    procedure SetStatGame(const Value: TVCLProcedure);
    procedure SetRelaxMode(const Value: TVCLProcedure);
    procedure SetStoryMode(const Value: TVCLProcedure);
    procedure SetSurvivalMode(const Value: TVCLProcedure);
    procedure SetLevelSelect(const Value: TVCLProcedure);
    procedure SetNextLevelNo(const Value: TVCLProcedure);
    procedure SetNextLevelYes(const Value: TVCLProcedure);
    procedure SetRetryLevelYes(const Value: TVCLProcedure);
  public
    property StartGame: TVCLProcedure write SetStartGame;
    property AboutGame: TVCLProcedure write SetAboutGame;
    property StatGame: TVCLProcedure write SetStatGame;
    property ExitGame: TVCLProcedure write SetExitGame;
    property RelaxMode: TVCLProcedure write SetRelaxMode;
    property StoryMode: TVCLProcedure write SetStoryMode;
    property LevelSelect: TVCLProcedure write SetLevelSelect;
    property SurvivalMode: TVCLProcedure write SetSurvivalMode;
    property OnNextLevelYes: TVCLProcedure write SetNextLevelYes;
    property OnRetryLevelYes: TVCLProcedure write SetRetryLevelYes;
    property OnNextLevelNo: TVCLProcedure write SetNextLevelNo;
    procedure ShowLevels(const AMaxLevel: Integer; const APage: Integer = -1);
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
  FBack := TButtonBack.Create(vEngine);
  FBack.Parent := vEngine;
  FBack.Resources := vEngine.Resources;
  FBack.CurRes := vEngine.Resources.IndexOf(ASpriteBackName);

  FText := TEngine2DText.Create(vEngine);
  FText.TextRect := RectF(-125, -50, 125, 50);
  FText.FontSize := 32;
  FText.Color := TAlphaColorRec.White;

  FText.Justify := Center;

  FBack.OnMouseDown := MouseDown;

  vEngine.AddObject(FBack, FName);
  vEngine.AddObject(FText);
  FBack.Link := FText;

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
  Result := FText.FontSize;
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
  tEngine2d(FParent).AnimationList.ClearAndRecoverForSubject(FBack);
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
  vEngine: tEngine2d;
  vFormatter: TEngineFormatter;
begin
  FParent := AParent;
  vEngine := FParent;
  FCurPage := 0;

  FGameLogo := TSprite.Create(vEngine);
  FGameLogo.Resources := vEngine.Resources;
  FGameLogo.Group := 'menu';
  FGameLogo.CurRes := vEngine.Resources.IndexOf('gamelogo');
  vEngine.AddObject(FGameLogo, 'gamelogo');

  vFormatter := TEngineFormatter.Create(FGameLogo);
  vFormatter.Text := 'left: engine.width * 0.5; top: engine.height * 0.21; width: engine.width * 0.8; max-height: engine.height * 0.30;' +
    'xifhor: engine.width * 0.25; widthifhor: engine.width * 0.4; yifhor: engine.height * 0.5;';
  vEngine.FormatterList.Add(vFormatter);

  FNextLevelMenu := TYesNoMenu.Create('nextlevel', 'nextlevel', FParent);
  FNextLevelMenu.Text :=
    'Congratulations!' + #13 +
    'You have completed level!' + #13 + #13 +
    'Play Next Level?';
  FRetryLevelMenu := TYesNoMenu.Create('retrylevel', 'retrylevel', FParent);
  FRetryLevelMenu.Text :=
    'Regretulations! :-(' + #13 +
    'Your ship is destroyed!' + #13 + #13 +
    'Retry This Level?';

  FList := TList<TGameButton>.Create;
  FLevelMenu := TList<TGameButton>.Create;

  CreateMenu1;
  CreateMenu2;
  CreateMenu3;
  FNextPage.OnClick := NextLevelPage;
  FPrevPage.OnClick := PrevLevelPage;
  CreateAbout;
  CreateStatistics;


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
  vEngine.AddObject(vText, 'aboutcaption');
  vFormatter := TEngineFormatter.Create(vText);
  vFormatter.Text := 'left: engine.width * 0.5; top: gamelogo.bottomborder + engine.height * 0.15; width: engine.width * 0.8;' +
    'max-height: engine.height * 0.30;' +
    'topifhor: gamelogo.topborder; leftifhor: engine.width*0.75; wifhor: engine.width*0.4; maxheightifhor: engine.height * 0.4';
  vEngine.FormatterList.Add(vFormatter);
  vFormatter.Format;

  vText := TEngine2DText.Create(vEngine);
  vText.TextRect := RectF(-250, -150, 250, 150);
  vText.FontSize := 16;
  vText.Color :=  TAlphaColorRec.Gray;
  vText.Text :=
  'It''s opensource project' + #13 +
  'Written with Delphi, using Firemonkey ' + #13 +
  'Game uses ShadowEngine (SO Engine) by Dmitriy Sorokin' + #13 + #13 +
  'Project on GitHub: ' + #13 +
  'https://github.com/dimsa/ShadowEngine' +  #13 + #13 +
  'Game uses FMX.IniFile by HOSOKAWA' + #13 +
  'Project on GitHub:' + #13 +
  'https://github.com/freeonterminate/delphi/tree/master/FMX.IniFile' + #13 + #13 +
  'Some illustrations made by Yunna Sorokina' + #13 + #13 +
  'Thanks to everyone who helped with Game and Engine!';
  vText.Group := 'about';
  vEngine.AddObject(vText, 'aboutdescription');

  vFormatter := TEngineFormatter.Create(vText);
  vFormatter.Text := 'left: engine.width * 0.5; top: aboutcaption.bottomborder + engine.height * 0.15; width: engine.width * 0.8;' +
  'leftifhor: engine.width*0.75; wifhor: engine.width*0.4; maxheightifhor: engine.height * 0.4; ';
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
  i, vN: Integer;
  vBut: TGameButton;
  vX, vY: Integer;
  vEngine: tEngine2d;
  vLoader: TLoader;
begin
  vEngine := FParent;
  vLoader := TLoader.Create(vEngine);

  FMaxLevel := 1;

  //FLevelPage := Trunc((FMaxLevel + 1) / 16);

  FNextPage := TGameButton.Create('nextpagebut', vEngine);
  FNextPage.Text := 'Next';
  FNextPage.Group := 'menu3';
  FNextPage.FontSize := 80;

  if FCurPage <  Trunc((FMaxLevel + 1) / 16) then
    FNextPage.FBack.CurRes := vEngine.Resources.IndexOf('ltlbutenabled')
  else
    FNextPage.FBack.CurRes := vEngine.Resources.IndexOf('ltlbutdisabled') ;

  FPrevPage := TGameButton.Create('prevpagebut', vEngine);
  FPrevPage.Text := 'Prev';
  FPrevPage.Group := 'menu3';
  FPrevPage.FontSize := 80;

 if FCurPage > 0 then
    FPrevPage.FBack.CurRes := vEngine.Resources.IndexOf('ltlbutenabled')
  else
    FPrevPage.FBack.CurRes := vEngine.Resources.IndexOf('ltlbutdisabled');

  vLoader.Formatter(FPrevPage.FBack, vLoader.LevelFormatText(0, 4)).Format;
  vLoader.Formatter(FNextPage.FBack, vLoader.LevelFormatText(3, 4)).Format;

  vN := 16;

    for i := 0 to vN - 1 do
    begin
      vBut := TGameButton.Create('levelbut' + IntToStr(i + 1), vEngine, 'ltlbutenabled');
      vBut.Text := IntToStr((i + 1) + (vN * (FCurPage)));
      vBut.FontSize := 80;
      vBut.Group := 'menu3';
      FLevelMenu.Add(vBut);
      vX := i mod 4;
      vY := Trunc(i / 4);
      vLoader.Formatter(vBut.BackSprite, vLoader.LevelFormatText(vX, vY)).Format;
    end;

  vLoader.Free;
end;

procedure TGameMenu.CreateStatistics;
var
  vText: TEngine2DText;
  vFormatter: TEngineFormatter;
  vEngine: tEngine2d;
begin
  vEngine := FParent;
  vText := TEngine2DText.Create(vEngine);
  vText.TextRect := RectF(-200, -200, 200, 200);
  vText.FontSize := 26;
  vText.Group := 'statistics';
  vText.Color :=  TAlphaColorRec.White;
  vText.Text := 'Statistics in progress';
  vEngine.AddObject(vText, 'statisticscaption');
  vFormatter := TEngineFormatter.Create(vText);
  vFormatter.Text := 'left: engine.width * 0.5; top: gamelogo.bottomborder + engine.height * 0.15; width: engine.width * 0.8;' +
  'max-height: engine.height * 0.40;' +
  'topifhor: gamelogo.top; leftifhor: engine.width*0.75; wifhor: engine.width*0.4; maxheightifhor: engine.height * 0.4';
  vEngine.FormatterList.Add(vFormatter);
  vFormatter.Format;
  vEngine.HideGroup('statistics');
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

procedure TGameMenu.DoNothing(ASender: TObject);
begin

end;

procedure TGameMenu.NextLevelPage(ASender: TObject);
var
  vEngine: tEngine2d;
begin
  vEngine := FParent;
  if TButtonBack(ASender).CurRes = vEngine.Resources.IndexOf('ltlbutenabled') then
    if FCurPage < Trunc(FMaxLevel / 16) then
      Inc(FCurPage);

   ShowLevels(FMaxLevel, FCurPage);
end;

procedure TGameMenu.PrevLevelPage(ASender: TObject);
var
  vEngine: tEngine2d;
begin
  vEngine := FParent;
  if TButtonBack(ASender).CurRes = vEngine.Resources.IndexOf('ltlbutenabled') then
    if FCurPage > 0 then
      Dec(FCurPage);

   ShowLevels(FMaxLevel, FCurPage);
end;

procedure TGameMenu.SendToFront;
var
  vBut: TGameButton;
begin
  FGameLogo.SendToFront;
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
var
  i: Integer;
begin
  for i := 0 to FLevelMenu.Count -1 do
    Self.FLevelMenu[i].OnClick := Value;

  FDoLevelSelect := Value;
  //FList[4].OnClick := Value;
end;

procedure TGameMenu.SetNextLevelNo(const Value: TVCLProcedure);
begin
  FNextLevelMenu.OnNo := Value;
  FRetryLevelMenu.OnNo := Value;
end;

procedure TGameMenu.SetNextLevelYes(const Value: TVCLProcedure);
begin
  FNextLevelMenu.OnYes := Value;
end;

procedure TGameMenu.SetRelaxMode(const Value: TVCLProcedure);
begin
  FList[6].OnClick := Value;
end;

procedure TGameMenu.SetRetryLevelYes(const Value: TVCLProcedure);
begin
  FRetryLevelMenu.OnYes := Value;
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

procedure TGameMenu.ShowLevels(const AMaxLevel, APage: Integer);
var
  vEngine: tEngine2d;
  i: Integer;
begin
  vEngine := FParent;

  FMaxLevel := AMaxLevel;
  if APage = -1 then
    FCurPage := Trunc(AMaxLevel / 16);


  if FCurPage <  Trunc((FMaxLevel + 1) / 16) then
    FNextPage.FBack.CurRes := vEngine.Resources.IndexOf('ltlbutenabled')
  else
  begin
    FNextPage.FBack.CurRes := vEngine.Resources.IndexOf('ltlbutdisabled') ;
 //   FNextPage.OnClick := DoNothing;
  end;

  if FCurPage > 0 then
    FPrevPage.FBack.CurRes := vEngine.Resources.IndexOf('ltlbutenabled')
  else
  begin
    FPrevPage.FBack.CurRes := vEngine.Resources.IndexOf('ltlbutdisabled');
  //  FPrevPage.OnClick := DoNothing;
  end;

    for i := 0 to 16 - 1 do
    begin
      FLevelMenu[i].Text := IntToStr((i + 1) + (16 * (FCurPage)));
      if (i + FCurPage * 16) < AMaxLevel+1 then
      begin
        FLevelMenu[i].BackSprite.CurRes := vEngine.Resources.IndexOf('ltlbutenabled');
        FLevelMenu[i].BackSprite.OnClick := FDoLevelSelect;
      end
      else
      begin
        FLevelMenu[i].BackSprite.CurRes := vEngine.Resources.IndexOf('ltlbutdisabled');
        FLevelMenu[i].BackSprite.OnClick := DoNothing;
      end;
    end;
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

{ TYesNoMenu }

constructor TYesNoMenu.Create(const AId, AGroup: string; AParent: Pointer);
var
  vEngine: tEngine2d;
  vLoader: TLoader;
  vBut: TGameButton;
  vFormatter: TEngineFormatter;
  vGroup: string;
begin
  FEngine := AParent;
  vEngine := FEngine;
  FId := AId;
  vGroup := AGroup;//'nextlevel';

  FBack := TFillRect.Create(vEngine);
  FBack.Group := vGroup;
  FBack.FigureRect := RectF(-150, -100, 150, 100);
  FBack.Pen.Thickness := 4;
  FBack.Pen.Color := RGBColor(62 , 6, 30, 255).Color;
  FBack.Brush.Color := TAlphaColorRec.White;
  vEngine.AddObject(FBack, FId + 'back');

  vFormatter := TEngineFormatter.Create(FBack);
  vFormatter.Text := 'width: engine.width * 0.6;  left: engine.width * 0.5; top: engine.height * 0.5;';
  vEngine.FormatterList.Insert(0, vFormatter);
  vFormatter.Format;

  vBut := TGameButton.Create(FId + 'yes', vEngine);
  vBut.Text := 'Yes';
  vBut.FontSize := 64;
  vBut.BackSprite.CurRes := vEngine.Resources.IndexOf('ltlbutenabled');
  vBut.Group := vGroup;
  FYes := vBut;

  vFormatter := TEngineFormatter.Create(vBut.BackSprite);
  vFormatter.Text := 'width: engine.width * 0.2;  left: engine.width * 0.5 + width*0.75;' +
    'top: engine.height * 0.5 + ' + FId + 'back.height * 0.35; max-height: ' + FId + 'back.height * 0.225;';
  vEngine.FormatterList.Insert(0, vFormatter);
  vFormatter.Format;

  vBut := TGameButton.Create(FId + 'no', vEngine);
  vBut.Text := 'No';
  vBut.FontSize := 64;
  vBut.BackSprite.CurRes := vEngine.Resources.IndexOf('ltlbutenabled');
  vBut.Group := vGroup;
  FNo := vBut;

  vFormatter := TEngineFormatter.Create(vBut.BackSprite);
  vFormatter.Text := 'width: engine.width * 0.2;  left: engine.width * 0.5 - width*0.75;' +
    'top: engine.height * 0.5 + ' + FId + 'back.height * 0.35; max-height: ' + FId + 'back.height * 0.225;';
  vEngine.FormatterList.Insert(0, vFormatter);
  vFormatter.Format;

  FText := TEngine2DText.Create(vEngine);
  FText.Group := vGroup;
  FText.WordWrap := True;
  FText.TextRect :=  RectF(-90, -50, 90, 50);
  FText.FontSize := 14;
  FText.Color := RGBColor(62 , 6, 30, 255).Color;
  FText.Text := '';
  vEngine.AddObject(FText);

  vFormatter := TEngineFormatter.Create(FText);
  vFormatter.Text := 'width: ' + FId + 'back.width * 0.8; left: ' + FId + 'back.left;' +
    'top: ' + FId + 'back.top - ' + FId + 'back.height * 0.20;';

  vEngine.FormatterList.Insert(0, vFormatter);
  vFormatter.Format;

  vEngine.HideGroup(vGroup);
end;

destructor TYesNoMenu.Destroy;
var
  vEngine: tEngine2d;
begin
  vEngine := FEngine;

  FYes.Free;
  FNo.Free;

  vEngine.DeleteObject(FBack);
  FBack.Free;

  vEngine.DeleteObject(FText);
  FText.Free;

  inherited;
end;

function TYesNoMenu.GetOnNo: TVCLProcedure;
begin
  Result := FNo.OnClick;
end;

function TYesNoMenu.GetOnYes: TVCLProcedure;
begin
  Result := FYes.OnClick;
end;

function TYesNoMenu.GetText: string;
begin
  Result := FText.Text;
end;

procedure TYesNoMenu.SetOnNo(const Value: TVCLProcedure);
begin
  FNo.OnClick := Value;
end;

procedure TYesNoMenu.SetOnYes(const Value: TVCLProcedure);
begin
  FYes.OnClick := Value;
end;

procedure TYesNoMenu.SetText(const Value: string);
begin
  FText.Text := Value;
end;

end.


