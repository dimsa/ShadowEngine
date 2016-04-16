unit uDemoMenu;

interface

uses
  System.Types, System.UITypes, System.Math,  System.Generics.Collections,
  {$I 'Utils\DelphiCompatability.inc'}
  System.SysUtils, System.Classes, FMX.Dialogs,
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
    FOnClick: TNotifyEvent;
    FGroup: string;
    function GetText: String;
    procedure SetText(const Value: String);
    procedure SetOnClick(const Value: TNotifyEvent);
    procedure MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure SetGroup(const Value: string);
    function GetFontSize: Single;
    procedure SetFontSize(const Value: Single);
  public
    property BackSprite: TButtonBack read FBack;
    property FontSize: Single read GetFontSize write SetFontSize;
    property OnClick: TNotifyEvent read FOnClick write SetOnClick;
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
    function GetOnNo: TNotifyEvent;
    function GetOnYes: TNotifyEvent;
    procedure SetOnNo(const Value: TNotifyEvent);
    procedure SetOnYes(const Value: TNotifyEvent);
  public
    property OnYes: TNotifyEvent read GetOnYes write SetOnYes;
    property OnNo: TNotifyEvent read GetOnNo write SetOnNo;
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
    FComixText1, FComixText2: TEngine2DText;
    FNextLevelMenu, FRetryLevelMenu: TYesNoMenu;
    // Меню выбора уровня
    FLevelMenu: TList<TGameButton>; // Кнопки выбора уровня
    FNextPage, FPrevPage: TGameButton; // Листалка страниц выбора уровня
    FCurPage: Integer; // Текущая страница выбора уровня
    FDoLevelSelect: TNotifyEvent;
    procedure CreateMenu1; // Первое меню из четырех кнопок
    procedure CreateMenu2; // Выбор типа игры
    procedure CreateMenu3; // Выбор уровня в сторимоде
    procedure CreateAbout; // Создает Эбаут сооотвественно
    procedure DoNothing(ASender: TObject);
    procedure CreateStatistics; // Создаёт статистику
    procedure NextLevelPage(ASender: TObject);
    procedure PrevLevelPage(ASender: TObject);
    procedure SetAboutGame(const Value: TNotifyEvent);
    procedure SetExitGame(const Value: TNotifyEvent);
    procedure SetStartGame(const Value: TNotifyEvent);
    procedure SetStatGame(const Value: TNotifyEvent);
    procedure SetRelaxMode(const Value: TNotifyEvent);
    procedure SetStoryMode(const Value: TNotifyEvent);
    procedure SetSurvivalMode(const Value: TNotifyEvent);
    procedure SetLevelSelect(const Value: TNotifyEvent);
    procedure SetNextLevelNo(const Value: TNotifyEvent);
    procedure SetNextLevelYes(const Value: TNotifyEvent);
    procedure SetRetryLevelYes(const Value: TNotifyEvent);
    procedure SetAstroidCount(const Value: Integer);
    procedure SetSecondsToFly(const Value: Integer);
  public
    property StartGame: TNotifyEvent write SetStartGame;
    property AboutGame: TNotifyEvent write SetAboutGame;
    property StatGame: TNotifyEvent write SetStatGame;
    property ExitGame: TNotifyEvent write SetExitGame;
    property RelaxMode: TNotifyEvent write SetRelaxMode;
    property StoryMode: TNotifyEvent write SetStoryMode;
    property LevelSelect: TNotifyEvent write SetLevelSelect;
    property SurvivalMode: TNotifyEvent write SetSurvivalMode;
    property OnNextLevelYes: TNotifyEvent write SetNextLevelYes;
    property OnRetryLevelYes: TNotifyEvent write SetRetryLevelYes;
    property OnNextLevelNo: TNotifyEvent write SetNextLevelNo;
    property ComixText1: TEngine2DText read FComixText1 write FComixText1; // Текст комиксов
    property ComixText2: TEngine2DText read FComixText2 write FComixText2;
    property AsteroidCount: Integer write SetAstroidCount;
    property SecondsToFly: Integer write SetSecondsToFly;
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

constructor TGameButton.Create(const AName: string; const AParent: Pointer; const ASpriteBackName: string = 'button');
var
  vEngine: tEngine2d;
  vFText: string;
  vPoly: TPolygon;
  vFigure: TNewFigure;
begin
  FParent := AParent;
  FName := AName;

  vEngine := AParent;
  FBack := TButtonBack.Create;
  FBack.Resources := vEngine.Resources;
  FBack.CurRes := vEngine.Resources.IndexOf(ASpriteBackName);

  FText := TEngine2DText.Create;
  FText.TextRect := RectF(-125, -50, 125, 50);
  FText.FontSize := 32;
  FText.Color := TAlphaColorRec.White;

  FText.Justify := Center;

  FBack.OnMouseDown := MouseDown;

  vEngine.AddObject(FBack, FName);
  vEngine.AddObject(FText);
  FBack.Link := FText;

  vFText := 'left: ' + Self.FName + '.left; top: ' + Self.FName + '.top; min-width:'  + Self.FName + '.width * 0.8; width: ' + Self.FName + '.width; max-width: ' + Self.FName + '.width;';
  vEngine.New.Formatter(FText, vFText).Format;

  vPoly := PolyFromRect(RectF(0, 0, FBack.w, FBack.h));
  Translate(vPoly, -PointF(FBack.wHalf, FBack.hHalf));
  vFigure := TNewFigure.CreatePoly;
  vFigure.SetData(vPoly);
  FBack.Shape.AddFigure(vFigure);

 // vFormatter.Format;
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

procedure TGameButton.SetOnClick(const Value: TNotifyEvent);
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
begin
  FParent := AParent;
  vEngine := FParent;
  FCurPage := 0;

  FGameLogo := TSprite.Create;
  FGameLogo.Resources := vEngine.Resources;
  FGameLogo.Group := 'menu';
  FGameLogo.CurRes := vEngine.Resources.IndexOf('gamelogo');
  vEngine.AddObject(FGameLogo, 'gamelogo');

  vEngine.New.Formatter(FGameLogo, 'gamelogo', []).Format;

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
  vEngine: tEngine2d;
begin
  vEngine := FParent;
  vText := TEngine2DText.Create;
  vText.TextRect := RectF(-250, -100, 250, 100);
  vText.FontSize := 28;
  vText.Group := 'about';
  vText.Color :=  TAlphaColorRec.White;
  vText.Text :=
    'Asteroids vs You' + #13 +
    'ver. 0.8.2' + #13 + #13 +
    'Game about confrontation of' + #13 + 'Humankind and Asteroids';
  vText.Group := 'about';
  vEngine.AddObject(vText, 'aboutcaption');
  vEngine.New.Formatter(vText, 'about1', []).Format;

  vText := TEngine2DText.Create;
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

  vEngine.New.Formatter(vText, 'about2', []).Format;
  vEngine.HideGroup('about');
end;

procedure TGameMenu.CreateMenu1;
var
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

    vEngine.New.Formatter(vBut.BackSprite, 'menubuttons', [i], 0)
  end;
end;

procedure TGameMenu.CreateMenu2;
var
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

    vEngine.New.Formatter(vBut.BackSprite, 'menubuttons', [i], 0)
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

  vEngine.New.Formatter(FPrevPage.FBack, 'levelmenu', [0,4], 0).Format;
  vEngine.New.Formatter(FNextPage.FBack, 'levelmenu', [3,4], 0).Format;

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
      vEngine.New.Formatter(vBut.BackSprite, 'levelmenu', [vX,vY], 0).Format;
    end;

  vLoader.Free;
end;

procedure TGameMenu.CreateStatistics;
var
  vText: TEngine2DText;
  vEngine: tEngine2d;
begin
  vEngine := FParent;
  vText := TEngine2DText.Create;
  vText.TextRect := RectF(-200, -200, 200, 200);
  vText.FontSize := 26;
  vText.Group := 'statistics';
  vText.Color :=  TAlphaColorRec.White;
  vText.Text := 'Statistics in progress';
  vEngine.AddObject(vText, 'statisticscaption');
  vEngine.New.Formatter(vText, 'statistics', []).Format;

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

procedure TGameMenu.SetAboutGame(const Value: TNotifyEvent);
begin
  FList[2].OnClick := Value;
end;

procedure TGameMenu.SetAstroidCount(const Value: Integer);
begin
  FComixText2.Text := 'Usually asteroids are at a very long distance from each other. But here are ' + IntToStr(Value) + ' of them within easy reach! And they are moving towards me at the different speed!'
end;

procedure TGameMenu.SetExitGame(const Value: TNotifyEvent);
begin
  FList[3].OnClick := Value;
end;

procedure TGameMenu.SetLevelSelect(const Value: TNotifyEvent);
var
  i: Integer;
begin
  for i := 0 to FLevelMenu.Count -1 do
    Self.FLevelMenu[i].OnClick := Value;

  FDoLevelSelect := Value;
  //FList[4].OnClick := Value;
end;

procedure TGameMenu.SetNextLevelNo(const Value: TNotifyEvent);
begin
  FNextLevelMenu.OnNo := Value;
  FRetryLevelMenu.OnNo := Value;
end;

procedure TGameMenu.SetNextLevelYes(const Value: TNotifyEvent);
begin
  FNextLevelMenu.OnYes := Value;
end;

procedure TGameMenu.SetRelaxMode(const Value: TNotifyEvent);
begin
  FList[6].OnClick := Value;
end;

procedure TGameMenu.SetRetryLevelYes(const Value: TNotifyEvent);
begin
  FRetryLevelMenu.OnYes := Value;
end;

procedure TGameMenu.SetSecondsToFly(const Value: Integer);
begin
  FComixText1.Text := 'Great! I so close to the destination planet! I need only ' + IntToStr(Value) +  ' seconds to reach it.';
end;

procedure TGameMenu.SetStartGame(const Value: TNotifyEvent);
begin
  FList[0].OnClick := Value;
end;

procedure TGameMenu.SetStatGame(const Value: TNotifyEvent);
begin
  FList[1].OnClick := Value;
end;

procedure TGameMenu.SetStoryMode(const Value: TNotifyEvent);
begin
  FList[4].OnClick := Value;
end;

procedure TGameMenu.SetSurvivalMode(const Value: TNotifyEvent);
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
  vBut: TGameButton;
  vGroup: string;
begin
  FEngine := AParent;
  vEngine := FEngine;
  FId := AId;
  vGroup := AGroup;//'nextlevel';

  FBack := TFillRect.Create;
  FBack.Group := vGroup;
  FBack.FigureRect := RectF(-150, -100, 150, 100);
  FBack.Pen.Thickness := 4;
  FBack.Pen.Color := RGBColor(62 , 6, 30, 255).Color;
  FBack.Brush.Color := TAlphaColorRec.White;
  vEngine.AddObject(FBack, FId + 'back');

  vEngine.New.Formatter(FBack, 'yesnoback', [], 0).Format;

  vBut := TGameButton.Create(FId + 'yes', vEngine);
  vBut.Text := 'Yes';
  vBut.FontSize := 64;
  vBut.BackSprite.CurRes := vEngine.Resources.IndexOf('ltlbutenabled');
  vBut.Group := vGroup;
  FYes := vBut;

  vEngine.New.Formatter(vBut.BackSprite, 'yesnoyes', [FId], 1).Format;

  vBut := TGameButton.Create(FId + 'no', vEngine);
  vBut.Text := 'No';
  vBut.FontSize := 64;
  vBut.BackSprite.CurRes := vEngine.Resources.IndexOf('ltlbutenabled');
  vBut.Group := vGroup;
  FNo := vBut;

  vEngine.New.Formatter(vBut.BackSprite, 'yesnono', [FId], 1).Format;

  FText := TEngine2DText.Create;
  FText.Group := vGroup;
  FText.WordWrap := True;
  FText.TextRect :=  RectF(-90, -50, 90, 50);
  FText.FontSize := 14;
  FText.Color := RGBColor(62 , 6, 30, 255).Color;
  FText.Text := '';
  vEngine.AddObject(FText);

  vEngine.New.Formatter(FText, 'yesnotext', [FID]).Format;

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

function TYesNoMenu.GetOnNo: TNotifyEvent;
begin
  Result := FNo.OnClick;
end;

function TYesNoMenu.GetOnYes: TNotifyEvent;
begin
  Result := FYes.OnClick;
end;

function TYesNoMenu.GetText: string;
begin
  Result := FText.Text;
end;

procedure TYesNoMenu.SetOnNo(const Value: TNotifyEvent);
begin
  FNo.OnClick := Value;
end;

procedure TYesNoMenu.SetOnYes(const Value: TNotifyEvent);
begin
  FYes.OnClick := Value;
end;

procedure TYesNoMenu.SetText(const Value: string);
begin
  FText.Text := Value;
end;

end.


