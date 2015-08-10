unit uDemoMenu;

interface

uses
  System.Types, System.UITypes, System.Math,  System.Generics.Collections,
  {$IFDEF VER290} System.Math.Vectors, {$ENDIF} System.SysUtils,
  uEngine2DObject, uEngine2DText, uEngine2DSprite, uEngineFormatter,
  uIntersectorMethods, uClasses;

type
  TGameButton = class
  private
    FParent: Pointer; // Engine2d
    FBack: TSprite;
    FText: TEngine2DText;
    FName: String;
    FOnClick: TVCLProcedure;
    function GetText: String;
    procedure SetText(const Value: String);
    procedure SetOnClick(const Value: TVCLProcedure);
  public
    property BackSprite: TSprite read FBack;
    property OnClick: TVCLProcedure read FOnClick write SetOnClick;
    constructor Create(const AName: string; const AParent: Pointer);
    destructor Destroy; override;
    property Text: String read GetText write SetText;
    property Name: String read FName write FName;
  end;

  TGameMenu = class
  private
    FParent: Pointer; // Engine2d
    FList: TList<TGameButton>;
    FStatGame: TVCLProcedure;
    FExitGame: TVCLProcedure;
    FAboutGame: TVCLProcedure;
    FStartGame: TVCLProcedure;
    procedure SetAboutGame(const Value: TVCLProcedure);
    procedure SetExitGame(const Value: TVCLProcedure);
    procedure SetStartGame(const Value: TVCLProcedure);
    procedure SetStatGame(const Value: TVCLProcedure);
  public
    property StartGame: TVCLProcedure write SetStartGame;
    property AboutGame: TVCLProcedure write SetAboutGame;
    property StatGame: TVCLProcedure write SetStatGame;
    property ExitGame: TVCLProcedure write SetExitGame;
    procedure Add(const AButton: TGameButton);
    constructor Create(const AParent: Pointer);
    destructor Destroy; override;
  end;


implementation

uses
  uEngine2D, uEngine2DObjectShape, uNewFigure;

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

constructor TGameButton.Create(const AName: string; const AParent: Pointer);
var
  vEngine: tEngine2d;
  vFormatter: TEngineFormatter;
  vFText: string;
  vPoly: TPolygon;
  vFigure: TNewFigure;
begin
  vEngine := AParent;
  FParent := vEngine;
  Self.FName := AName;
  FBack := TSprite.Create(vEngine);
  FBack.Resources := vEngine.Resources;
  FBack.CurRes := vEngine.Resources.IndexOf('button');
  FBack.Group := 'menu';
  FText := TEngine2DText.Create(vEngine);
  FText.Group := 'menu';
  FText.Color := TAlphaColorRec.White;

  vEngine.AddObject(FName, FBack);
  vEngine.AddObject(FText);

  FBack.SendToFront;
  FText.SendToFront;

  vFormatter := TEngineFormatter.Create(FText);
  vFormatter.Parent := vEngine;
  vFText := 'left: ' + Self.FName + '.left; top: ' + Self.FName + '.top; min-width:'  + Self.FName + '.width * 1.5; width: ' + Self.FName + '.width; max-width: ' + Self.FName + '.width;';
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

function TGameButton.GetText: String;
begin
  Result := FText.Text;
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
  FList := TList<TGameButton>.Create;
 // FFormatterList := TList<TEngineFormatter>.Create;



  vBut := TGameButton.Create('button1', vEngine);
  vBut.Text := 'Start Game';
  vBut.OnClick := FStartGame;
  FList.Add(vBut);
  vFormatter := TEngineFormatter.Create(vBut.BackSprite);
  vFormatter.Text := 'left: engine.width * 0.5; top: engine.height * 0.2 * ' +
    IntToStr(FList.Count) + '; width: engine.width * 0.8; max-height: engine.height * 0.18;';
  vEngine.FormatterList.Insert(0, vFormatter);

  vBut := TGameButton.Create('button2', vEngine);
  vBut.Text := 'Statistics';
  vBut.OnClick := FStatGame;
  FList.Add(vBut);
  vFormatter := TEngineFormatter.Create(vBut.BackSprite);
  vFormatter.Text := 'left: engine.width * 0.5; top: engine.height * 0.2 * ' +
    IntToStr(FList.Count) +'; width: engine.width * 0.8; max-height: engine.height * 0.18;';
  vEngine.FormatterList.Insert(0, vFormatter);

  vBut := TGameButton.Create('button3', vEngine);
  vBut.Text := 'About';
  vBut.OnClick := FAboutGame;
  FList.Add(vBut);
  vFormatter := TEngineFormatter.Create(vBut.BackSprite);
  vFormatter.Text := 'left: engine.width * 0.5; top: engine.height * 0.2 * ' +
    IntToStr(FList.Count) +'; width: engine.width * 0.8; max-height: engine.height * 0.18;';
  vEngine.FormatterList.Insert(0, vFormatter);

  vBut := TGameButton.Create('button4', vEngine);
  vBut.Text := 'Exit';
  vBut.OnClick := FExitGame;
  FList.Add(vBut);
  vFormatter := TEngineFormatter.Create(vBut.BackSprite);
  vFormatter.Text := 'left: engine.width * 0.5; top: engine.height * 0.2 * ' +
    IntToStr(FList.Count) +'; width: engine.width * 0.8; max-height: engine.height * 0.18;';
  vEngine.FormatterList.Insert(0, vFormatter);
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

procedure TGameMenu.SetAboutGame(const Value: TVCLProcedure);
begin
  FList[1].OnClick := Value;
end;

procedure TGameMenu.SetExitGame(const Value: TVCLProcedure);
begin
  FList[3].OnClick := Value;
end;

procedure TGameMenu.SetStartGame(const Value: TVCLProcedure);
begin
  FList[0].OnClick := Value;
end;

procedure TGameMenu.SetStatGame(const Value: TVCLProcedure);
begin
  FList[2].OnClick := Value;
end;

end.
