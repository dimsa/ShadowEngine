unit uDemoMenu;

interface

uses
  System.Types, System.UITypes, System.Math,
  {$IFDEF VER290} System.Math.Vectors, {$ENDIF}
  uEngine2DObject, uEngine2DText, uEngine2DSprite, uEngineFormatter,
  uIntersectorMethods;

type
  TGameButton = class
  private
    FParent: Pointer; // Engine2d
    FBack: TSprite;
    FText: TEngine2DText;
    FFormatters: TEngineFormatter;
    function GetText: String;
    procedure SetText(const Value: String);
  public
    constructor Create(AParent: Pointer);
    destructor Destroy; override;
    property Text: String read GetText write SetText;
  end;


implementation

uses
  uEngine2D, uEngine2DObjectShape, uNewFigure;

{ TGameButton }

constructor TGameButton.Create(AParent: Pointer);
var
  vEngine: tEngine2d;
  vFormatter: TEngineFormatter;
  vFigure: TNewFigure;
  vPoly: TPolygon;
  vFText: String;
begin
  vEngine := AParent;
  FParent := vEngine;
  FBack := TSprite.Create(vEngine);
  FBack.Resources := vEngine.Resources;
  FBack.CurRes := vEngine.Resources.IndexOf('button');
  FBack.Group := 'menu';
  FText := TEngine2DText.Create(vEngine);
  FText.Group := 'menu';
  FText.Color := TAlphaColorRec.White;

  vEngine.AddObject('button1', FBack);
  vEngine.AddObject(FText);

  vFormatter := TEngineFormatter.Create(FBack);
  vFormatter.Parent := vEngine;
  vFText := 'left: engine.width * 0.5; top: engine.height * 0.2; width: engine.width * 0.5';
  vFormatter.Text := vFText;
  vEngine.FormatterList.Add(vFormatter);

  vFormatter := TEngineFormatter.Create(FText);
  vFormatter.Parent := vEngine;
  vFText := 'left: engine.width * 0.5; top: engine.height * 0.2; ';
  vFormatter.Text := vFText;
  vEngine.FormatterList.Add(vFormatter);


  vFormatter.Format;
  vPoly := PolyFromRect(RectF(0, 0, FBack.w, FBack.h));
  Translate(vPoly, -PointF(FBack.wHalf, FBack.hHalf));
  vFigure := TNewFigure.CreatePoly;
  vFigure.SetData(vPoly);

  FBack.Shape.AddFigure(vFigure);


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

procedure TGameButton.SetText(const Value: String);
begin
  Ftext.Text := Value;
end;

end.
