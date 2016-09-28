unit uSoRenditionTemplate;

interface

uses
  System.JSON, uJsonUtils, FMX.Graphics, System.UITypes, System.UIConsts, System.SysUtils,
  uGeometryClasses, uSoTypes, uE2DRendition, uE2DShape, uE2DSprite, uE2DText, uSoObject,
  uEngine2DClasses;

type
  TSoRenditionTemplate = class
  protected
    FOpacity: Single;
    FObjectJustify: TObjectJustify;
  public
    function Instantiate(const ASubject: TSoObject; const AImage: TAnonImage): TEngine2DRendition; virtual; abstract;
    constructor Create(const AJson: TJSONObject); virtual;
  end;

  TSoSpriteTemplate = class(TSoRenditionTemplate)
  private
    FResources: TDict<string, TBitmap>;
    FResourceList: TList<TBitmap>;
  public
    function Instantiate(const ASubject: TSoObject; const AImage: TAnonImage): TEngine2DRendition; override;
    constructor Create(const AJson: TJSONObject; const AResources: TDict<string, TBitmap>);
  end;

  TSoShapeTemplate = class(TSoRenditionTemplate)
  private
    FType: TFigureType;
    FFigureRect: TRectF;
    FPen: TStrokeBrush;
    FBrush: TBrush;
  public
    function Instantiate(const ASubject: TSoObject; const AImage: TAnonImage): TEngine2DRendition; override;
    constructor Create(const AJson: TJSONObject); override;
  end;

  TSoTextTemplate = class(TSoRenditionTemplate)
  private
    FText: string; // Text to Out
    FFont: TFont; // Text Font
    FColor: TColor; // Цвет текста
    FFillTextFlags: TFillTextFlags; // Properties of text
    FVerAlign, FHorAlign: TTextAlign; // Horizontal and Vertical Align of text
    FWordWrap: Boolean;
    FTextRect: TRectF;
  public
    function Instantiate(const ASubject: TSoObject; const AImage: TAnonImage): TEngine2DRendition; override;
    constructor Create(const AJson: TJSONObject); override;
  end;

implementation

{ TSoTextTemplate }

constructor TSoTextTemplate.Create(const AJson: TJSONObject);
var
  vVal: TJSONValue;
begin
  inherited;

  if AJson.TryGetValue('Text', vVal) then
    FText:= vVal.Value;

  if AJson.TryGetValue('Font', vVal) then
    FFont := JsonToFont(TJSONObject(vVal));

  if AJson.TryGetValue('Color', vVal) then
    FColor := StringToColor(vVal.Value);

  if AJson.TryGetValue('FillTextFlags', vVal) then
    FFillTextFlags := JsonToFillTextFlags(TJSONObject(vVal));

  if AJson.TryGetValue('VerAlign', vVal) then
    FVerAlign := JsonToTextAlign(TJSONObject(vVal));

  if AJson.TryGetValue('HorAlign', vVal) then
    FHorAlign := JsonToTextAlign(TJSONObject(vVal));

  if AJson.TryGetValue('WordWrap', vVal) then
    FWordWrap := JsonToBoolean(TJSONObject(vVal));

  if AJson.TryGetValue('TextRect', vVal) then
    FTextRect := JsonToRectF(TJSONObject(vVal));
end;

function TSoTextTemplate.Instantiate(const ASubject: TSoObject; const AImage: TAnonImage): TEngine2DRendition;
var
  vFont: TFont;
begin
  Result := TEngine2DText.Create(ASubject, AImage);
  with TEngine2DText(Result) do begin
    Opacity := FOpacity;
    Justify := FObjectJustify;
    vFont := TFont.Create;
    vFont.Assign(FFont);
    Font := vFont;
    TextRect := TRectF.Create(FTextRect);
    Color := FColor;
    WordWrap := FWordWrap;
  end;
end;

{ TSoShapeTemplate }

constructor TSoShapeTemplate.Create(const AJson: TJSONObject);
var
  vVal: TJSONValue;
begin
  inherited;

  if AJson.TryGetValue('Type', vVal) then
    if LowerCase(vVal.ToString) = 'ellipse' then
      FType := ftEllipse
    else
      if LowerCase(vVal.ToString) = 'rect' then
        FType := ftRect
      else
        raise Exception.Create('Unknown Figure Type Template');

  FPen := TStrokeBrush.Create(TBrushKind.None, TAlphaColorRec.Red);
  FBrush := TBrush.Create(TBrushKind.None, TAlphaColorRec.Gray);

  { TODO : Add Reading of Brushes and Pens }
end;

function TSoShapeTemplate.Instantiate(const ASubject: TSoObject; const AImage: TAnonImage): TEngine2DRendition;
begin
  Result := TEngine2DShape.Create(ASubject, AImage);

  with TEngine2DShape(Result) do begin
    Opacity := FOpacity;
    Justify := FObjectJustify;
    Pen.Assign(FPen);
    Brush.Assign(FBrush);
  end;
end;

{ TSoSpriteTemplate }

constructor TSoSpriteTemplate.Create(const AJson: TJSONObject; const AResources: TDict<string, TBitmap>);
var
  vVal: TJSONValue;
  i: Integer;
  vArr: TJSONArray;
begin
  inherited Create(AJson);

  FResources := AResources;
  FResourceList := TList<TBitmap>.Create;
  if AJson.TryGetValue('Resources', vVal) then
  begin
    vArr := TJSONArray(vVal);
    for i := 0 to vArr.Count - 1 do
      FResourceList.Add(AResources[vArr.Items[i].Value]);
  end;
end;

function TSoSpriteTemplate.Instantiate(const ASubject: TSoObject; const AImage: TAnonImage): TEngine2DRendition;
begin
{ TODO : Create new TSoSprite }
  raise Exception.Create('Instantiate of TSoSpriteTemplate not Implemented');
{  Result := TEngine2DSprite.Create(ASubject, AImage, FResourceList);
  with TEngine2DSprite(Result) do begin
    Opacity := FOpacity;
    Justify := FObjectJustify;
  end;   }
end;

{ TSoRenditionTemplate }

constructor TSoRenditionTemplate.Create(const AJson: TJSONObject);
var
  vVal: TJSONValue;
begin
  if AJson.TryGetValue('Opacity', vVal) then
    FOpacity := StrToFloat(vVal.ToString);

  if AJson.TryGetValue('Justify', vVal) then
    FObjectJustify := JsonToJustify(TJSONObject(vVal));
end;

end.
