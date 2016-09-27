unit uSoRenditionTemplate;

interface

uses
  System.JSON, uJsonUtils, FMX.Graphics, System.UITypes, System.UIConsts,
  uSoTypes, uE2DRendition;

type
  TSoRenditionTemplate = class
  public
    function Instantiate: TEngine2DRendition; virtual; abstract;
  end;

  TSoSpriteTemplate = class(TSoRenditionTemplate)
  private
    FResourceList: TList<Integer>;
  public
    function Instantiate: TEngine2DRendition; override;
    constructor Create(const AJson: TJSONObject);
  end;

  TSoShapeTemplate = class(TSoRenditionTemplate)
  private
    FFigureRect: TRectF;
    FPen: TStrokeBrush;
    FBrush: TBrush;
  public
    function Instantiate: TEngine2DRendition; override;
    constructor Create(const AJson: TJSONObject);
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
    function Instantiate: TEngine2DRendition; override;
    constructor Create(const AJson: TJSONObject);
  end;

implementation

{ TSoTextTemplate }

constructor TSoTextTemplate.Create(const AJson: TJSONObject);
var
  vVal: TJSONValue;
begin
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

//  if AJson.TryGetValue('Color', vVal) then
//    TColor.
end;

function TSoTextTemplate.Instantiate: TEngine2DRendition;
begin

end;

{ TSoShapeTemplate }

constructor TSoShapeTemplate.Create(const AJson: TJSONObject);
begin

end;

function TSoShapeTemplate.Instantiate: TEngine2DRendition;
begin

end;

{ TSoSpriteTemplate }

constructor TSoSpriteTemplate.Create(const AJson: TJSONObject);
begin

end;

function TSoSpriteTemplate.Instantiate: TEngine2DRendition;
begin

end;

end.
