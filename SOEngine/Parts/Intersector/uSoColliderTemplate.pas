unit uSoColliderTemplate;

interface

uses
  System.JSON, uJsonUtils,
  uSoColliderObject, uSoObject;

type
  TSoColliderTemplate = class
  private

  public
    function Instantiate(const ASubject: TSoObject): TSoColliderObj;
    constructor Create(const AJson: TJSONValue); virtual;
  end;

implementation

{ TSoColliderTemplate }

constructor TSoColliderTemplate.Create(const AJson: TJSONValue);
var
  vVal, vProp: TJSONValue;
  vArr: TJSONArray;
  i: Integer;
begin

  vArr := TJSONArray(AJson);
  for i := 0 to vArr.Count - 1 do
  begin
    vVal := vArr.Items[i];

    if vVal.TryGetValue('Type', vProp) then
      uJsonUtils.JsonToColliderType(vProp);

  end;



 { if AJson.TryGetValue('Opacity', vVal) then
    FOpacity := StrToFloat(vVal.ToString)
  else
    FOpacity := 1;

  if AJson.TryGetValue('Justify', vVal) then
    FObjectJustify := JsonToJustify(TJSONObject(vVal))
  else
    FObjectJustify := Center;

  if AJson.TryGetValue('Margin', vVal) then
    FMargin := JsonToPointF(vVal)
  else
    FMargin := TPointF.Zero;      }
end;

function TSoColliderTemplate.Instantiate(
  const ASubject: TSoObject): TSoColliderObj;
begin
  Result := TSoColliderObj.Create(ASubject);

end;

end.
