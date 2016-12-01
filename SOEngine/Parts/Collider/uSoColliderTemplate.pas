unit uSoColliderTemplate;

interface

uses
  System.JSON, uJsonUtils,
  uSoTypes, uSoColliderObject, uSoObject, uRawShapeJsonConverter, uRawShapes,
  uColliderDefinition;

type
  TSoColliderTemplate = class
  private
    FShapeList: TList<TRawShape>;
    FFriction: Single;
    FDensity: Single;
    FRestitution: Single;
    FIsSensor: Boolean;
    FDefinition: TColliderDefinition;
  public
//    property ShapeList: TList<TRawShape> read FShapeList;
//    function Instantiate(const ASubject: TSoObject): TSoColliderObj;
    property Definition: TColliderDefinition read FDefinition;
    constructor Create(const AJson: TJSONValue); virtual;
    destructor Destroy; override;
  end;

implementation

{ TSoColliderTemplate }

constructor TSoColliderTemplate.Create(const AJson: TJSONValue);
var
  vVal, vProp: TJSONValue;
  vArr: TJSONArray;
  i: Integer;
begin
  FShapeList := TList<TRawShape>.Create;
  vArr := TJSONArray(AJson);
  for i := 0 to vArr.Count - 1 do
  begin
    vVal := vArr.Items[i];

    FShapeList.Add(TRawShapeJsonConverter.ConvertFrom(vVal));

    if vVal.TryGetValue('Friction', vProp) then
    begin
      FFriction := JsonToSingle(vProp);
      vProp.Free;
    end;
    if vVal.TryGetValue('Density', vProp) then
    begin
      FDensity := JsonToSingle(vProp);
      vProp.Free;
    end;
    if vVal.TryGetValue('Restitution', vProp) then
    begin
      FRestitution := JsonToSingle(vProp);
      vProp.Free;
    end;
    if vVal.TryGetValue('IsSensor', vProp) then
    begin
      FIsSensor:= JsonToBool(vProp);
      vProp.Free;
    end else
      FIsSensor := False;

  end;

  FDefinition := TColliderDefinition.Create(FShapeList, FFriction, FDensity, FRestitution, FIsSensor);
end;

destructor TSoColliderTemplate.Destroy;
var
  i: Integer;
begin
  for i := 0 to FShapeList.Count - 1 do
    FShapeList[i].Free;

  FShapeList.Free;
  inherited;
end;

{function TSoColliderTemplate.Instantiate(
  const ASubject: TSoObject): TSoColliderObj;
begin
  Result := TSoColliderObj.Create(ASubject);

end; }

end.
