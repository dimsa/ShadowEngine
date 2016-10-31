unit uSoColliderTemplate;

interface

uses
  System.JSON, uJsonUtils,
  uSoTypes, uSoColliderObject, uSoObject, uRawShapesConverter, uRawShapes;

type
  TSoColliderTemplate = class
  private
    FShapeList: TList<TRawShape>;
  public
    property ShapeList: TList<TRawShape> read FShapeList;
//    function Instantiate(const ASubject: TSoObject): TSoColliderObj;
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

    if vVal.TryGetValue('Type', vProp) then
      FShapeList.Add(TRawShapeJsonConverter.ConvertFrom(vProp));
  end;
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
