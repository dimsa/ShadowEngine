unit uEngine2DModel;

interface

uses
  System.Generics.Collections, System.SyncObjs,
  uEngine2DClasses, uSpriteList, uFastFields, uEngine2DAnimationList,
  uFormatterList, uEngine2DResources, uEngine2DObject, uClasses;

type

TEngine2DModel = class
private
  FCritical: TCriticalSection;
  // Ключевые списки движка.
  FObjects: TObjectsList; // Массив спрайтов для отрисовки
  FFastFields: TFastFields; // Содержит ссылки на TFastField, которые представляют собой найденные значения определенных спрайтов
  FObjectOrder: TIntArray; // Массив порядка отрисовки. Нужен для уменьшения кол-ва вычислений, содержит номер спрайта
  FResources: TEngine2DResources; //tResourceArray; // Массив битмапов
  FFormatters: TFormatterList; // Массив Форматтеров спрайтов
  FAnimationList: TEngine2DAnimationList; // Массив анимаций
  FIsHor: TBooleanFunction;
  procedure setObject(AIndex: integer; ASprite: tEngine2DObject);
  function getObject(AIndex: integer): tEngine2DObject;
public
  ObjectOrder: TIntArray;
  property Resources: TEngine2DResources read FResources;
  property AnimationList: TEngine2DAnimationList read FAnimationList;
  property FormatterList: TFormatterList read FFormatters;
  property ObjectList: TObjectsList read FObjects;
//property ObjectOrder: TIntArray ;read FObjectOrder write FObjectOrder;
  property FastFields: tFastFields read FFastFields; // Быстрый вызов для экспрешенсов
  property Objects[index: integer]: tEngine2DObject read getObject write setObject;
  procedure ClearSprites;
  constructor Create(const ACritical: TCriticalSection; const AEngine: Pointer; const AIsHor: TBooleanFunction);
  destructor Destroy; override;
end;

implementation

{ TEngine2DModel }

procedure TEngine2DModel.ClearSprites;
var
  i: Integer;
begin
  for i := 0 to FObjects.Count - 1 do
    Objects[i].Free;

  setLength(FObjectOrder, 0);
end;

constructor TEngine2DModel.Create(const ACritical: TCriticalSection; const AEngine: Pointer; const AIsHor: TBooleanFunction);
begin
  FCritical := ACritical;
  FObjectOrder := TIntArray.Create(0);
  FResources := TEngine2DResources.Create(FCritical);
  FAnimationList := TEngine2DAnimationList.Create(FCritical);
  FFormatters := TFormatterList.Create(FCritical, AEngine);
  FObjects := TObjectsList.Create(FCritical);

  FIsHor := AIsHor;
  FFastFields := TFastFields.Create(FIsHor);
end;

destructor TEngine2DModel.Destroy;
begin
  FAnimationList.Free;
  FFormatters.Free;
  FFastFields.Free;
  FFormatters.Free;

  ClearSprites;
  FObjects.Free;
  inherited;
end;

function TEngine2DModel.getObject(AIndex: integer): tEngine2DObject;
begin
  FCritical.Enter;
  result := FObjects[AIndex];
  FCritical.Leave;
end;

procedure TEngine2DModel.setObject(AIndex: integer; ASprite: tEngine2DObject);
begin
  FCritical.Enter;
  FObjects[AIndex] := ASprite;
  FCritical.Leave;
end;

end.
