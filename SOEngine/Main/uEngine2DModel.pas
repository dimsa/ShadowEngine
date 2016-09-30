unit uEngine2DModel;

interface

uses
  System.Generics.Collections, System.SyncObjs, uCommonClasses,
  uEngine2DClasses, uSpriteList, uFastFields, uEngine2DAnimationList,
  uFormatterList, uEngine2DResources, uEngine2DObject, uEngine2DIntersector, uClasses;

type

TEngine2DModel = class
private
  FCritical: TCriticalSection;
  FObjects: TObjectsList;
  FFastFields: TFastFields; // Pointers to TFastField to do formatting fast // Содержит ссылки на TFastField, которые представляют собой найденные значения определенных спрайтов
  FObjectOrder: TIntArray;
  FResources: TEngine2DResources;
  FFormatters: TFormatterList;
  FAnimationList: TEngine2DAnimationList;
  FIsHor: TDelegate<Boolean>;
  FIntersector: TEngine2DIntersector; // Object tha test for colliding
  procedure setObject(AIndex: integer; ASprite: tEngine2DObject);
  function getObject(AIndex: integer): tEngine2DObject;
public
  // Main lists of Engine
  ObjectOrder: TIntArray; // Массив порядка отрисовки. Нужен для уменьшения кол-ва вычислений, содержит номер спрайта
  property Resources: TEngine2DResources read FResources; //tResourceArray; // Массив битмапов
  property AnimationList: TEngine2DAnimationList read FAnimationList;
  property FormatterList: TFormatterList read FFormatters; // Массив Форматтеров спрайтов
  property ObjectList: TObjectsList read FObjects; // Массив спрайтов для отрисовки
  property FastFields: tFastFields read FFastFields; // Быстрый вызов для экспрешенсов
  property Objects[index: integer]: tEngine2DObject read getObject write setObject;
  procedure ClearSprites; // Очищает массив спрайтов, т.е. является подготовкой к полной перерисовке
  constructor Create(const ACritical: TCriticalSection; const AIsHor: TDelegate<Boolean>);
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

constructor TEngine2DModel.Create(const ACritical: TCriticalSection; const AIsHor: TDelegate<Boolean>);
begin
  FCritical := ACritical;
  FObjectOrder := TIntArray.Create(0);
  FResources := TEngine2DResources.Create(FCritical);
  FAnimationList := TEngine2DAnimationList.Create(FCritical);
  FFormatters := TFormatterList.Create(FCritical);
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
