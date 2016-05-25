unit uEngine2DModel;

interface

uses
  System.Generics.Collections, System.SyncObjs,
  uEngine2DClasses, uSpriteList, uFastFields, uEngine2DAnimationList,
  uFormatterList, uEngine2DResources, uEngine2DObject;

type

TEngine2DModel = class
private
  FCritical: TCriticalSection;
  FObjects: TObjectsList; // Массив спрайтов для отрисовки
  FFastFields: TFastFields; // Содержит ссылки на TFastField, которые представляют собой найденные значения определенных спрайтов
  FObjectOrder: TIntArray; // Массив порядка отрисовки. Нужен для уменьшения кол-ва вычислений, содержит номер спрайта
  FResources: TEngine2DResources; //tResourceArray; // Массив битмапов
  FFormatters: TFormatterList; // Массив Форматтеров спрайтов
  FAnimationList: TEngine2DAnimationList; // Массив анимаций
  procedure setObject(AIndex: integer; ASprite: tEngine2DObject);
  function getObject(AIndex: integer): tEngine2DObject;
public
  property Resources: TEngine2DResources read FResources;
  property AnimationList: TEngine2DAnimationList read FAnimationList;
  property FormatterList: TFormatterList read FFormatters;
  property ObjectList: TObjectsList read FObjects;
  property ObjectOrder: TIntArray read FObjectOrder;
  property FastFields: tFastFields read FFastFields; // Быстрый вызов для экспрешенсов
  property Objects[index: integer]: tEngine2DObject read getObject write setObject;
  constructor Create(const ACritical: TCriticalSection);
end;

implementation

{ TEngine2DModel }

constructor TEngine2DModel.Create(const ACritical: TCriticalSection);
begin
  FCritical := ACritical;
  FObjectOrder := TIntArray.Create(0);
  FResources := TEngine2DResources.Create(FCritical);
  FAnimationList := TEngine2DAnimationList.Create(FCritical);
  FFormatters := TFormatterList.Create(FCritical, Self);
  FObjects := TObjectsList.Create(FCritical);
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
