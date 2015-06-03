unit uEngine2DUnclickableObject;

interface

uses
  FMX.Objects, System.Types, System.Generics.Collections,
  uEngine2DClasses, uIntersectorClasses;

type

  tEngine2DUnclickableObject = class abstract// Базовый класс для объекта отрисовки движка
  protected
    fPosition: TPosition;
    fVisible: boolean; // Отрисовывать объект или нет
    fOpacity: Single; // Прозрачность
    fParent: pointer; // Должен быть tEngine2d
    fImage: TImage;
    fCreationNumber: integer; // Номер спрайта в массиве спрайтов
    fGroup: string;
    fNeedRecalc: Boolean;
    fClonedFrom: tEngine2DUnclickableObject;
    function GetW: single; virtual; abstract;
    function GetH: single; virtual; abstract;

    procedure SetX(AValue: single); virtual;
    procedure SetY(AValue: single); virtual;
    procedure SetScaleX(const Value: single); virtual;
    procedure SetScaleY(const Value: single); virtual;
    procedure SetScale(AValue: single); virtual;
    procedure SetRotate(AValue: single); virtual;
  public
//    property CreationNumber: integer read fCreationNumber write fCreationNumber; // Номер создания спрайта, вне зависимости от порядка отрисовки. Лень прятать. Пожалуйста, не меняйте вручную
    property Group: string read fGroup write fGroup; // Группа для быстрого скрытия - открытия спрайтов
    property Position: TPosition read fPosition write fPosition; // Быстрое получение всех данных о позиции спрайта
    property x: single read fPosition.x write setX; // Координата X на главном битмапе
    property y: single read fPosition.y write setY; // Координата Y на главном битмапе
    property w: single read getW; // Оригинальная ширина
    property h: single read getH; // Оригинальная высота
    property Opacity: single read {fPosition.opacity}fOpacity write {fPosition.opacity}fOpacity; // Прозрачность
    property Rotate: single read fPosition.rotate write setRotate; // Угол поворота относительно центра
    property ScaleX: single read fPosition.ScaleX write setScaleX;  // Масштаб спрайта во время отрисовки
    property ScaleY: single read fPosition.scaleY write setScaleY;  // Масштаб спрайта во время отрисовки
    property Scale: single write setScale;  // Масштаб спрайта во время отрисовки
    property Parent: pointer read fParent write fParent; // Родитель спрайта. Обычно tEngine2d
    property Visible: boolean read fVisible write fVisible; // Риовать спрайт или нет
    property ClonedFrom: tEngine2DUnclickableObject read fClonedFrom write fClonedFrom; // Не уверен, что это необходимо, но пока не знаю как иначе решить проблемы псевдоформаттерсов

    property Image: TImage read fImage write fImage; // В этом имедже происходит отрисовка.

   // procedure Format; virtual; // Процедура, производящая форматирование по fFormatter
    property NeedRecalc: Boolean read FNeedRecalc write fNeedRecalc; // Показывает, нужно ли пересчитывать фигуры
    function Clone: tEngine2DUnclickableObject; virtual; deprecated;
    procedure Repaint; virtual; abstract; // Процедура отрисовки объекта, переписывается спрайтом или текстом и т.д.

    constructor Create(AParent: pointer); virtual;
    destructor Destroy; override;
  end;

implementation

uses
  uEngine2D;

{ t2dEngineObject }

{procedure tEngine2DUnclickableObject.bringToBack;
begin

end;  }

function tEngine2DUnclickableObject.Clone: tEngine2DUnclickableObject;
{var
  vRes: tEngine2DUnclickableObject;   }
begin
{  vRes := tEngine2DUnclickableObject.Create(Self.Parent);
  vRes.Position := Self.fPosition;
  vRes.Visible := Self.Visible;
  vRes.Image := Self.Image;
  vRes.Group := Self.Group;
  vRes.ClonedFrom := Self;
  Result := vRes;}
  Result := Nil
end;

constructor tEngine2DUnclickableObject.Create(AParent: pointer);
var
  vEngine: TEngine2D;
begin

  fParent := AParent;
//  fCreationNumber := vEngine.SpriteList.Count;
  fPosition.ScaleX := 1;
  fPosition.ScaleY := 1;
  fPosition.rotate := 0;
  fPosition.x := 0;
  fPosition.y := 0;
  fVisible := true;
  Self.Opacity := 1;
  vEngine := TEngine2D(fParent);
  fImage := vEngine.Image;
//  fAnimation := TList<Integer>.Create;
end;

destructor tEngine2DUnclickableObject.Destroy;
begin
  fImage := Nil;
end;

{procedure tEngine2DUnclickableObject.Format;
begin
  if fFormatter.Text = '' then
    Exit;


  if fFormatter.x <> Nil then
    Self.x := fFormatter.X.Value;
  if fFormatter.y <> Nil then
    Self.y := fFormatter.y.Value;
  if fFormatter.Width <> Nil then
    Self.w := fFormatter.Width.Value;
  if fFormatter.Height <> Nil then
    Self.h := fFormatter.Height.Value;
  if fFormatter.MinWidth <> Nil then
    if Self.h * Self.Scale < fFormatter.MinWidth.Value then
      fFo
    Self.h := fFormatter.Height.Value;

end; }

{ne2DUnclickableObject.sendToFront;
begin

//  tEngine2d(fParent).spriteToFront(fCreationNumber);
end; }

{procedure tEngine2DUnclickableObject.SetAnimation(AValue: tAnimation);
begin
  fAnimation := AValue;
  fAnimation.Init;
end;                      }

procedure tEngine2DUnclickableObject.setRotate(AValue: single);
begin
  fPosition.rotate := AValue;
end;

procedure tEngine2DUnclickableObject.setScale(AValue: single);
begin
  fPosition.scaleX := AValue;
  fPosition.scaleY := AValue;
end;

procedure tEngine2DUnclickableObject.setScaleX(const Value: single);
begin
  fPosition.ScaleX := Value;
end;

procedure tEngine2DUnclickableObject.setScaleY(const Value: single);
begin
  fPosition.scaleY := Value;
end;

procedure tEngine2DUnclickableObject.setX(AValue: single);
begin
  fPosition.x := AValue;
end;

procedure tEngine2DUnclickableObject.setY(AValue: single);
begin
  fPosition.y := AValue;
end;

end.







