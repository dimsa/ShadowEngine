unit uEngine2DObject;

interface

uses
  FMX.Types, System.UITypes, System.Classes, System.Types, FMX.Objects,
  uClasses, uEngine2DUnclickableObject, uEngine2DClasses, uEngine2DObjectShape;

type
  tEngine2DObject = class(tEngine2DUnclickableObject)// Базовый класс для объекта отрисовки движка
  strict private
    FShape: TObjectShape;
    FOnMouseDown, FOnMouseUp: TMouseEvent;
    FOnClick: TNotifyEvent;
    FJustify: TObjectJustify;
    FBringToBack, FSendToFront: TNotifyEvent;
    procedure EmptyMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single); virtual;
    procedure EmptyMouseUp(Sender: TObject; Button: TMouseButton;  Shift: TShiftState; X, Y: Single); virtual;
    procedure EmptyClick(ASender: TObject); virtual;
  protected
    procedure ShapeCreating; virtual; // По умолчанию создает форму без фигур
    procedure SetJustify(const Value: TObjectJustify); virtual;
  public
    property Shape: TObjectShape read FShape write FShape; // Форма спрайта
    property Justify: TObjectJustify read FJustify write SetJustify;
    property OnMouseDown: TMouseEvent read FOnMouseDown write FOnMouseDown;
    property OnMouseUp: TMouseEvent read FOnMouseUp write FOnMouseUp;
    property OnClick: TNotifyEvent read FOnClick write FOnClick;
    property OnBringToBack: TNotifyEvent read FBringToBack write FBringToBack;
    property OnSendToFront: TNotifyEvent read FSendToFront write FSendToFront;
    function UnderTheMouse(const MouseX, MouseY: double): boolean; virtual; // Говорит, попала ли мышь в круг спрайта. Круг с диаметром - диагональю прямоугольника спрайта
    procedure BringToBack; // Ставит спрайт первым в списке отрисовки. Т.е. Переносит назад
    procedure SendToFront; // Ставит спрайт последним в списке отрисовки. Т.е. Переносит вперед
    function Config(const AGroup: string = ''; const AJustify: TObjectJustify = Center; const AShape: TObjectShape = nil): TEngine2DObject; virtual;

    procedure RepaintWithShapes;

    constructor Create; override;
    destructor Destroy; override;
  end;

implementation

uses
  uEngine2D;

{ tEngine2DObject }

procedure tEngine2DObject.BringToBack;
begin
    FBringToBack(Self);
end;

function tEngine2DObject.Config(const AGroup: string = ''; const AJustify: TObjectJustify =Center;  const AShape: TObjectShape = nil): TEngine2DObject;
begin
    FJustify := AJustify;

  if AGroup <> '' then
    FGroup := AGroup;

  if AShape <> nil then
    FShape := AShape;

  Result := Self;
end;

constructor tEngine2DObject.Create;
begin
  inherited Create;
  FJustify := TObjectJustify.Center;
  ShapeCreating;
  Self.OnMouseDown := EmptyMouseDown;
  Self.OnMouseUp := EmptyMouseUp;
  Self.OnClick := EmptyClick;
end;

destructor tEngine2DObject.Destroy;
begin

  inherited;
end;

procedure tEngine2DObject.EmptyClick;
begin

end;

procedure tEngine2DObject.EmptyMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin

end;

procedure tEngine2DObject.EmptyMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin

end;

procedure tEngine2DObject.RepaintWithShapes;
begin
  Shape.Draw;
end;

procedure tEngine2DObject.SendToFront;
begin
    FSendToFront(Self);
end;

procedure tEngine2DObject.SetJustify(const Value: TObjectJustify);
begin
  FJustify := Value;
end;

procedure tEngine2DObject.ShapeCreating;
begin
  FShape := TObjectShape.Create;
  FShape.Owner := Self;
end;

function tEngine2DObject.UnderTheMouse(const MouseX,
  MouseY: Double): boolean;
begin
  { Попадает ли нажатия мыши в круг на диагонали прямоугольника спрайта
       __---__
     /|     /|\
    ( |    / | )
   |  |   /  |  |
    ( |  /   | )
     \|/_   _|/
         ---  }
 { if (mousex - fPosition.x) * (mousex - fPosition.x) + (mousey - fPosition.y) *
     (mousey - fPosition.y) <= (H * Scale * H * Scale + W * Scale * W * Scale) * 0.25 then
    result := true
  else }

  Result := FShape.UnderTheMouse(MouseX, MouseY);
end;

end.
