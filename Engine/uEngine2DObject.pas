unit uEngine2DObject;

interface

uses
  FMX.Types, System.UITypes, System.Classes, System.Types,
  uClasses, uEngine2DUnclickableObject, uEngine2DClasses, uEngine2DObjectShape;

type

  tEngine2DObject = class(tEngine2DUnclickableObject)// Базовый класс для объекта отрисовки движка
  strict private
    FShape: TObjectShape;
    FOnMouseDown, FOnMouseUp: TMouseEvent;
    FOnClick: TVCLProcedure;
    FJustify: TObjectJustify;
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
    property OnClick: TVCLProcedure read FOnClick write FOnClick;
    function UnderTheMouse(const MouseX, MouseY: double): boolean; virtual; // Говорит, попала ли мышь в круг спрайта. Круг с диаметром - диагональю прямоугольника спрайта
    //procedure NullValue;
    procedure BringToBack; // Ставит спрайт первым в списке отрисовки. Т.е. Переносит назад
    procedure SendToFront; // Ставит спрайт последним в списке отрисовки. Т.е. Переносит вперед

    procedure Repaint; override;

    constructor Create(AParent: pointer); override;
    destructor Destroy; override;
  end;

implementation

uses
  uEngine2D;

{ tEngine2DObject }

procedure tEngine2DObject.BringToBack;
begin
  //tEngine2d(fParent).SpriteList.IndexOfItem(Self)
  tEngine2d(fParent).spriteToBack(
    tEngine2d(fParent).SpriteList.IndexOfItem(Self, FromBeginning)
  );
end;

constructor tEngine2DObject.Create(AParent: pointer);
begin
  inherited Create(AParent);
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

{procedure tEngine2DObject.NullValue;
begin
  Self.fPosition.X := 0;
  Self.fPosition.Y := 0;
end;              }

procedure tEngine2DObject.Repaint;
begin
  if TEngine2D(FParent).DrawFigures then
    Shape.Draw;
end;

procedure tEngine2DObject.SendToFront;
begin
  tEngine2d(fParent).SpriteToFront(
    tEngine2d(fParent).SpriteList.IndexOfItem(Self, FromBeginning)
  );
end;

procedure tEngine2DObject.SetJustify(const Value: TObjectJustify);
begin
  FJustify := Value;
end;

procedure tEngine2DObject.ShapeCreating;
begin
  FShape := TObjectShape.Create;
  FShape.Parent := fParent;
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

