unit uEngine2DObjectShape;

interface

uses
  System.Types, System.Generics.Collections,
  uEngine2DClasses, uIntersectorFigure, uIntersectorMethods,
  uIntersectorShapeModificator;

type
  TObjectShape = class
  private
    FFigures: TList<TFigure>;
    FOriginFigures: TList<TFigure>;
    FParent: Pointer;
    FOwner: Pointer;
    FNeedRecalc: Boolean;
    FSize: Single;
    FModificators: TList<TShapeModificator>;
    function GetOuterRect: TRectF;
    function GetFigure(Index: Integer): TFigure;
    procedure SetFigure(Index: Integer; const Value: TFigure);
    function GetCount: Integer;
    procedure SetSize(const Value: Single);
  public
    property NeedRecalc: Boolean read FNeedRecalc write fNeedRecalc; // Показывает, нужно ли пересчитывать фигуры
    property Parent: Pointer read FParent write FParent; // ССылка на TEngine2D
    property Owner: Pointer read FOwner write FOwner; // Хозяин фигуры TEngine2DObject
    property Figures[Index: Integer]: TFigure read GetFigure write SetFigure; default; // Это уже посчитанные фигуры
//    property OriginFigures
    property OriginFigures: TList<TFigure> read FOriginFigures;
    property Modificators: TList<TShapeModificator> read FModificators write FModificators;
    property Count: Integer read GetCount;
    property OuterRect: TRectF read GetOuterRect;
    property Size: Single read FSize write SetSize; // Испльзуется для быстрых расчетов
    procedure Draw; // Рисует форму фигуры
    procedure Compute; virtual;
//    procedure Recalc; // Пересчитывает фигуры, согласно масштабу, повороту и положению хозяина
    function UnderTheMouse(const MouseX, MouseY: double): boolean; virtual; // Говорит, попала ли мышь в круг спрайта. Круг с диаметром - диагональю прямоугольника спрайта
    function IsIntersectWith(AShape: TObjectShape): Boolean; // Пересекаеися ли с конкретной фигурой
    function Intersections: TList<TObjectShape>; // Список всех пересечений с другими фигурами
    constructor Create;
    destructor Destroy;
  end;

implementation

uses
  uEngine2DObject;

{ TObjectShape }

procedure TObjectShape.Compute;
var
  i, vN: Integer;
begin
  {vN := FModificators.Count - 1;
  for i := 0 to vN do
    FModificators[i].Apply();
    FFigures[i].Compute;   }

  vN := FFigures.Count - 1;
  for i := 0 to vN do
  begin
    FFigures[i].Scale(TPointF(tEngine2DObject(FOwner).ScaleX,tEngine2DObject(FOwner).ScaleY));
    FFigures[i].Rotate();
  end;


  //  FFigures[i].Compute;
//    .Position := TEngine2DObject(FOwner).Position;   }
end;

constructor TObjectShape.Create;
begin
  FFigures := TList<TFigure>.Create;
end;

destructor TObjectShape.Destroy;
var
  i, vN: Integer;
begin
  vN := FFigures.Count - 1;

  for i := vN downto 0 do
  begin
    FFigures[i].Free;
    FFigures.Delete(i);
  end;

  FFigures.Free;
end;

procedure TObjectShape.Draw;
begin
  TEngine2DObject(Owner).
  image.Bitmap.Canvas.FillEllipse(
  RectF(
    TEngine2DObject(Owner).x -  TEngine2DObject(Owner).w*0.5,
    TEngine2DObject(Owner).y -  TEngine2DObject(Owner).h*0.5,
    TEngine2DObject(Owner).x +  TEngine2DObject(Owner).w*0.5,
    TEngine2DObject(Owner).y +  TEngine2DObject(Owner).h*0.5),
    TEngine2DObject(Owner).opacity
  );
end;

function TObjectShape.GetCount: Integer;
begin
  Result := Self.FFigures.Count;
end;

function TObjectShape.GetFigure(Index: Integer): TFigure;
begin
  Result := FFigures[Index];
end;

function TObjectShape.GetOuterRect: TRectF;
var
  vLeft, vRight: TPointF;
  i, vN: Integer;
begin
  if FFigures.Count > 0 then
  begin
    vLeft := FFigures[0].FigureRect.TopLeft;
    vRight := FFigures[0].FigureRect.BottomRight;
    vN := FFigures.Count - 1;
    for i := 1 to vN do
    begin
      if vLeft.X > FFigures[i].FigureRect.Left then
        vLeft.X := FFigures[i].FigureRect.Left;
      if vLeft.Y > FFigures[i].FigureRect.Top then
        vLeft.Y := FFigures[i].FigureRect.Top;
      if vRight.X < FFigures[i].FigureRect.Right then
        vRight.X := FFigures[i].FigureRect.Right;
      if vRight.Y < FFigures[i].FigureRect.Bottom then
        vRight.Y := FFigures[i].FigureRect.Bottom;
    end;
    Exit(RectF(vLeft.X,vLeft.Y, vRight.X, vRight.Y));
  end;
  Result := TRectF.Empty;
end;

function TObjectShape.Intersections: TList<TObjectShape>;
begin
end;

function TObjectShape.IsIntersectWith(AShape: TObjectShape): Boolean;
var
  i, j, vN: Integer;
begin
{  vN := Self.FFigures.Count - 1;
  for i := 0 to vN do
    for j := 0 to AShape.Count - 1 do
      if TIntersectionComparer.IsFiguresCollide(Self.FFigures[i], AShape.Figures[j]) then
        Exit(True);

  Result := False;  }
end;

{procedure TObjectShape.Recalc;
var
  i, vN: Integer;
begin
  vN := FFigures.Count - 1;
  for i := 0 to vN do
    FFigures[i].Position := TEngine2DObject(FOwner).Position;
end;  }

procedure TObjectShape.SetFigure(Index: Integer; const Value: TFigure);
begin
  FFigures[Index] := Value;
end;

procedure TObjectShape.SetSize(const Value: Single);
begin
  FSize := Value;
end;

function TObjectShape.UnderTheMouse(const MouseX, MouseY: double): boolean;
var
  i: Integer;
begin
{  for i := 0 to FFigures.Count - 1 do
    if FFigures[i].BelongPoint(MouseX, MouseY) then
      Exit(True); }

  Result := False;
end;

end.





