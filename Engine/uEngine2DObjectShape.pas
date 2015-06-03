unit uEngine2DObjectShape;

interface

uses
  System.Types, System.Generics.Collections,
  uEngine2DClasses, uIntersectorFigure, uIntersectorComparer;

type
  TObjectShape = class
  private
    FFigures: TList<TFigure>;
    FParent: Pointer;
    FOwner: Pointer;
    function GetOuterRect: TRectF;
    function GetFigure(Index: Integer): TFigure;
    procedure SetFigure(Index: Integer; const Value: TFigure);
    function GetCount: Integer;
  public
    property Parent: Pointer read FParent write FParent; // ——ылка на TEngine2D
    property Owner: Pointer read FOwner write FOwner; // ’оз€ин фигуры TEngine2DObject
    property Figures[Index: Integer]: TFigure read GetFigure write SetFigure; default;
    property FiguresList: TList<TFigure> read FFigures;
    property Count: Integer read GetCount;
    property OuterRect: TRectF read GetOuterRect;
    procedure Recalc; // ѕересчитывает фигуры, согласно масштабу, повороту и положению хоз€ина
    function UnderTheMouse(const MouseX, MouseY: double): boolean; virtual; // √оворит, попала ли мышь в круг спрайта.  руг с диаметром - диагональю пр€моугольника спрайта
    function IsIntersectWith(AShape: TObjectShape): Boolean; // ѕересекаеис€ ли с конкретной фигурой
    function Intersections: TList<TObjectShape>; // —писок всех пересечений с другими фигурами
  end;

implementation

uses
  uEngine2DObject;

{ TObjectShape }

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

procedure TObjectShape.Recalc;
var
  i, vN: Integer;
begin
  vN := FFigures.Count - 1;
  for i := 0 to vN do
    FFigures[i].Position := TEngine2DObject(FOwner).Position;
end;

procedure TObjectShape.SetFigure(Index: Integer; const Value: TFigure);
begin
  FFigures[Index] := Value;
end;

function TObjectShape.UnderTheMouse(const MouseX, MouseY: double): boolean;
var
  i: Integer;
begin
  for i := 0 to FFigures.Count - 1 do
    if FFigures[i].BelongPoint(MouseX, MouseY) then
      Exit(True);

  Result := False;
end;

end.





