unit uEngine2DObjectShape;

interface

uses
  System.Types, System.Generics.Collections,System.Math,
  {$IFDEF VER290} System.Math.Vectors, {$ENDIF}
  uEngine2DClasses, uIntersectorFigure, uIntersectorCircle, uIntersectorPoly,
  uIntersectorMethods, uIntersectorShapeModificator;

type
  TObjectShape = class
  private
//    FCalcedFigures: TArray<TFigure>;
    FFigures: TArray<TFigure>;
    FParent: Pointer;
    FOwner: Pointer;
    FNeedRecalc: Boolean;
    FSize: Single;
    FModificators: TList<TShapeModificator>;
    function GetOuterRect: TRectF;
    function GetFigure(Index: Integer): TFigure;
    //procedure SetFigure(Index: Integer; const Value: TFigure);
    function GetCount: Integer;
    procedure SetSize(const Value: Single);
    function PointToLocal(const APoint: TPointF): TPointF;
  public
    property NeedRecalc: Boolean read FNeedRecalc write fNeedRecalc; // Показывает, нужно ли пересчитывать фигуры
    property Parent: Pointer read FParent write FParent; // ССылка на TEngine2D
    property Owner: Pointer read FOwner write FOwner; // Хозяин фигуры TEngine2DObject
    property Figures[Index: Integer]: TFigure read GetFigure;// write SetFigure; default; // Это уже посчитанные фигуры
//    function ToWorldCoord(const AFigure: TFigure): TFigure;
//    property OriginFigures
//    property OriginFigures: TList<TFigure> read FOriginFigures;
//    property Modificators: TList<TShapeModificator> read FModificators write FModificators;
    property Count: Integer read GetCount;
    property OuterRect: TRectF read GetOuterRect;
    property Size: Single read FSize write SetSize; // Испльзуется для быстрых расчетов
    procedure Draw; // Рисует форму фигуры
//    procedure Recalc; // Пересчитывает фигуры, согласно масштабу, повороту и положению хозяина
    function IsPointInFigure(const APoint: TPointF; const AFigure: TFigure): Boolean;
    function AddFigure(AFigure: TFigure): Integer;
    function RemoveFigure(const AIndex: Integer): TFigure;
    function UnderTheMouse(const MouseX, MouseY: double): boolean; virtual; // Говорит, попала ли мышь в круг спрайта. Круг с диаметром - диагональю прямоугольника спрайта
    function IsIntersectWith(AShape: TObjectShape): Boolean; // Пересекаеися ли с конкретной фигурой
    function Intersections: TList<TObjectShape>; // Список всех пересечений с другими фигурами
    constructor Create;
    destructor Destroy;
  const
    pi180 = 0.01745329251;
  end;

implementation

uses
  uEngine2DObject;

{ TObjectShape }

function TObjectShape.AddFigure(AFigure: TFigure): Integer;
begin
//  SetLength(FCalcedFigures, Length(FCalcedFigures) + 1);
  SetLength(FFigures, Length(FFigures) + 1);
  FFigures[High(FFigures)] := AFigure;
//  FCalcedFigures[High(FOriginFigures)] := AFigure.Clone;

  Result := High(FFigures);
end;

constructor TObjectShape.Create;
begin

end;

destructor TObjectShape.Destroy;
var
  i, vN: Integer;
begin
  vN := Length(FFigures)- 1;

  for i := vN downto 0 do
  begin
    FFigures[i].Free;
  end;

  SetLength(FFigures, 0);
end;

procedure TObjectShape.Draw;
var
  vFigure, vTemp: TFigure;
begin
  // it's only for debug, so it not very fast
  if Length(Self.FFigures) > 0 then
  begin
    for vFigure in FFigures do
    begin
      vTemp := vFigure.Clone;
      vTemp.X := vTemp.X + TEngine2DObject(Owner).x;
      vTemp.Y := vTemp.Y + TEngine2DObject(Owner).y;
//      vTemp.Center := vTemp.Center - TEngine2DObject(Owner).Position
      vTemp.Draw(TEngine2DObject(Owner).Image);
    end;
  end else

    TEngine2DObject(Owner).
    image.Bitmap.Canvas.FillEllipse(
    RectF(
      TEngine2DObject(Owner).x - TEngine2DObject(Owner).w*0.5,
      TEngine2DObject(Owner).y - TEngine2DObject(Owner).h*0.5,
      TEngine2DObject(Owner).x + TEngine2DObject(Owner).w*0.5,
      TEngine2DObject(Owner).y + TEngine2DObject(Owner).h*0.5),
      TEngine2DObject(Owner).opacity
    );
end;

function TObjectShape.GetCount: Integer;
begin
  Result := Length(FFigures);
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
{  if FFigures.Count > 0 then
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
  Result := TRectF.Empty;  }
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

function TObjectShape.IsPointInFigure(const APoint: TPointF; const AFigure: TFigure): Boolean;
var
  vPoint: TPointF;
begin
//  Result := False;
  vPoint := PointToLocal(APoint);
  Result := AFigure.BelongPointLocal(vPoint);

//  Result := AFigure.BelongPointLocal(APoint.X - TEngine2DObject(Owner).x, APoint.Y - TEngine2DObject(Owner).y)
{  if AFigure is TPolyFigure then
    Result := uIntersectorMethods.IsPointInPolygon(APoint, TPolyFigure(AFigure).AsType);

  if AFigure is TCircleFigure then
    Result := uIntersectorMethods.IsPointInCircle(APoint, TCircleFigure(AFigure).AsType);  }
end;

function TObjectShape.PointToLocal(const APoint: TPointF): TPointF;
var
  vRes: TVector;
  vScale, vRotate, vTranslate: TMatrix;
begin
//  with tEngine2DObject(Owner) do
//  begin
//    vTranslate := TMatrix.CreateTranslation(tEngine2DObject(Owner).x, tEngine2DObject(Owner).y )
    vRotate := TMatrix.CreateRotation(-tEngine2DObject(Owner).Rotate * pi180);
    vScale := TMatrix.CreateScaling(1 / tEngine2DObject(Owner).ScaleX, 1 / tEngine2DObject(Owner).ScaleY);
    Result := TVector.Create(APoint.X - tEngine2DObject(Owner).x, APoint.y - tEngine2DObject(Owner).y) * vScale * vRotate;
 // end;


  //vScale :=
//X = x0 + (x - x0) * cos(a) - (y - y0) * sin(a);
//Y = y0 + (y - y0) * cos(a) + (x - x0) * sin(a);
{  with tEngine2DObject(Owner) do
  Result := PointF(
    (AX * ScaleX - x) * Cos(Rotate * pi180)
      -
    (AY * ScaleY - y) * Sin(Rotate * pi180)
      - x
      ,
    (AY * ScaleY - y) * Cos(Rotate * pi180)
      +
    (AX * ScaleX - x) * Sin(Rotate * pi180)
  ); }
end;

function TObjectShape.RemoveFigure(const AIndex: Integer): TFigure;
begin

end;

{procedure TObjectShape.Recalc;
var
  i, vN: Integer;
begin
  vN := FFigures.Count - 1;
  for i := 0 to vN do
    FFigures[i].Position := TEngine2DObject(FOwner).Position;
end;  }

{procedure TObjectShape.SetFigure(Index: Integer; const Value: TFigure);
begin
  FFigures[Index] := Value;
end;}

procedure TObjectShape.SetSize(const Value: Single);
begin
  FSize := Value;
end;

(* function TObjectShape.ToWorldCoord(const AFigure: TFigure): TFigure;
var
  i, vN: Integer;
begin

 { Result := AFigure.Clone.FastMigration(
    PointF(TEngine2DObject(Owner).x,  TEngine2DObject(Owner).y),
    PointF(TEngine2DObject(Owner).ScaleX,  TEngine2DObject(Owner).ScaleY),
    TEngine2DObject(Owner).Rotate
  );  }
  vN := Length(FOriginFigures) - 1;
  for i := 0 to vN do
  begin
    FCalcedFigures[i].Assign(FOriginFigures[i]);
    FCalcedFigures[i].FastMigration(
      PointF(TEngine2DObject(Owner).ScaleX,  TEngine2DObject(Owner).ScaleY),
      TEngine2DObject(Owner).Rotate
    );
{    FFigures[i].Scale(PointF(tEngine2DObject(FOwner).ScaleX, tEngine2DObject(FOwner).ScaleY));
    FFigures[i].Rotate(tEngine2DObject(FOwner).Rotate);
    FFigures[i].Translate(PointF(tEngine2DObject(FOwner).X, tEngine2DObject(FOwner).Y));}
  end;


  {TEngine2DObject(Owner).x -  TEngine2DObject(Owner).w*0.5,
    TEngine2DObject(Owner).y -  TEngine2DObject(Owner).h*0.5,
    TEngine2DObject(Owner).x +  TEngine2DObject(Owner).w*0.5,
    TEngine2DObject(Owner).y +  TEngine2DObject(Owner).h*0.5),   }

end;   *)

function TObjectShape.UnderTheMouse(const MouseX, MouseY: double): boolean;
var
  i: Integer;
begin
  //ToWorldCoord(Nil);
  for i := 0 to High(FFigures) do
  begin
    if IsPointInFigure(PointF(MouseX, MouseY), Self.FFigures[i]) then
      Exit(True);

//    uIntersectorMethods.
   { if FCalcedFigures[i].BelongPoint(MouseX, MouseY) then
      Exit(True);  }
  end;

  Result := False;
end;

end.





