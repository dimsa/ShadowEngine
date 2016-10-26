unit uEngine2DObjectShape;

interface

uses
  System.Types, System.Generics.Collections, System.UITypes, System.Math,
  {$I 'Utils\DelphiCompatability.inc'}
  uEngine2DClasses, uIntersectorMethods, uGeometryClasses, uNewFigure;

type
  // С точки зрения Box2D это боди с несколькими Fixtures
  TObjectShape = class
  private
    FFigures: TArray<TNewFigure>;
    FParent: Pointer;
    FOwner: Pointer;
    FSize: Single;
    FMaxRadius: Single;
    function GetFigure(Index: Integer): TNewFigure;
    function GetCount: Integer;
    procedure SetSize(const Value: Single);
    function PointToLocal(const APoint: TPointF): TPointF;
  public
    property Owner: Pointer read FOwner write FOwner; // Хозяин фигуры TEngine2DObject
    property Figures[Index: Integer]: TNewFigure read GetFigure; default;
    property Count: Integer read GetCount;
    property Size: Single read FSize write SetSize; // Испльзуется для быстрых расчетов
    property MaxRadius: Single read FMaxRadius;
    procedure Draw; // Рисует форму фигуры
    function IsPointInFigure(const APoint: TPointF; const AFigure: TNewFigure): Boolean;
    function AddFigure(AFigure: TNewFigure): Integer;
    function RemoveFigure(const AIndex: Integer): TNewFigure;
    function UnderTheMouse(const MouseX, MouseY: double): boolean; virtual; // Говорит, попала ли мышь в круг спрайта. Круг с диаметром - диагональю прямоугольника спрайта
    function IsIntersectWith(AShape: TObjectShape): Boolean; // Пересекаеися ли с конкретной фигурой
    function Intersections: TList<TObjectShape>; // Список всех пересечений с другими фигурами
    procedure ToGlobal;
    procedure ToLocal;
    constructor Create;
    destructor Destroy; override;
  const
    pi180 = 0.01745329251;
  end;

implementation

uses
  uEngine2DObject;

{ TObjectShape }

function TObjectShape.AddFigure(AFigure: TNewFigure): Integer;
begin
  SetLength(FFigures, Length(FFigures) + 1);
  FFigures[High(FFigures)] := AFigure;

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
  vFigure: TNewFigure;
  vSpr: tEngine2DObject;
begin
  // it's only for debug, so it is not very fast
  //ToGlobal;

  if Length(Self.FFigures) > 0 then
  begin

    for vFigure in FFigures do
    begin
      vFigure.Reset;

      vSpr := TEngine2DObject(Owner);

     vFigure.TempTranslate(PointF(vSpr.X, vSpr.Y));

      vFigure.Draw(TEngine2DObject(Owner).Image.Bitmap.Canvas);
    end;
  end
  else begin
    TEngine2DObject(Owner).Image.Bitmap.Canvas.Fill.Color := TAlphaColorRec.Red;
    TEngine2DObject(Owner).Image.Bitmap.Canvas.FillEllipse
      (RectF(
        TEngine2DObject(Owner).X - TEngine2DObject(Owner).w * 0.5,
        TEngine2DObject(Owner).Y - TEngine2DObject(Owner).h * 0.5,
        TEngine2DObject(Owner).X + TEngine2DObject(Owner).w * 0.5,
        TEngine2DObject(Owner).Y + TEngine2DObject(Owner).h * 0.5),
        TEngine2DObject(Owner).Opacity * 0.5);
  end;

  // Center of the all figures. Base point of sprite
  with TEngine2DObject(Owner) do
  begin
    Image.Bitmap.Canvas.Fill.Color := TAlphaColorRec.Yellow;
    Image.Bitmap.Canvas.FillEllipse(RectF(X - 3, Y - 3, X + 3, Y + 3), 1);

    // Max radius.If circles of this radius collides then engine start test colliding in parts
    Image.Bitmap.Canvas.Fill.Color := TAlphaColorRec.Pink;
    Image.Bitmap.Canvas.FillEllipse(RectF(X - MaxRadius / ScaleX, Y - MaxRadius / ScaleY, X + MaxRadius / ScaleX, Y + MaxRadius / ScaleY), 0.5);
  end;
end;

function TObjectShape.GetCount: Integer;
begin
  Result := Length(FFigures);
end;

function TObjectShape.GetFigure(Index: Integer): TNewFigure;
begin
  Result := FFigures[Index];
end;

function TObjectShape.Intersections: TList<TObjectShape>;
begin
  Result := nil;
end;

function TObjectShape.IsIntersectWith(AShape: TObjectShape): Boolean;
var
  i, j, vN, vL: Integer;
begin
  vN := Self.Count - 1;
  vL := AShape.Count - 1;
  AShape.ToGlobal;
  Self.ToGlobal;
  if SqrDistance(
    tEngine2DObject(Self.Owner).Center,
    tEngine2DObject(AShape.Owner).Center
  ) < Sqr(AShape.MaxRadius + Self.MaxRadius) then
  begin
    for i := 0 to vN do
    begin
      for j := 0 to vL do
    { TODO : Add Fast Intersection }
//      if FFigures[i].FastIntersectWith(AShape[j]) then
      // Owner Center!!!!
        if FFigures[i].IsIntersectWith(AShape[j]) then
          Exit(True);
    end;
  end;
  Result := False;
end;

function TObjectShape.IsPointInFigure(const APoint: TPointF; const AFigure: TNewFigure): Boolean;
var
  vPoint: TPointF;
begin
  vPoint := PointToLocal(APoint);
  Result := AFigure.BelongPointLocal(vPoint);
end;

function TObjectShape.PointToLocal(const APoint: TPointF): TPointF;
var
  vScale, vRotate: TMatrix;
begin
    vRotate := TMatrix.CreateRotation(-tEngine2DObject(Owner).Rotate * pi180);
    vScale := TMatrix.CreateScaling(1 / tEngine2DObject(Owner).ScaleX, 1 / tEngine2DObject(Owner).ScaleY);
    Result := PointF(
        APoint.X - tEngine2DObject(Owner).X,
        APoint.Y - tEngine2DObject(Owner).Y
      ) * vScale * vRotate;
end;

function TObjectShape.RemoveFigure(const AIndex: Integer): TNewFigure;
var
  i, vN: Integer;
begin
  Result := FFigures[AIndex];
  vN := Length(FFigures) - 2;
  for i := AIndex to vN do
    FFigures[i] := FFigures[i + 1];

end;

procedure TObjectShape.SetSize(const Value: Single);
begin
  FSize := Value;
end;

procedure TObjectShape.ToGlobal;
var
  vFigure: TNewFigure;
begin
  FMaxRadius := 0;
  for vFigure in FFigures do
  begin
    vFigure.Reset;
    vFigure.TempScale(tEngine2DObject(Owner).ScalePoint);
    vFigure.TempRotate(tEngine2DObject(Owner).Rotate);
    vFigure.TempTranslate(tEngine2DObject(Owner).Center);
    if vFigure.TempMaxRadius > FMaxRadius then
      FMaxRadius := vFigure.TempMaxRadius;
  end;
end;

procedure TObjectShape.ToLocal;
var
  vN, i: Integer;
begin
  vN := Length(FFigures) - 1;
  for i := 0 to vN do
    FFigures[i].Reset;
end;

function TObjectShape.UnderTheMouse(const MouseX, MouseY: double): boolean;
var
  i: Integer;
begin
  for i := 0 to High(FFigures) do
  begin
    if IsPointInFigure(PointF(MouseX, MouseY), Self.FFigures[i]) then
      Exit(True);
  end;

  Result := False;
end;

end.
