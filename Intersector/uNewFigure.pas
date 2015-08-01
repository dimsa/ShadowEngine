unit uNewFigure;

interface

uses
  System.Generics.Collections, System.Types, System.Classes,
  {$IFDEF VER290} System.Math.Vectors, {$ENDIF} System.Math,
  uIntersectorClasses, uIntersectorMethods, FMX.Objects, System.UITypes;

type
  TNewFigure = class
  private
    FKind: Byte;
    FData: TArray<TPointF>; // Оригинальная фигура. Задается через SetData
    FTemp: TArray<TPointF>; // Темповые координаты. Задаются через Temp методы
    FTempMaxRadius: Single;
    procedure RecalcMaxRadius;
    function GetCircle: uIntersectorClasses.TCircle;
    function GetPoly: TPolygon; // Вызывается в SetData
  public
    property Kind: Byte read FKind; // Тип. Круг или полигон пока что
    property Temp: TArray<TPointF> read FTemp write FTemp;
    property TempMaxRadius: Single read FTempMaxRadius; // Временный радиус. Т.е. с учетом масштаба
    property AsCircle: uIntersectorClasses.TCircle read GetCircle;
    property AsPoly: TPolygon read GetPoly;

    procedure Reset; // Сбрасывает темповые координат на начальные.
    procedure SetData(const vData: TArray<TPointF>); overload;// Трактует данные в зависимости от своего типа
    procedure SetData(const vData: TPolygon); overload;// Трактует данные в зависимости от своего типа
    procedure SetData(const vData: uIntersectorClasses.TCircle); overload;// Трактует данные в зависимости от своего типа
    procedure AddPoint(const APoint: TPointF); // Добавляет точку фигуре и пересчитывает её

    procedure TempTranslate(const APoint: TPointF);
    procedure TempRotate(const AAngle: Single);
    procedure TempScale(const APoint: TPointF);

    function IsIntersectWith(const AFigure: TNewFigure): Boolean;
    function FastIntersectWith(const AFigure: TNewFigure): Boolean;
    function BelongPointLocal(const APoint: TPointF): Boolean;
    procedure Draw(AImage: TImage);

    constructor Create(const AKind: Byte);
    constructor CreatePoly;
    constructor CreateCircle;
  const
    cfCircle = 1;
    cfPoly = 2;
  end;



implementation

{ TNewFigure }

procedure TNewFigure.AddPoint(const APoint: TPointF);
begin
  SetLength(FData, Length(FData) + 1);
  FData[High(FData)] := APoint;
  Reset;
end;

function TNewFigure.BelongPointLocal(const APoint: TPointF): Boolean;
begin
  Reset;
  case FKind of
    cfCircle: Exit(uIntersectorMethods.IsPointInCircle(APoint, AsCircle));
    cfPoly: Exit(uIntersectorMethods.IsPointInPolygon(APoint, AsPoly));
  end;
end;

constructor TNewFigure.Create(const AKind: Byte);
begin
  FKind := AKind;
  SetLength(FData, 0);
  FTemp := FData;
end;

constructor TNewFigure.CreateCircle;
begin
  Self.Create(cfCircle);
end;

constructor TNewFigure.CreatePoly;
begin
  Self.Create(cfPoly);
end;

procedure TNewFigure.Draw(AImage: TImage);
begin
  case FKind of
    cfCircle:
    begin
      AImage.Bitmap.Canvas.Fill.Color := TAlphaColorRec.Aqua;
      AImage.Bitmap.Canvas.FillEllipse(
        RectF(
        FTemp[0].X - FTemp[1].X,
        Temp[0].Y - FTemp[1].X,
        Temp[0].X + FTemp[1].X,
        Temp[0].Y + FTemp[1].X),
        0.75
      );
    end;
    cfPoly:
    begin
      AImage.Bitmap.Canvas.Fill.Color := TAlphaColorRec.Blue;
      AImage.Bitmap.Canvas.FillPolygon(AsPoly, 0.75);
    end;
  end;
end;

function TNewFigure.FastIntersectWith(const AFigure: TNewFigure): Boolean;
begin
  Result :=
    (Sqr(Self.FTemp[0].X - AFigure.Temp[0].Y) +
    Sqr(Self.FTemp[0].Y - AFigure.Temp[0].X)) <
    Sqr(AFigure.TempMaxRadius + Self.TempMaxRadius);
end;

function TNewFigure.GetCircle: uIntersectorClasses.TCircle;
begin
  Result.X := FTemp[0].X;
  Result.Y := FTemp[0].Y;
  Result.Radius := FTemp[1].X;
end;

function TNewFigure.GetPoly: TPolygon;
begin
  Result := @FTemp;
  SetLength(Result, Length(FTemp));
end;

function TNewFigure.IsIntersectWith(const AFigure: TNewFigure): Boolean;
begin
  case FKind of
    cfCircle:
      case AFigure.Kind of
        cfCircle: Exit(CircleCircleCollide(Self.AsCircle, AFigure.AsCircle));
        cfPoly: Exit(CirclePolyCollide(AFigure.AsPoly, Self.AsCircle));
      end;
    cfPoly:
      case AFigure.Kind of
        cfCircle: Exit(CirclePolyCollide(Self.AsPoly, AFigure.AsCircle));
        cfPoly: Exit(PolyPolyCollide(Self.AsPoly, AFigure.AsPoly));
      end;
  end;
  Result := False;
end;

procedure TNewFigure.RecalcMaxRadius;
var
  i, vN: Integer;
begin
  case FKind of
    cfCircle: FTempMaxRadius := Distance(FData[0]) + FData[1].X;
    cfPoly:
    begin
      vN := Length(FData) - 1;
      FTempMaxRadius := Distance(FData[0]);
      for i := 1 to vN do
        FTempMaxRadius := Max(Distance(FData[i]), FTempMaxRadius)
    end;
  end;
end;

procedure TNewFigure.Reset;
begin
  FTemp := FData;
  RecalcMaxRadius;
end;

procedure TNewFigure.SetData(const vData: TPolygon);

begin
  SetLength(FData, Length(vData));
  FData := @vData[0];
 // SetLength(FData, Length(vData));
  FTemp := FData;
  Reset;
end;

procedure TNewFigure.SetData(const vData: uIntersectorClasses.TCircle);
begin
  if Length(FData) < 2 then
    SetLength(FData, 2);

  FData[0] := PointF(vData.X, vData.Y);
  FData[1] := PointF(vData.Radius, vData.Radius);
  FTemp := FData;
  Reset;
end;

procedure TNewFigure.SetData(const vData: TArray<TPointF>);
begin
  FData := vData;
  Reset;
end;

procedure TNewFigure.TempRotate(const AAngle: Single);
var
  i, vN: Integer;
  vTemp: Single;
begin
  vN := Length(FTemp);

  for i := 0 to vN do
  begin
    vTemp := FTemp[i].X;
    FTemp[i].X := (FTemp[i].X) * Cos(AAngle * pi180) - (FTemp[i].Y) * Sin(AAngle * pi180);
    FTemp[i].Y := (vTemp) * Sin(AAngle * pi180) + (FTemp[i].Y) * Cos(AAngle * pi180);
  end;

end;

procedure TNewFigure.TempScale(const APoint: TPointF);
var
  i, vN: Integer;
begin
  vN := Length(FTemp) - 1;
  for i := 0 to vN do
    FTemp[i] := FTemp[i] * APoint;

  RecalcMaxRadius;
end;

procedure TNewFigure.TempTranslate(const APoint: TPointF);
var
  i, vN: Integer;
begin
  vN := Length(FTemp) - 1;
  for i := 0 to vN do
    FTemp[i] := FTemp[i] + APoint;
end;

end.
