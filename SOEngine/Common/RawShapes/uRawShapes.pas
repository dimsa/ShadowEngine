unit uRawShapes;

interface

uses
  System.Types, System.Math, uGeometryClasses, System.Generics.Collections, System.SysUtils;

type
  TRawShape = class abstract
  protected
    FData: TArray<TPointF>;
    FType: TFigureType;
  public
    function GetData: TArray<TPointF>;
    function FigureType: TFigureType; virtual; abstract;
    function IsEqualTo(const AShape: TRawShape): Boolean; virtual;
    function Clone: TRawShape;
    constructor Create;
  end;

  TRawShapeClass = class of TRawShape;

  TRawCircle = class(TRawShape)
  public
    function FigureType: TFigureType; override;
    constructor Create(const AX, AY, AR: Single); overload;
  end;
  TRawPoly = class(TRawShape)
  public
    function Count: Integer;
    function FigureType: TFigureType; override;
    constructor Create(const APoly: TArray<TPointF>); overload;
  end;

implementation

{ TRawFigure }

function TRawShape.Clone: TRawShape;
var
  vClass: TRawShapeClass;
begin
  vClass := TRawShapeClass(Self.ClassType);
  Result := vClass.Create;
  Result.FData := GetData;
  Result.FType := FType;
end;

constructor TRawShape.Create;
begin
  FType := FigureType;
end;

function TRawShape.GetData: TArray<TPointF>;
var
  i: Integer;
begin
  SetLength(Result, Length(FData));
  for i := 0 to High(FData) do
    Result[i] := TPointF.Create(FData[i].X, FData[i].Y)
end;

function TRawShape.IsEqualTo(const AShape: TRawShape): Boolean;
var
  i: Integer;
begin
  if (Self.FType <> AShape.FType) then
    Exit(False);
  if (Length(Self.FData) <> Length(AShape.FData)) then
    Exit;

  for i := 0 to High(FData) do
    if (Self.FData[i] <> AShape.FData[i]) then

      Exit(False);

  Result := True;
end;

{ TRawCircle }

{constructor TRawCircle.Create(const ACircle: TCircle);
begin
  SetLength(FData, 2);
  FData[0] := TPointF.Create(ACircle.X, ACircle.Y);
  FData[1] := TPointF.Create(ACircle.Radius, 0);
end; }

constructor TRawCircle.Create(const AX, AY, AR: Single);
begin
  inherited Create;
  SetLength(FData, 2);
  FData[0] := TPointF.Create(AX, AY);
  FData[1] := TPointF.Create(AR, 0);
end;

function TRawCircle.FigureType: TFigureType;
begin
  Result := TFigureType.ftCircle;
end;

{ TRawPoly }

{constructor TRawPoly.Create(const APoly: TPolygon);
var
  i: Integer;
begin
  SetLength(FData, Length(APoly));
  for i := 0 to High(APoly) do
    FData[i] := TPointF.Create(FData[i].X, FData[i].Y)
end;  }

function TRawPoly.Count: Integer;
begin
  Result := Length(FData);
end;

constructor TRawPoly.Create(const APoly: TArray<TPointF>);
var
  i: Integer;
begin
  inherited Create;
  SetLength(FData, Length(APoly));
  for i := 0 to High(APoly) do
    FData[i] := TPointF.Create(APoly[i].X, APoly[i].Y)
end;

function TRawPoly.FigureType: TFigureType;
begin
  Result := TFigureType.ftPoly;
end;

end.
