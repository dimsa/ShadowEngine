unit uRawShapes;

interface

uses
  System.Types, System.Math, uGeometryClasses, System.Generics.Collections;

type
  TRawShape = class abstract
  protected
    FData: TArray<TPointF>;
    FType: TFigureType;
  public
    function GetData: TArray<TPointF>;
    function FigureType: TFigureType; virtual; abstract;
    function Clone: TRawShape;
  end;

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
begin
  Result := TRawShape.Create;
  Result.FData := GetData;
  Result.FType := FType;
end;

function TRawShape.GetData: TArray<TPointF>;
var
  i: Integer;
begin
  SetLength(Result, Length(FData));
  for i := 0 to High(FData) do
    Result[i] := TPointF.Create(FData[i].X, FData[i].Y)
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
  SetLength(FData, Length(APoly));
  for i := 0 to High(APoly) do
    FData[i] := TPointF.Create(FData[i].X, FData[i].Y)
end;

function TRawPoly.FigureType: TFigureType;
begin
  Result := TFigureType.ftPoly;
end;

end.
