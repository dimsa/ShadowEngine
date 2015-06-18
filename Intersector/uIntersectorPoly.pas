unit uIntersectorPoly;

interface

uses
  System.Types, System.Math, FMX.Objects, System.UITypes,
  {$IFDEF VER290} System.Math.Vectors, {$ENDIF}
  uIntersectorFigure, uIntersectorClasses;

type
  TPolyFigure = class(TFigure)
  private
    //function BelongPoint(const AX, AY: Single): Boolean;
    procedure FastMigration(const AScale: TPointF; const ARotate: Single);
  protected
    FPolygon: TPolygon;
    function CalcedPoly: TPolygon;
  public
    property AsType: TPolygon read FPolygon;
    procedure AddPoint(const APoint: TPointF);
    procedure Rotate(const AValue: Single); override;
    procedure Scale(const AValue: TPointF); override;
//    function BelongPointLocal(const AX, AY: Single): Boolean; overload; override;
    function BelongPointLocal(const APoint: TPointF): Boolean; overload; override;
    procedure Draw(AImage: TImage); override;

    procedure Assign(const AFigure: TFigure); override;
    function Clone: TFigure; override;
  end;

implementation

uses
  uIntersectorMethods;


{ TPolyFigure }

procedure TPolyFigure.AddPoint(const APoint: TPointF);
begin
  SetLength(FPolygon, Length(FPolygon) + 1);
  FPolygon[High(FPolygon)] := APoint;
end;

procedure TPolyFigure.Assign(const AFigure: TFigure);
var
  vPoly: TPolygon;
  i, vN: Integer;
begin
  inherited;
  vN := Length(TPolyFigure(AFigure).AsType);
  SetLength(vPoly, vN);
  for i := 0 to vN - 1 do
    vPoly[i] := TPolyFigure(AFigure).AsType[i];

  Self.FPolygon := vPoly; //TPolyFigure(AFigure).AsType;
end;

function TPolyFigure.BelongPointLocal(const APoint: TPointF): Boolean;
begin
  Result := uIntersectorMethods.IsPointInPolygon(APoint, CalcedPoly);
end;

{function TPolyFigure.BelongPointLocal(const AX, AY: Single): Boolean;
begin
  Result := uIntersectorMethods.IsPointInPolygon(PointF(AX, AY), CalcedPoly);
end;}

function TPolyFigure.CalcedPoly: TPolygon;
var
  i, vN: Integer;
  vPoly: TPolygon;
begin
  vN := Length(FPolygon) ;
  SetLength(vPoly, vN);
  for i := 0 to vN-1 do
    vPoly[i] := FPolygon[i] + Self.FCenter;
  Result := vPoly;
end;

function TPolyFigure.Clone: TFigure;
var
  vRes: TPolyFigure;
begin
  vRes := TPolyFigure.Create;
  vRes.Assign(Self);
  Result := vRes;
end;

procedure TPolyFigure.Draw(AImage: TImage);
begin
  inherited;
  AImage.Bitmap.Canvas.Fill.Color := TAlphaColorRec.Blue;
  AImage.Bitmap.Canvas.FillPolygon(CalcedPoly, 0.75);
end;

procedure TPolyFigure.FastMigration(const AScale: TPointF; const ARotate: Single);
var
  i, vN: Integer;
  vTemp: Single;
begin
//  inherited;

  Self.Scale(AScale);
  Self.Rotate(ARotate);
{  vN := Length(FPolygon) - 1;
  for i := 0 to vN do
  begin
//  FCircle.Radius := FCircle.Radius * AScale.X;
    FPolygon[i] := (FPolygon[i] - FCenter) * AScale;
    vTemp := FPolygon[i].X;
    FPolygon[i].X := (FPolygon[i].X - FCenter.X) * Cos(ARotate * pi180) - (FPolygon[i].Y  - FCenter.Y) * Cos(ARotate * pi180);
    FPolygon[i].Y := (vTemp - FCenter.X) * Sin(ARotate * pi180) + (FPolygon[i].Y - FCenter - Y) * Cos(ARotate * pi180);
//    FCenter := ATranslate;

  end;  }

 { FCenter.X := FCenter.X * Cos(ARotate * pi180) - FCenter.Y * Cos(ARotate * pi180);
  FCenter.Y := FCenter.X * Sin(ARotate * pi180) + FCenter.Y * Cos(ARotate * pi180);

  FCircle.X := FCenter.X + ATranslate.X;
  FCircle.Y := FCenter.Y + ATranslate.Y;   }
end;

procedure TPolyFigure.Rotate(const AValue: Single);
var
  i, vN: Integer;
  vTemp: Single;
begin
  vN := Length(FPolygon) - 1;
  for i := 0 to vN do
  begin
    vTemp := FPolygon[i].X;
  {  FPolygon[i].X := (FPolygon[i].X) * Cos(AValue * pi180) - (FPolygon[i].Y) * Cos(AValue * pi180);
    FPolygon[i].Y := (vTemp) * Sin(AValue * pi180) + (FPolygon[i].Y) * Cos(AValue * pi180);}
    FPolygon[i].X := (FPolygon[i].X - FCenter.X) * Cos(AValue * pi180) - (FPolygon[i].Y  - FCenter.Y) * Sin(AValue * pi180);
    FPolygon[i].Y := (vTemp - FCenter.X) * Sin(AValue * pi180) + (FPolygon[i].Y - FCenter.Y) * Cos(AValue * pi180);
  end;

 { Self.FCenter := PointF(
    Center.X * Cos(AValue) - Center.Y * Cos(AValue),
    Center.Y * Sin(AValue) + Center.Y * Cos(AValue)
  );}
end;

procedure TPolyFigure.Scale(const AValue: TPointF);
var
  i, vN: Integer;
begin
  vN := Length(FPolygon) - 1;
  for i := 0 to vN do
    FPolygon[i] := (FPolygon[i] - FCenter) * AValue;
end;

{procedure TPolyFigure.Translate(const AValue: TPointF);
begin
  Self.FCenter := //Self.FCenter +
  AValue;
end;}

end.
