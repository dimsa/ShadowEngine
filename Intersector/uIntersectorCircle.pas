unit uIntersectorCircle;

interface

uses
  System.Types, FMX.Objects, UITypes, System.Math,
  uIntersectorFigure, uIntersectorClasses;

type
  TCircleFigure = class(TFigure)
  private
    FCircle: TCircle;
    procedure SetRadius(const Value: Single);
  protected
    procedure SetCenterX(const Value: Single); override;
    procedure SetCenterY(const Value: Single); override;
    procedure SetCenter(const Value: TPointF); override;
    procedure CalculateMaxRadius; override;
  public
    property Radius: Single read FCircle.Radius write SetRadius;
    property AsType: TCircle read FCircle;

    procedure Rotate(const AValue: Single); override;
    procedure Scale(const AValue: TPointF); override;

   // function InGlobal(const AScale: TPointF; const ARotate: Single; const ATranslate: TPointF): TFigure; override;
//    procedure FastMigration(const AScale: TPointF; const ARotate: Single); override;
//    function BelongPointLocal(const AX, AY: Single): Boolean; override; // Тут координаты должны вводиться в местных координатах, т.е. например MouseX - Figure.X и т.д.
    function BelongPointLocal(const APoint: TPointF): Boolean; override;
    procedure Draw(AImage: TImage); override;

    procedure Assign(const AFigure: TFigure); override;
    function Clone: TFigure; override;
  //  function FigureRect: TRectF; override;
    constructor Create; override;
  end;

implementation

uses
  uIntersectorMethods;

{ TCircleFigure }

procedure TCircleFigure.Assign(const AFigure: TFigure);
begin
  inherited;
  Self.Radius := TCircleFigure(AFigure).Radius;
end;

function TCircleFigure.BelongPointLocal(const APoint: TPointF): Boolean;
begin
   Result := IsPointInCircle(APoint, FCircle);
end;

procedure TCircleFigure.CalculateMaxRadius;
begin
  Self.FMaxRadius :=
    Max(Self.Radius + Abs(Self.X), Self.Radius + Abs(Self.Y))
end;

function TCircleFigure.Clone: TFigure;
var
  vRes: TCircleFigure;
begin
  vRes := TCircleFigure.Create;
  vRes.Assign(Self);
  Result := vRes;
end;

constructor TCircleFigure.Create;
begin
  inherited;
  FCenter := PointF(0,0); //TPointF.Zero;
  FCircle.Radius := 0;
 { SetLength(FPoints, 2);
  // 0 - это центр окружности. 1 - Это радиус
  FPoints[0].X := 0;
  FPoints[0].Y := 0;
  FPoints[1].X := 0;
  FPoints[1].Y := 0;   }
end;

procedure TCircleFigure.Draw(AImage: TImage);
begin
  inherited;
  AImage.Bitmap.Canvas.Fill.Color := TAlphaColorRec.Aqua;
  AImage.Bitmap.Canvas.FillEllipse(
    RectF(
      FCenter.X - FCircle.Radius,
      FCenter.Y - FCircle.Radius,
      FCenter.X + FCircle.Radius,
      FCenter.Y + FCircle.Radius),
      0.75
    );
end;

{procedure TCircleFigure.FastMigration(const AScale: TPointF; const ARotate: Single);
begin
  Self.Scale(AScale);
  Self.Rotate(ARotate);
  {FCircle.Radius := FCircle.Radius * AScale.X;

  FCenter.X := FCenter.X * Cos(ARotate * pi180) - FCenter.Y * Cos(ARotate * pi180);
  FCenter.Y := FCenter.X * Sin(ARotate * pi180) + FCenter.Y * Cos(ARotate * pi180);    }

{  FCircle.X := FCenter.X + ATranslate.X;
  FCircle.Y := FCenter.Y + ATranslate.Y; }
//end;

{function TCircleFigure.FigureRect: TRectF;
begin
  Result := RectF(
    Self.X - Radius,
    Self.Y - Radius,
    Self.X + Radius,
    Self.Y + Radius
  );
end;  }

{function TCircleFigure.InGlobal(const AScale: TPointF; const ARotate: Single; const ATranslate: TPointF): TFigure;
var
  vRes: TFigure;
begin
  vRes := TCircleFigure.Create;
  vRes.Assign(Self);
  vRes.AutoCalcMaxRadius := False;
  vRes.Scale(AScale);
  vRes.Rotate(ARotate);
  vRes.Center := ATranslate;
//  vRes.Translate(ATranslate);

  Result := vRes;
end;  }

procedure TCircleFigure.Rotate(const AValue: Single);
var
  vLength: Single;
begin
  FCenter.X := FCenter.X * Cos(AValue) - FCenter.Y * Cos(AValue);
  FCenter.Y := FCenter.X * Sin(AValue) + FCenter.Y * Cos(AValue);

  FCircle.X := FCenter.X;
  FCircle.Y := FCenter.Y;
  inherited;
end;

procedure TCircleFigure.Scale(const AValue: TPointF);
begin
  FCircle.Radius := FCircle.Radius * AValue.X;
  inherited;
end;

procedure TCircleFigure.SetCenter(const Value: TPointF);
begin
  FCircle.X := Value.X;
  FCircle.Y := Value.Y;
  inherited;
end;

procedure TCircleFigure.SetCenterX(const Value: Single);
begin
  inherited;
  FCircle.X := Value;
end;

procedure TCircleFigure.SetCenterY(const Value: Single);
begin
  inherited;
  FCircle.Y := Value;
end;

procedure TCircleFigure.SetRadius(const Value: Single);
begin
  FCircle.Radius := Value;
  if Self.AutoCalcMaxRadius then
    CalculateMaxRadius;
end;

end.
