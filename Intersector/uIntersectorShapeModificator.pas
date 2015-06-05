unit uIntersectorShapeModificator;

interface

uses
  System.Types;

type
  TShapeModificator = class abstract
  public
    procedure Apply(var APoint: TPointF); virtual; abstract;
    constructor Create; virtual; abstract;
  end;

  TTranslateModificator = class(TShapeModificator)
  private
    FTranslate: TPointF;
  public
    property X: Single read FTranslate.X write FTranslate.X;
    property Y: Single read FTranslate.Y write FTranslate.Y;
    property Translate: TPointF read FTranslate write FTranslate;
    procedure Apply(var APoint: TPointF); override;
    constructor Create; override;
  end;

  TScaleModificator = class(TShapeModificator)
  private
    FScale: TPointF;
  public
    property ScaleX: Single read FScale.X write FScale.X;
    property ScaleY: Single read FScale.Y write FScale.Y;
    property Scale: TPointF read FScale write FScale;
    procedure Apply(var APoint: TPointF); override;
    constructor Create; override;
  end;

  TRotateModificator = class(TShapeModificator)
  private
    FRotate: Single;
  public
    property Rotate: Single read FRotate write FRotate;
    procedure Apply(var APoint: TPointF); override;
    constructor Create; override;
  end;

implementation

{ TTranslateModificator }

procedure TTranslateModificator.Apply(var APoint: TPointF);
begin
  APoint := APoint + FTranslate;
end;

constructor TTranslateModificator.Create;
begin
  FTranslate.X := 0;
  FTranslate.Y := 0;
end;

{ TScaleModificator }

procedure TScaleModificator.Apply(var APoint: TPointF);
begin
  APoint := APoint * FScale;
end;

constructor TScaleModificator.Create;
begin
  FScale.X := 1;
  FScale.Y := 1;
end;

{ TRotateModificator }

procedure TRotateModificator.Apply(var APoint: TPointF);
var
  vR: Single;
begin
  vR := Sqrt(APoint.X*APoint.X + APoint.Y*APoint.Y);
  APoint.X := vR * Cos(FRotate);
  APoint.Y := vR * Sin(FRotate);
end;

constructor TRotateModificator.Create;
begin
  FRotate := 0;
end;

end.
