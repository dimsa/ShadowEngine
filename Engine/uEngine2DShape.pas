unit uEngine2DShape;

interface

uses
  System.Types, FMX.Graphics, FMX.Objects, FMX.Types, System.UITypes,
  uEngine2DClasses, uEngine2DObject, uEngine2DResources,
  uEngine2DUnclickableObject;

type
 TEngine2DShape = class(tEngine2DObject)
  private
    procedure SetFigureRect(const Value: TRectF);
  protected
    FFigureRect: TRectF;
    FPen: TStrokeBrush;
    FBrush: TBrush;
    fWhalf, fHHalf: single; // половина ширины и высоты
    function GetW: single; override;
    function GetH: single; override;
  public
    property Pen: TStrokeBrush read FPen write FPen;
    property Brush: TBrush read FBrush write FBrush;
    property FigureRect: TRectF read FFigureRect write SetFigureRect;

    constructor Create(AParent: pointer); override;
    destructor Destroy; override;
  end;

  TFillRect = class(TEngine2DShape)
  public
    procedure Repaint; override;
  end;

  TFillEllipse = class(TEngine2DShape)
  public
    procedure Repaint; override;
  end;

implementation

{ TEngine2DShape }

constructor TEngine2DShape.Create(AParent: pointer);
begin
  inherited;
  FFigureRect := RectF(-50, -50, 50, 50);

  FPen := TStrokeBrush.Create(TBrushKind.Solid, TAlphaColorRec.Black);
  FPen.Thickness := 1;
  FPen.Color := TAlphaColorRec.Black;

  FBrush := TBrush.Create(TBrushKind.Solid, TAlphaColorRec.White);
  FBrush.Color := TAlphaColorRec.White;
end;

destructor TEngine2DShape.Destroy;
begin
  FPen.Free;
  FBrush.Free;
  inherited;
end;

function TEngine2DShape.GetH: single;
begin
  Result := fFigureRect.Height;
end;

function TEngine2DShape.GetW: single;
begin
  Result := fFigureRect.Width;
end;

procedure TEngine2DShape.SetFigureRect(const Value: TRectF);
begin
  FFigureRect := Value;
  fWhalf := Value.Width * 0.5;
  fHhalf := Value.Height * 0.5;
end;

{ TFillEllipse }

procedure TFillEllipse.Repaint;
begin
  FImage.Bitmap.Canvas.Fill.Assign(FBrush);
  FImage.Bitmap.Canvas.Stroke.Assign(FPen);

  FImage.Bitmap.Canvas.FillEllipse(
    RectF(
      FwHalf * CJustifyPoints[Justify].Left,
      FhHalf * CJustifyPoints[Justify].Top,
      FwHalf * CJustifyPoints[Justify].Right,
      FhHalf * CJustifyPoints[Justify].Bottom),
    fOpacity
   );

  inherited;
end;

{ TFillRect }

procedure TFillRect.Repaint;
begin
  FImage.Bitmap.Canvas.Fill.Assign(FBrush);
  FImage.Bitmap.Canvas.Stroke.Assign(FPen);

  FImage.Bitmap.Canvas.FillRect(
    RectF(
      FwHalf * CJustifyPoints[Justify].Left,
      FhHalf * CJustifyPoints[Justify].Top,
      FwHalf * CJustifyPoints[Justify].Right,
      FhHalf * CJustifyPoints[Justify].Bottom),
    0,
    0,
    [],
    fOpacity,
    FMX.Types.TCornerType.Round
  );

  inherited;
end;

end.
