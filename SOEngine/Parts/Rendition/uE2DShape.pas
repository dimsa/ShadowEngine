unit uE2DShape;

interface

uses
  System.Types, FMX.Graphics, FMX.Types, FMX.Objects, System.UITypes,
  uE2DRendition, uSoObject;

type
  TEngine2DShape = class(TEngine2DRendition)
  private
    procedure SetBrush(const Value: TBrush);
    procedure SetFigureRect(const Value: TRectF);
    procedure SetPen(const Value: TStrokeBrush);
  protected
    FFigureRect: TRectF;
    FPen: TStrokeBrush;
    FBrush: TBrush;
    FWidth, FHeight, FWHalf, FHHalf: single; // Caching for quick draw
    function GetHeight: Single; override;
    function GetWidth: Single; override;
  public
    property Pen: TStrokeBrush read FPen write SetPen;
    property Brush: TBrush read FBrush write SetBrush;
    property FigureRect: TRectF read FFigureRect write SetFigureRect;
    constructor Create(const ASubject: TSoObject; const AImage: TImage);
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

uses
  uEngine2DClasses;

{ TEngine2DShape }

constructor TEngine2DShape.Create(const ASubject: TSoObject; const AImage: TImage);
begin
  inherited Create(ASubject, AImage);

  FigureRect := RectF(-50, -50, 50, 50);

  FPen := TStrokeBrush.Create(TBrushKind.Solid, TAlphaColorRec.Black);
  FPen.Thickness := 1;
  FPen.Color := TAlphaColorRec.Black;

  FBrush := TBrush.Create(TBrushKind.Solid, TAlphaColorRec.White);
  FBrush.Color := TAlphaColorRec.White;
end;

destructor TEngine2DShape.Destroy;
begin
  FBrush.Free;
  FPen.Free;

  inherited;
end;

function TEngine2DShape.GetHeight: Single;
begin
  Result := FHeight;
end;

function TEngine2DShape.GetWidth: Single;
begin
  Result := FWidth;
end;

procedure TEngine2DShape.SetBrush(const Value: TBrush);
begin
  FBrush := Value;
end;

procedure TEngine2DShape.SetFigureRect(const Value: TRectF);
begin
  FFigureRect := Value;

  FWidth := FFigureRect.Width;
  FHeight := FFigureRect.Height;
  FWHalf := FWidth / 2;
  FHHalf := FHeight / 2;
end;

procedure TEngine2DShape.SetPen(const Value: TStrokeBrush);
begin
  FPen := Value;
end;

{ TFillRect }

procedure TFillRect.Repaint;
begin
  FImage.Bitmap.Canvas.Fill.Assign(FBrush);
  FImage.Bitmap.Canvas.Stroke.Assign(FPen);

  FImage.Bitmap.Canvas.FillRect(
    RectF(
      FSubject.X + FWHalf * CJustifyPoints[Justify].Left,
      FSubject.Y + FHHalf * CJustifyPoints[Justify].Top,
      FSubject.X + FWHalf * CJustifyPoints[Justify].Right,
      FSubject.Y + FHHalf * CJustifyPoints[Justify].Bottom),
    0,
    0,
    [],
    FOpacity,
    FMX.Types.TCornerType.Bevel
  );

  if FPen.Thickness > 0 then
    FImage.Bitmap.Canvas.DrawRect(
      RectF(
        FSubject.X + FwHalf * CJustifyPoints[Justify].Left,
        FSubject.Y + FhHalf * CJustifyPoints[Justify].Top,
        FSubject.X + FwHalf * CJustifyPoints[Justify].Right,
        FSubject.Y + FhHalf * CJustifyPoints[Justify].Bottom),
      0,
      0,
      [],
      FOpacity,
      FMX.Types.TCornerType.Bevel
    );
end;

{ TFillEllipse }

procedure TFillEllipse.Repaint;
begin
  FImage.Bitmap.Canvas.Fill.Assign(FBrush);
  FImage.Bitmap.Canvas.Stroke.Assign(FPen);

  FImage.Bitmap.Canvas.FillEllipse(
    RectF(
      FSubject.X + FWHalf * CJustifyPoints[Justify].Left,
      FSubject.Y + FHHalf * CJustifyPoints[Justify].Top,
      FSubject.X + FWHalf * CJustifyPoints[Justify].Right,
      FSubject.Y + FHHalf * CJustifyPoints[Justify].Bottom),
    FOpacity
   );
end;

end.
