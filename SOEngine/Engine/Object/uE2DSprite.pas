unit uE2DSprite;

interface

uses
  FMX.Objects, System.Types,
  uEngine2DRendition, uEngine2DResourceList, uBaseContainer, uEngine2DClasses;

type

  TEngine2DSPrite = class(TEngine2DRendition)
  private
    FCurRes: Integer;
    FResourceList: TEngine2DResourceList;
  public
    procedure Repaint; override;
    constructor Create(const ASubject: TBaseUnitContainer; const AImage: TImage; const AResourceList: TEngine2DResourceList);
    destructor Destroy; override;
  end;

implementation

{ TEngine2DSPrite }

constructor TEngine2DSPrite.Create(const ASubject: TBaseUnitContainer; const AImage: TImage;
  const AResourceList: TEngine2DResourceList);
begin
  inherited Create(ASubject, AImage);
  FResourceList := AResourceList;
end;

destructor TEngine2DSPrite.Destroy;
begin
  FResourceList := nil;
  inherited;
end;

procedure TEngine2DSPrite.Repaint;
begin
  FImage.Bitmap.Canvas.DrawBitmap(
    FResourceList[FCurRes].Picture,
    FResourceList[FCurRes].Rect,
    RectF(
      FSubject.X + FResourceList[FCurRes].WHalf * FSubject.ScaleX * CJustifyPoints[Justify].Left,
      FSubject.Y + FResourceList[FCurRes].HHalf * FSubject.ScaleY * CJustifyPoints[Justify].Top,
      FSubject.X + FResourceList[FCurRes].WHalf * FSubject.ScaleX * CJustifyPoints[Justify].Right,
      FSubject.Y + FResourceList[FCurRes].HHalf * FSubject.ScaleY * CJustifyPoints[Justify].Bottom),
    1,
    True);
end;

end.
