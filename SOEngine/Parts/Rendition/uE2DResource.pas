// It's class that caches Image parameters for quick using

unit uE2DResource;

interface

uses
  FMX.Graphics, System.Types;

type
  TSoSpriteResource = class
  strict private
    FPicture: TBitmap;
    FWidth: Single;
    FWHalf: Single;
    FHeight: Single;
    FHHalf: Single;
    FRect: TRectF;
  public
    constructor Create(const APicture: TBitmap);
    property Picture: TBitmap read FPicture;
    property Rect: TRectF read FRect;
    property Width: Single read FWidth;
    property Height: Single read FHeight;
    property WHalf: Single read FWHalf;
    property HHalf: Single read FHHalf;
  end;

implementation

{ TEngine2DResource }

constructor TSoSpriteResource.Create(const APicture: TBitmap);
begin
  FPicture := APicture;

  FWidth := FPicture.Width;
  FHeight := FPicture.Height;
  FWHalf := FWidth / 2;
  FHHalf := FHeight / 2;
  FRect := RectF(0, 0, FWidth, FHeight);
end;

end.
