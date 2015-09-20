unit uEngine2DSprite;

interface

uses
  System.Types, FMX.Graphics, FMX.Objects, FMX.Types,
  uEngine2DClasses, uEngine2DObject, uEngine2DResources,
  uEngine2DUnclickableObject;

type

  TSprite = class(tEngine2DObject)
  private
    fResources: TEngine2DResources;
    fWhalf, fHHalf: single; // половина ширины и высоты
    fCurRes: Integer;
    function getScW: single;
    function getScH: single;
  protected
    function GetW: single; override;
    function GetH: single; override;
    procedure SetCurRes(const Value: Integer); virtual;
  public
    property Resources: TEngine2DResources read FResources write FResources;
    property CurRes: Integer read fCurRes write SetCurRes;
    property wHalf: single read fWhalf; // Служебное свойство - половина ширины
    property hHalf: single read fHHalf; // Служебное свойство - половина высоты
    property scW: single read getScW; // Даёт ширину с учетом масштаба
    property scH: single read getScH; // Даёт высоту с учетом масштаба

    procedure Repaint; override;

    constructor Create(AParent: pointer); override;
    destructor Destroy; override;
  end;


implementation

uses
  uEngine2D, mainUnit;

{ tSprite }

constructor tSprite.Create(AParent: pointer);
begin
  inherited Create(AParent);
  fCreationNumber := tEngine2d(fParent).spriteCount;
end;

destructor tSprite.Destroy;
begin
  inherited;
end;

function tSprite.getScH: single;
begin
  result := h * fPosition.scaleY;
end;

function tSprite.getScW: single;
begin
  result := w * fPosition.scaleX;
end;

function tSprite.getH: single;
begin
  Result := self.Resources[fCurRes].bmp.Height;
end;

function tSprite.getW: single;
begin
  Result := self.Resources[fCurRes].bmp.Width;
end;

procedure tSprite.Repaint;
begin
  Image.Bitmap.Canvas.DrawBitmap(
    fResources[fCurRes].bmp,
    fResources[fCurRes].rect,
              RectF(x + wHalf * CJustifyPoints[Justify].Left,
              y + hHalf * CJustifyPoints[Justify].Top,
              x + wHalf * CJustifyPoints[Justify].Right,
              y + hHalf * CJustifyPoints[Justify].Bottom),
              Opacity, False
            );
  inherited;
end;

procedure tSprite.SetCurRes(const Value: Integer);
begin
  FCurRes := Value;
  Self.fWhalf := (W / 2){ * fPosition.scaleX};
  Self.fHhalf := (H / 2){ * fPosition.scaleY};
end;

end.




