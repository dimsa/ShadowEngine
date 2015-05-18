unit uEngine2DSprite;

interface

uses
  System.Types, FMX.Graphics, FMX.Objects, FMX.Types,
  uEngine2DClasses, uEngine2DObject, uEngine2DResources,
  uEngine2DUnclickableObject;

type

  tSprite = class(tEngine2DObject)
  private
    fResources: TEngine2DResources;
    fWhalf, fHHalf: single; // половина ширины и высоты
    fCurRes: Integer;
    function getScW: single;
    function getScH: single;
    procedure SetCurRes(const Value: Integer);
  protected
    function GetW: single; override;
    function GetH: single; override;
    procedure SetScaleX(const AValue: single); override;
    procedure SetScaleY(const AValue: single); override;
    procedure SetScale(AValue: single); override;
  public
    property Resources: TEngine2DResources read FResources write FResources;
    property CurRes: Integer read fCurRes write SetCurRes;
    property wHalf: single read fWhalf; // Служебное свойство - половина ширины
    property hHalf: single read fHHalf; // Служебное свойство - половина высоты
    property scW: single read getScW; // Даёт ширину с учетом масштаба
    property scH: single read getScH; // Даёт высоту с учетом масштаба

    function UnderTheMouse(const MouseX, MouseY: Double): boolean; override;
    procedure Repaint; override;

    constructor Create(newParent: pointer); override;
    destructor Destroy; override;
  end;


implementation

uses
  uEngine2D, mainUnit;

{ tSprite }

constructor tSprite.Create(newParent: pointer);
begin
  inherited;
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
  inherited;


  if DrawSelect then
  image.Bitmap.Canvas.FillEllipse(
  RectF(x - wHalf,
              y - hHalf,
              x + wHalf,
              y + hHalf),
              opacity);

  Image.Bitmap.Canvas.DrawBitmap(
    fResources[fCurRes{Animation.Frames[fFrame].num}].bmp,
    fResources[fCurRes{Animation.Frames[fFrame].num}].rect,
              RectF(x - wHalf,
              y - hHalf,
              x + wHalf,
              y + hHalf),
              opacity{, False}
            );
end;

procedure tSprite.SetCurRes(const Value: Integer);
begin
  FCurRes := Value;
  Self.fWhalf := (W / 2){ * fPosition.scaleX};
  Self.fHhalf := (H / 2){ * fPosition.scaleY};
end;

procedure tSprite.setScale(AValue: single);
begin
  inherited;

end;

procedure tSprite.SetScaleX(const AValue: single);
begin
  inherited;

end;

procedure tSprite.SetScaleY(const AValue: single);
begin
  inherited;

end;

function tSprite.UnderTheMouse(const MouseX, MouseY: Double): boolean;
var
  vDist: Double;
  vEX, vEY: Double; // абсцисса и ордианта точки на эллипсе
  vFi: Double; // Угол точки эллипса
  vR: Double; // Радиус эллипса в конкретной точке
begin

  vEX := Self.x - MouseX;
  vEY := Self.y - MouseY;
  vDist := Sqrt(Sqr(vEX) + Sqr(vEY));

  if  (vEX <> 0) then
    vFi := ArcTan(vEY / vEX) else
       if vEY > 0
       then
         vFi := pi / 2
       else
         vFi := - pi / 2;

  vEx := Abs(Self.w * Self.ScaleX * 0.5);
  vEy := Abs(Self.h * Self.ScaleY * 0.5);
  vR := (vEX * vEY) /
        sqrt( sqr(vEX * cos(vFi)) + sqr(vEY * sin(vFi)) );

  Result := (vR >= vDist);
  //Inherited;
end;

end.



