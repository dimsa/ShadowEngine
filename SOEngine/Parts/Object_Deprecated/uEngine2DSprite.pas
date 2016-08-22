unit uEngine2DSprite;

interface

uses
  System.Types, FMX.Graphics, FMX.Objects, FMX.Types, System.Classes,
  uEngine2DClasses, uEngine2DObject, uEngine2DResources, uEngine2DObjectShape,
  uEngine2DUnclickableObject;

type

  TSprite = class(tEngine2DObject)
  private
    fResources: TEngine2DResources;
    fWhalf, fHHalf: single; // половина ширины и высоты
    fCurRes: Integer;
    function getScW: single;
    function getScH: single;
    function Config(const AGroup: string = ''; const AJustify: TObjectJustify = Center; const AShape: TObjectShape = nil): TEngine2DObject; overload; override;
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
    function Config(const ACurRes: Integer = -1; AGroup: string = ''; const AJustify: TObjectJustify =Center; const AShape: TObjectShape = nil): TSprite;  reintroduce; overload;
    function Config(const AResName: string; AGroup: string = ''; const AJustify: TObjectJustify =Center; const AShape: TObjectShape = nil): TSprite; reintroduce; overload;
    procedure Repaint; override;

    constructor Create; override;
    destructor Destroy; override;
  end;

implementation

uses
  uEngine2D;

{ tSprite }

function TSprite.Config(const ACurRes: Integer; AGroup: string;
  const AJustify: TObjectJustify; const AShape: TObjectShape): TSprite;
begin
  if ACurRes <> -1 then
    CurRes := ACurRes;
  inherited Config(AGroup, AJustify, AShape);
  Result := Self;
end;

function TSprite.Config(const AResName: string; AGroup: string;
  const AJustify: TObjectJustify; const AShape: TObjectShape): TSprite;
begin 
  CurRes := FResources.IndexOf(AResName);
  inherited Config(AGroup, AJustify, AShape);
  Result := Self;
end;

function TSprite.Config(const AGroup: string; const AJustify: TObjectJustify;
  const AShape: TObjectShape): TEngine2DObject;
begin
  Result := inherited Config(AGroup, AJustify, AShape);
end;

constructor tSprite.Create;
begin
  inherited Create;
//  fCreationNumber := tEngine2d(fParent).spriteCount;
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
              1, True
            );
  inherited;
end;

procedure tSprite.SetCurRes(const Value: Integer);
begin
  FCurRes := Value;
  Self.fWhalf := (W / 2);
  Self.fHhalf := (H / 2);
end;

end.




