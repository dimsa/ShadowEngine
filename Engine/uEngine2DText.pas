unit uEngine2DText;

interface

uses
  System.Types, System.UITypes, FMX.Graphics, FMX.Objects, FMX.Types,
  uEngine2DClasses, uEngine2DResources,
  uEngine2DObject;

type
  TEngine2DText = class(tEngine2DObject)
  strict private
    fText: string; // Текст. Если есть, то выводится
    fFont: tFont; // Шрифт надписи
    fColor: TAlphaColor; // Цвет текста
    FFillTextFlags: TFillTextFlags; // Свойство текста
    FVerAlign, FHorAlign: TTextAlign; // Выравнивание текста
    FTextRect: TRectF;
  private
    procedure SetFont(const Value: tFont);
    procedure SetText(const Value: string);
    procedure RenewRec;
  protected
    function GetW: single; override;
    function GetH: single; override;
    procedure SetScale(AValue: single); override;
    procedure SetX(AValue: single); override;
    procedure SetY(AValue: single); override;
  public
    property Font: tFont read fFont write SetFont;
    property Text: string read fText write SetText;
    property TextRec: TRectF read FTextRect;
    property Color: TAlphaColor read FColor write FColor;

 //   procedure copy(var AText: TEngine2DText); // Делает копию объекта
    function UnderTheMouse(const Mousex, MouseY: Double): boolean; override;
    procedure Repaint; override;
    constructor Create(AParent: pointer); override;
    destructor Destroy; override;
  end;

implementation

{ TEngine2DText }

constructor TEngine2DText.Create(AParent: pointer);
begin
  inherited;
  fFont := tFont.Create; // Шрифт надписи
  fFont.Family := 'Arial';
  fFont.Size := 12;
  fColor := TAlphaColorRec.Black; // Цвет текста
  FFillTextFlags := [TFillTextFlag.RightToLeft]; // Свойство текста
  FVerAlign := TTextAlign.Center;
  FHorAlign := TTextAlign.Center;
end;

destructor TEngine2DText.Destroy;
begin

  inherited;
end;

function TEngine2DText.getH: single;
begin
  Result := Image.Bitmap.Canvas.TextHeight(FText);
end;

function TEngine2DText.getW: single;
begin
  Result := Image.Bitmap.Canvas.TextWidth(FText);
end;

procedure TEngine2DText.RenewRec;
begin
  FTextRect := RectF(
    X - W * ScaleX / 2, Y - H * ScaleY / 2,
    X + W * ScaleX / 2, Y + H * ScaleY / 2);
end;

procedure TEngine2DText.Repaint;
var
  vTmp: TFont;
begin
  inherited;

  with Image do
  begin
    Bitmap.Canvas.Stroke.Kind := TBrushKind.Solid;
    Bitmap.Canvas.StrokeThickness := 1;
    Bitmap.Canvas.Fill.Color := FColor;
//    if self. then

    vTmp := TFont.Create;
    vTmp.Assign( Bitmap.Canvas.Font);
    Bitmap.Canvas.Font.Assign(FFont);

    Bitmap.Canvas.FillText(FTextRect, FText, False, Opacity, FFillTextFlags,
    FHorAlign, FVerAlign);
    Bitmap.Canvas.Font.Assign(vTmp);
    vTmp.Free;
//    bitmap.Canvas.FillRect(rectf(x-1, y-1, x+1, y+1), 0, 0, AllCorners, $FF);
  end;
end;

procedure TEngine2DText.SetFont(const Value: tFont);
begin
  fFont := Value;
  RenewRec;
end;

procedure TEngine2DText.SetScale(AValue: single);
begin
  inherited;

  Self.fFont.Size := 12 * ScaleY;
  RenewRec;
end;

procedure TEngine2DText.SetText(const Value: string);
begin
  fText := Value;
  RenewRec;
end;

procedure TEngine2DText.setX(AValue: single);
begin
  inherited;
  RenewRec;
end;

procedure TEngine2DText.setY(AValue: single);
begin
  inherited;
  RenewRec;
end;

function TEngine2DText.underTheMouse(const MouseX, MouseY: Double): boolean;
begin
  Result := Inherited;
end;

end.



