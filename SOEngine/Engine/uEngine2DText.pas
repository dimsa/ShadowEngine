unit uEngine2DText;

interface

uses
  System.Types, System.UITypes, FMX.Graphics, FMX.Objects, FMX.Types,
  System.Math, System.SysUtils, System.Classes,
  uEngine2DClasses, uEngine2DResources, uEngine2DObjectShape,
  uEngine2DObject;

type
  TEngine2DText = class(tEngine2DObject)
  strict private
    FText: string; // Текст. Если есть, то выводится
    FFont: tFont; // Шрифт надписи
    FStartFont: TFont; // Первоначальный шрифт при задании
    FStartTextRect: TRectF;
    FColor: TAlphaColor; // Цвет текста
    FFillTextFlags: TFillTextFlags; // Свойство текста
    FVerAlign, FHorAlign: TTextAlign; // Выравнивание текста
    FTextRect: TRectF;
    FFontSizeRatio: Single;
    FAutoSize: Boolean;
    FWordWrap: Boolean;
    procedure SetFont(const Value: tFont);
    procedure SetText(const Value: string);
    procedure SetTextRect(const Value: TRectF);
    procedure SetWordWrap(const Value: Boolean);
    function GetFontSize: Single;
    procedure SetFontSize(const Value: Single);
  protected
    function GetW: single; override;
    function GetH: single; override;
    procedure SetScale(AValue: single); override;
    procedure SetX(AValue: single); override;
    procedure SetY(AValue: single); override;
    procedure SetJustify(const Value: TObjectJustify); override;
  public
    property Font: tFont write SetFont;
    property FontSize: Single read GetFontSize write SetFontSize;
    property Text: string read FText write SetText;
    property TextRect: TRectF read FTextRect write SetTextRect;
    property Color: TAlphaColor read FColor write FColor;
    property AutoSizeFont: Boolean read FAutoSize write FAutoSize;
    property FontSizeRatio: Single read FFontSizeRatio write FFontSizeRatio;
    property WordWrap: Boolean read FWordWrap write SetWordWrap;

    function Config(const AText: string = '';
      const AColor: TAlphaColor = TAlphaColorRec.Black;
      const AGroup: string = '';
      const AJustify: TObjectJustify = Center;
      const AShape: TObjectShape = nil): TEngine2DText; reintroduce;

    procedure AutoResizeFont(const ARatio: Single = 0); experimental; // Автоматически подбирает размер шрифта
    procedure Repaint; override;

    constructor Create; override;
    destructor Destroy; override;
  end;

implementation

{ TEngine2DText }

procedure TEngine2DText.AutoResizeFont(const ARatio: Single);
begin
  if FText <> '' then
   FFont.Size := FStartFont.Size {* Self.ScaleX} * ARatio;
end;

function TEngine2DText.Config(const AText: string; const AColor: TAlphaColor;
  const AGroup: string; const AJustify: TObjectJustify;
  const AShape: TObjectShape): TEngine2DText;
begin
  Result := Self;
  Self.Text := Text;
  Self.Color := AColor;

  inherited Config(AGroup, AJustify, AShape);
end;

constructor TEngine2DText.Create;
begin
  inherited Create;
  FFont := tFont.Create; // Шрифт надписи
  FFont.Family := 'Arial';
  FFont.Size := 12;
  FColor := TAlphaColorRec.Black; // Цвет текста
  FFontSizeRatio := 1;
  FAutoSize := True;
  FWordWrap := False;
  FTextRect := RectF(-50, -25, 50, 25);
  {$IFDEF VER290}
//    FFillTextFlags := [TFillTextFlag.RightToLeft]; // Свойство текста
    FVerAlign := TTextAlign.Center;
    FHorAlign := TTextAlign.Center;
  {$ENDIF}
  {$IFDEF VER260}
 //   FFillTextFlags := [TFillTextFlag.ftRightToLeft]; // Свойство текста
    FVerAlign := TTextAlign.taCenter;
    FHorAlign := TTextAlign.taCenter;
  {$ENDIF}

  FStartFont := TFont.Create;
  FStartFont.Assign(FFont);
  FStartTextRect := FTextRect;
end;

destructor TEngine2DText.Destroy;
begin

  inherited;
end;

function TEngine2DText.GetFontSize: Single;
begin
  Result := FFont.Size;
end;

function TEngine2DText.getH: single;
begin
  Result := FTextRect.Height;  //FStartTextRect.Height; //Image.Bitmap.Canvas.TextHeight(FText);
end;

function TEngine2DText.getW: single;
begin
  Result := FTextRect.Width; //FStartTextRect.Width;// Image.Bitmap.Canvas.TextWidth(FText);
end;

procedure TEngine2DText.Repaint;
begin
  inherited;

  with Image do
  begin
  {$IFDEF VER290}
    Bitmap.Canvas.Stroke.Kind := TBrushKind.Solid;
  {$ENDIF}
  {$IFDEF VER260}
    Bitmap.Canvas.Stroke.Kind := TBrushKind.bkSolid;
  {$ENDIF}
    Bitmap.Canvas.StrokeThickness := 1;
    Bitmap.Canvas.Fill.Color := FColor;

    Bitmap.Canvas.Font.Assign(FFont);
    Bitmap.Canvas.FillText(FTextRect, FText, FWordWrap, FOpacity, FFillTextFlags,
    FHorAlign, FVerAlign);
  end;
end;

procedure TEngine2DText.SetFont(const Value: tFont);
begin
  FFont := Value;
  FStartFont.Assign(FFont);
end;

procedure TEngine2DText.SetFontSize(const Value: Single);
begin
  FFont.Size := Value;
  FStartFont.Size := Value;
end;

procedure TEngine2DText.SetJustify(const Value: TObjectJustify);
begin
  inherited;
  FHorAlign := CJustifyTextAlign[Value].HorAlign;
  FVerAlign := CJustifyTextAlign[Value].VerAlign;
end;

procedure TEngine2DText.SetScale(AValue: single);
begin
  inherited;
  if FAutoSize then
    AutoResizeFont(FFontSizeRatio);
end;

procedure TEngine2DText.SetText(const Value: string);
begin
  FText := Value;
  if FAutoSize then
    AutoResizeFont(FFontSizeRatio);
end;

procedure TEngine2DText.SetTextRect(const Value: TRectF);
begin
  FTextRect := Value;
  FStartTextRect := Value;
  if FAutoSize then
    AutoResizeFont(FFontSizeRatio);
end;

procedure TEngine2DText.SetWordWrap(const Value: Boolean);
begin
  FWordWrap := Value;
  if FAutoSize then
    AutoResizeFont(FFontSizeRatio);
end;

procedure TEngine2DText.setX(AValue: single);
begin
  inherited;
  FTextRect.Location := TPointF.Create(AValue - FTextRect.Width * 0.5, Self.y - FTextRect.Height * 0.5);//.AValue;
end;

procedure TEngine2DText.setY(AValue: single);
begin
  inherited;
  FTextRect.Location := TPointF.Create(Self.x - FTextRect.Width * 0.5, AValue - FTextRect.Height * 0.5);//.AValue;
end;

end.






