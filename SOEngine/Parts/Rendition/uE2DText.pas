unit uE2DText;

interface

uses
  FMX.Graphics, FMX.Types, System.UITypes, System.Types, FMX.Objects,
  uSoObject, uE2DRendition;

type
  TEngine2DText = class(TEngine2DRendition)
  strict private
    FText: string; // Text to Out
    FFont: TFont; // Text Font
    FColor: TAlphaColor; // Цвет текста
    FFillTextFlags: TFillTextFlags; // Properties of text
    FVerAlign, FHorAlign: TTextAlign; // Horizontal and Vertical Align of text
  //  FFontSizeRatio: Single;
 //   FAutoSize: Boolean;
    FWordWrap: Boolean;
    FTextRect: TRectF;
    function GetFontSize: Single;
    procedure SetFont(const Value: tFont);
    procedure SetFontSize(const Value: Single);
    procedure SetText(const Value: string);
    procedure SetTextRect(const Value: TRectF);
    procedure SetWordWrap(const Value: Boolean); // True for wrap words
  protected
    function GetHeight: Single; override;
    function GetWidth: Single; override;
  public
    property Font: tFont write SetFont;
    property FontSize: Single read GetFontSize write SetFontSize;
    property Text: string read FText write SetText;
    property TextRect: TRectF read FTextRect write SetTextRect;
    property Color: TAlphaColor read FColor write FColor;

    procedure Repaint; override;
//    property AutoSizeFont: Boolean read FAutoSize write FAutoSize;
//    property FontSizeRatio: Single read FFontSizeRatio write FFontSizeRatio;
    property WordWrap: Boolean read FWordWrap write SetWordWrap;
    constructor Create(const ASubject: TSoObject; const AImage: TImage);
    destructor Destroy; override;
  end;

implementation

{ TEngine2DText }

constructor TEngine2DText.Create(const ASubject: TSoObject; const AImage: TImage);
begin
  inherited Create(ASubject, AImage);
  FFont := TFont.Create; // Font of Text
  FFont.Family := 'Arial';
  FFont.Size := 12;
  FColor := TAlphaColorRec.White; // Color of Text

  //FFontSizeRatio := 1;
  //FAutoSize := True;
  FWordWrap := False;
  FTextRect := RectF(-50, -25, 50, 25);

  {$IFDEF CONDITIONALEXPRESSIONS}
    {$IF CompilerVersion <= 19.0}
      FVerAlign := TTextAlign.taCenter;
      FHorAlign := TTextAlign.taCenter;
    {$IFEND}
    {$IF RTLVersion > 19.0}
      FVerAlign := TTextAlign.Center;
      FHorAlign := TTextAlign.Center;
    {$IFEND}
  {$ENDIF}
end;

destructor TEngine2DText.Destroy;
begin
  FFont.Free;

  inherited;
end;

function TEngine2DText.GetFontSize: Single;
begin
  Result := FFont.Size;
end;

function TEngine2DText.GetHeight: Single;
begin
  Result := FTextRect.Height;
end;

function TEngine2DText.GetWidth: Single;
begin
  Result := FTextRect.Width;
end;

procedure TEngine2DText.Repaint;
begin
  inherited;

  with FImage do
  begin
  {$IFDEF CONDITIONALEXPRESSIONS}
    {$IF CompilerVersion <= 19.0}
      Bitmap.Canvas.Stroke.Kind := TBrushKind.bkSolid;
    {$IFEND}
    {$IF RTLVersion > 19.0}
      Bitmap.Canvas.Stroke.Kind := TBrushKind.Solid;
    {$IFEND}
  {$ENDIF}

    Bitmap.Canvas.StrokeThickness := 1;
    Bitmap.Canvas.Fill.Color := FColor;

    Bitmap.Canvas.Font.Assign(FFont);
    Bitmap.Canvas.FillText(
      FTextRect,
      FText,
      FWordWrap,
      FOpacity,
      FFillTextFlags,
      FHorAlign,
      FVerAlign);
  end;
end;

procedure TEngine2DText.SetFont(const Value: tFont);
begin
  FFont := Value;
end;

procedure TEngine2DText.SetFontSize(const Value: Single);
begin
  FFont.Size := Value;
end;

procedure TEngine2DText.SetText(const Value: string);
begin
  FText := Value;
end;

procedure TEngine2DText.SetTextRect(const Value: TRectF);
begin
  FTextRect := Value;
end;

procedure TEngine2DText.SetWordWrap(const Value: Boolean);
begin
  FWordWrap := Value;
end;

end.
