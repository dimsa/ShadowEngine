unit uSoText;

interface

uses
  uSoTypes,
  uNamedList, uE2DRendition, uSoObject;

type
  TSoText = class(TEngine2DRendition)
  private type
    TSoObjectFriend = class(TSoObject);
  private
    FText: string; // Text to Out
    FFont: TFont; // Text Font
    FColor: TColor; // Цвет текста
    FFillTextFlags: TFillTextFlags; // Properties of text
    FVerAlign, FHorAlign: TTextAlign; // Horizontal and Vertical Align of text
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
    property Font: TFont write SetFont;
    property FontSize: Single read GetFontSize write SetFontSize;
    property Text: string read FText write SetText;
    property TextRect: TRectF read FTextRect write SetTextRect;
    property Color: TColor read FColor write FColor;

    procedure Repaint; override;

    property WordWrap: Boolean read FWordWrap write SetWordWrap;
    constructor Create(const ASubject: TSoObject; const AImage: TAnonImage);
    destructor Destroy; override;
  end;

implementation

constructor TSoText.Create(const ASubject: TSoObject; const AImage: TAnonImage);
begin
  inherited Create(ASubject, AImage);
  FFont := TFont.Create; // Font of Text
  FFont.Family := 'Arial';
  FFont.Size := 12;
  FColor := TAlphaColorRec.White; // Color of Text

  //FFontSizeRatio := 1;
  //FAutoSize := True;
  FWordWrap := False;
  FTextRect := TRectF.Create(-50, -25, 50, 25);

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

destructor TSoText.Destroy;
begin
  FFont.Free;

  inherited;
end;

function TSoText.GetFontSize: Single;
begin
  Result := FFont.Size;
end;

function TSoText.GetHeight: Single;
begin
  Result := FTextRect.Height;
end;

function TSoText.GetWidth: Single;
begin
  Result := FTextRect.Width;
end;

procedure TSoText.Repaint;
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

procedure TSoText.SetFont(const Value: tFont);
begin
  FFont := Value;
end;

procedure TSoText.SetFontSize(const Value: Single);
begin
  FFont.Size := Value;
end;

procedure TSoText.SetText(const Value: string);
begin
  FText := Value;
end;

procedure TSoText.SetTextRect(const Value: TRectF);
begin
  FTextRect := Value;
end;

procedure TSoText.SetWordWrap(const Value: Boolean);
begin
  FWordWrap := Value;
end;

end.
