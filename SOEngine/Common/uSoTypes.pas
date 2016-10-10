unit uSoTypes;

interface

uses
  FMX.Types, System.Types, System.UITypes, FMX.Graphics, System.Generics.Collections, FMX.Objects,
  System.Classes, System.SyncObjs, System.SysUtils;

type

  TRectF = System.Types.TRectF;
  TRect = System.Types.TRect;
  TPoint = System.Types.TPoint;
  TPointF = System.Types.TPointF;

  TFont =  FMX.Graphics.TFont;
  TColor = System.UITypes.TAlphaColor;
  TFillTextFlag = FMX.Graphics.TFillTextFlag;
  TFillTextFlags = FMX.Graphics.TFillTextFlags;
  TTextAlign = FMX.Types.TTextAlign;
  TStrokeBrush = FMX.Graphics.TStrokeBrush;
  TBrush = FMX.Graphics.TBrush;

  TDict<TKey,TValue> = class(System.Generics.Collections.TDictionary<TKey,TValue>);
  TList<T> = class(System.Generics.Collections.TList<T>);
  Int = Integer;
  Bool = Boolean;
  TBitmap = FMX.Graphics.TBitmap;

  TAnonImage = TImage;
  TNotifyEvent = System.Classes.TNotifyEvent;
  TShiftState = System.Classes.TShiftState;
  TMouseButton = System.UITypes.TMouseButton;
  TStringList = System.Classes.TStringList;

  TMouseEvent = FMX.Types.TMouseEvent;

  TCriticalSection = System.SyncObjs.TCriticalSection;
  TAlphaColorRec = System.UITypes.TAlphaColorRec;
  TFontStyle = System.UITypes.TFontStyle;
  Exception = System.SysUtils.Exception;

  TSizeObject = class
  private
    FSize: TPointF;
    procedure SetHeight(const Value: Single);
    procedure SetSize(const Value: TPointF);
    procedure SetWidth(const Value: Single);
  public
    property Width: Single read FSize.X write SetWidth;
    property Height: Single read FSize.Y write SetHeight;
    property Value: TPointF read FSize write SetSize;
    function IsHor: Boolean;
  end;

  TRectObject = class
  private
    FRect: TRectF;
    procedure SetTopLeft(const Value: TPointF);
    procedure SetBottomRight(const Value: TPointF);
    procedure SetRect(const Value: TRectF);
    procedure SetHeight(const Value: Single);
    procedure SetWidth(const Value: Single);
    function GetHeight: Single;
    function GetWidth: Single;
  public
    property TopLeft: TPointF read FRect.TopLeft write SetTopLeft;
    property BottomRight: TPointF read FRect.BottomRight write SetBottomRight;
    property Width: Single read GetWidth write SetWidth;
    property Height: Single read GetHeight write SetHeight;
    property Rect: TRectF read FRect write SetRect;
    function IsHor: Boolean;
    constructor Create;
  end;

implementation

{ TSize }

function TSizeObject.IsHor: Boolean;
begin
  Result := FSize.X > FSize.Y;
end;

procedure TSizeObject.SetHeight(const Value: Single);
begin
  FSize.Y := Value;
end;

procedure TSizeObject.SetSize(const Value: TPointF);
begin
  FSize := Value;
end;

procedure TSizeObject.SetWidth(const Value: Single);
begin
  FSize.X := Value;
end;

{ TRectObject }

constructor TRectObject.Create;
begin
  FRect := TRectF.Empty;
end;

function TRectObject.GetHeight: Single;
begin
  Result := FRect.Height;
end;

function TRectObject.GetWidth: Single;
begin
  Result := FRect.Width;
end;

function TRectObject.IsHor: Boolean;
begin
  Result := FRect.Width >= FRect.Height;
  
end;

procedure TRectObject.SetBottomRight(const Value: TPointF);
begin
  FRect.BottomRight := Value;
end;

procedure TRectObject.SetHeight(const Value: Single);
begin
  FRect.Height := Value;
end;

procedure TRectObject.SetRect(const Value: TRectF);
begin
  FRect := Value;
end;

procedure TRectObject.SetTopLeft(const Value: TPointF);
begin
  FRect.TopLeft := Value;
end;

procedure TRectObject.SetWidth(const Value: Single);
begin
  FRect.Width := Value;
end;

end.
