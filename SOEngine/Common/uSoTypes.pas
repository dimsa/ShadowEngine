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

end.
