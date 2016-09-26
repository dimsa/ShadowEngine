unit uSoTypes;

interface

uses
  FMX.Types, System.Types, System.UITypes, FMX.Graphics, System.Generics.Collections, FMX.Objects,
  System.Classes;

type

  TRectF = System.Types.TRectF;
  TRect = System.Types.TRect;
  TPoint = System.Types.TPoint;
  TPointF = System.Types.TPointF;

  TFont =  FMX.Graphics.TFont;
  TColor = System.UITypes.TAlphaColor;
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

implementation

end.
