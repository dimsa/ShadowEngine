unit uEngine2DClasses;

interface

uses
  System.SysUtils, System.Generics.Defaults,
  uSoTypes, uGeometryClasses, uClasses, uNamedList;

type
  TReturnSingleFunction = function: Single of object;
  TIntArray = array of Integer;
  PIntArray = ^TIntArray;

  TObjectJustify = (
    TopLeft, TopCenter, TopRight,
    CenterLeft, Center, CenterRight,
    BottomLeft, BottomCenter, BottomRight);

  TTextAlignRecord = record
    HorAlign, VerAlign: TTextAlign;
  end;

  TRenditionType = (rtSprite, rtText, rtShape);

  TColliderType = (ctPoly, ctCircle);

const
  pi180 = 0.0174532925;
{  CJustifyPoints: array[TObjectJustify] of TPoint = (
    (X: -2; Y: -1), (X: 0; Y: -1), (X: 1; Y: -1),
    (X: -1; Y:  0), (X: 0; Y:  0), (X: 1; Y:  0),
    (X: -1; Y:  1), (X: 0; Y:  1), (X: 1; Y:  1)); }
  CJustifyPoints: array[TObjectJustify] of TRect = (
    (Left: -2; Top: -2; Right: 0; Bottom: 0), (Left: -1; Top: -2; Right: 1; Bottom: 0), (Left: 0; Top: -2; Right: 2; Bottom: 0),
    (Left: -2; Top: -1; Right: 0; Bottom: 1), (Left: -1; Top: -1; Right: 1; Bottom: 1), (Left: 0; Top: -1; Right: 2; Bottom: 1),
    (Left: -2; Top:  0; Right: 0; Bottom: 2), (Left: -1; Top:  0; Right: 1; Bottom: 2), (Left: 0; Top:  0; Right: 2; Bottom: 2));

  {$IFDEF CONDITIONALEXPRESSIONS}
    {$IF CompilerVersion >= 19.0}
    CJustifyTextAlign: array[TObjectJustify] of TTextAlignRecord = (
      (HorAlign: TTextAlign.Trailing; VerAlign: TTextAlign.Leading), (HorAlign: TTextAlign.Center; VerAlign: TTextAlign.Leading), (HorAlign: TTextAlign.Leading; VerAlign: TTextAlign.Leading),
      (HorAlign: TTextAlign.Trailing; VerAlign: TTextAlign.Center),  (HorAlign: TTextAlign.Center; VerAlign: TTextAlign.Center),  (HorAlign: TTextAlign.Leading; VerAlign: TTextAlign.Center),
      (HorAlign: TTextAlign.Trailing; VerAlign: TTextAlign.Trailing),  (HorAlign: TTextAlign.Center; VerAlign: TTextAlign.Trailing),  (HorAlign: TTextAlign.Leading; VerAlign: TTextAlign.Trailing));
     {$ENDIF}
  {$ENDIF}

  {$IFDEF VER260}
  CJustifyTextAlign: array[TObjectJustify] of TTextAlignRecord = (
    (HorAlign: TTextAlign.taTrailing; VerAlign: TTextAlign.taLeading), (HorAlign: TTextAlign.taCenter; VerAlign: TTextAlign.taLeading), (HorAlign: TTextAlign.taLeading; VerAlign: TTextAlign.taLeading),
    (HorAlign: TTextAlign.taTrailing; VerAlign: TTextAlign.taCenter),  (HorAlign: TTextAlign.taCenter; VerAlign: TTextAlign.taCenter),  (HorAlign: TTextAlign.taLeading; VerAlign: TTextAlign.taCenter),
    (HorAlign: TTextAlign.taTrailing; VerAlign: TTextAlign.taTrailing),  (HorAlign: TTextAlign.taCenter; VerAlign: TTextAlign.taTrailing),  (HorAlign: TTextAlign.taLeading; VerAlign: TTextAlign.taTrailing));
  {$ENDIF}


type
  tSpriteResource = record
    Rect: tRectF;
    Bmp: tBitmap;
  end;

  // Thread-save class for Engine Threads Потокобезопасный класс именованных листов для енджайна. Обязательно должен быть указан Парент!
  TEngine2DNamedList<T> = class(TNamedList<T>)
  protected
    FCriticalSection: TCriticalSection;
  public
    function Add(const AName: String; Const AValue: T): Integer; override;
    function Add(AValue: T): Integer; override;
    function Insert(const AIndex: Integer; const AName: String; const AValue: T): Integer; override;
    function Insert(const AIndex: Integer; AValue: T): Integer; override;
    procedure Delete(const AName: String); overload; override;
    procedure Delete(const AT: T); overload; override;
    procedure Delete(const AIndex: Integer); overload; override;
    constructor Create(const ACritical: TCriticalSection); reintroduce; virtual;
  end;

  tPointArray = array of TPoint;

  tSpriteFrame = record
    num: integer; // Number in Resources // Номер в ресурсе
    w,h: single; // Precalculated Width and Height  // Заранее опсчитанная ширина и высота
  end;

  tFramesArray = array of tSpriteFrame;

  tPositionArray = array of tPosition;
  tBitmapArray = array of tBitmap;
  tResourceArray = array of tSpriteResource;

  function IntArrInIntArr(Const AIntArr1, AIntArr2: TIntArray): TIntArray;
  function ClearPosition: TPosition;

implementation

function ClearPosition: TPosition;
begin
  Result.X := 0;
  Result.Y := 0;
  Result.Rotate := 0;
  Result.ScaleX := 1;
  Result.ScaleY := 1;
end;


function IntArrInIntArr(Const AIntArr1, AIntArr2: TIntArray): TIntArray;
var
  i1, i2: Integer;
  l1, l2: Integer;
  vRes: TIntArray;
begin
  l1 := Length(AIntArr1) - 1;
  l2 := Length(AIntArr2) - 1;
  SetLength(vRes, 0);

  for i1 := 0 to l1 do
    for i2 := 0 to l2 do
      if AIntArr1[i1] = AIntArr2[i2] then
      begin
        SetLength(vRes, Length(vRes) + 1);
        vRes[High(vRes)] := AIntArr1[i1];
      end;

  Result := vRes;

end;

{ TEngine2DNamedList<T> }

function TEngine2DNamedList<T>.Add(const AName: String;
  const AValue: T): Integer;
begin
  FCriticalSection.Enter;
  Result := inherited Add(AName, AValue);
  FCriticalSection.Leave;
end;

function TEngine2DNamedList<T>.Add(AValue: T): Integer;
begin
  FCriticalSection.Enter;
  Result := inherited Add(AValue);
  FCriticalSection.Leave;
end;

constructor TEngine2DNamedList<T>.Create(const ACritical: TCriticalSection);
begin
  inherited Create;
  FCriticalSection := ACritical;
end;

procedure TEngine2DNamedList<T>.Delete(const AName: String);
begin
  FCriticalSection.Enter;
  inherited Delete(AName);
  FCriticalSection.Leave;
end;

procedure TEngine2DNamedList<T>.Delete(const AT: T);
begin
  FCriticalSection.Enter;
  inherited Delete(AT);
  FCriticalSection.Leave;
end;

procedure TEngine2DNamedList<T>.Delete(const AIndex: Integer);
begin
  FCriticalSection.Enter;
  inherited Delete(AIndex);
  FCriticalSection.Leave;
end;

function TEngine2DNamedList<T>.Insert(const AIndex: Integer;
  const AName: String; const AValue: T): Integer;
begin
  FCriticalSection.Enter;
  Result := inherited Insert(AIndex, AName, AValue);
  FCriticalSection.Leave;
end;

function TEngine2DNamedList<T>.Insert(const AIndex: Integer;
  AValue: T): Integer;
begin
  FCriticalSection.Enter;
  Result := inherited Insert(AIndex, AValue);
  FCriticalSection.Leave;
end;

end.
