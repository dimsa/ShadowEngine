unit uEngine2DClasses;

interface

uses
  FMX.Graphics, System.SyncObjs, System.SysUtils, System.Types,
  FMX.Types, uIntersectorClasses,
  System.Generics.Collections, uNamedList, System.Generics.Defaults;

type

  TObjectJustify = (
    TopLeft, TopCenter, TopRight,
    CenterLeft, Center, CenterRight,
    BottomLeft, BottomCenter, BottomRight);

  TTextAlignRecord = record
    HorAlign, VerAlign: TTextAlign;
  end;

const
{  CJustifyPoints: array[TObjectJustify] of TPoint = (
    (X: -2; Y: -1), (X: 0; Y: -1), (X: 1; Y: -1),
    (X: -1; Y:  0), (X: 0; Y:  0), (X: 1; Y:  0),
    (X: -1; Y:  1), (X: 0; Y:  1), (X: 1; Y:  1)); }
  CJustifyPoints: array[TObjectJustify] of TRect = (
    (Left: -2; Top: -2; Right: 0; Bottom: 0), (Left: -1; Top: -2; Right: 1; Bottom: 0), (Left: 0; Top: -2; Right: 2; Bottom: 0),
    (Left: -2; Top: -1; Right: 0; Bottom: 1), (Left: -1; Top: -1; Right: 1; Bottom: 1), (Left: 0; Top: -1; Right: 2; Bottom: 1),
    (Left: -2; Top:  0; Right: 0; Bottom: 2), (Left: -1; Top:  0; Right: 1; Bottom: 2), (Left: 0; Top:  0; Right: 2; Bottom: 2));

  {$IFDEF VER290}
  CJustifyTextAlign: array[TObjectJustify] of TTextAlignRecord = (
    (HorAlign: TTextAlign.Trailing; VerAlign: TTextAlign.Leading), (HorAlign: TTextAlign.Center; VerAlign: TTextAlign.Leading), (HorAlign: TTextAlign.Leading; VerAlign: TTextAlign.Leading),
    (HorAlign: TTextAlign.Trailing; VerAlign: TTextAlign.Center),  (HorAlign: TTextAlign.Center; VerAlign: TTextAlign.Center),  (HorAlign: TTextAlign.Leading; VerAlign: TTextAlign.Center),
    (HorAlign: TTextAlign.Trailing; VerAlign: TTextAlign.Trailing),  (HorAlign: TTextAlign.Center; VerAlign: TTextAlign.Trailing),  (HorAlign: TTextAlign.Leading; VerAlign: TTextAlign.Trailing));
  {$ENDIF}

  {$IFDEF VER260}
  CJustifyTextAlign: array[TObjectJustify] of TTextAlignRecord = (
    (HorAlign: TTextAlign.taTrailing; VerAlign: TTextAlign.taLeading), (HorAlign: TTextAlign.taCenter; VerAlign: TTextAlign.taLeading), (HorAlign: TTextAlign.taLeading; VerAlign: TTextAlign.taLeading),
    (HorAlign: TTextAlign.taTrailing; VerAlign: TTextAlign.taCenter),  (HorAlign: TTextAlign.taCenter; VerAlign: TTextAlign.taCenter),  (HorAlign: TTextAlign.taLeading; VerAlign: TTextAlign.taCenter),
    (HorAlign: TTextAlign.taTrailing; VerAlign: TTextAlign.taTrailing),  (HorAlign: TTextAlign.taCenter; VerAlign: TTextAlign.taTrailing),  (HorAlign: TTextAlign.taLeading; VerAlign: TTextAlign.taTrailing));
  {$ENDIF}


type
  tSpriteResource = record
    rect: tRectF;
    bmp: tBitmap;
  end;

  TEngine2DOptions = record
    ToClickOnlyTop: Boolean; // События кликов передаются только верхнему объекту в клике
    ToAnimateForever: Boolean; // Сообщает, что надо перерисовывать сцену, даже если нет анимаций
  end;

  // Потокобезопасный класс именованных листов для енджайна. Обязательно должен быть указан Парент!
  TEngine2DNamedList<T> = class(TNamedList<T>)
  public
    function Add(const AName: String; Const AValue: T): Integer; override;
    function Add(AValue: T): Integer; override;
    function Insert(const AIndex: Integer; const AName: String; const AValue: T): Integer; override;
    function Insert(const AIndex: Integer; AValue: T): Integer; override;
    procedure Delete(const AName: String); overload; override;
    procedure Delete(const AT: T); overload; override;
    procedure Delete(const AIndex: Integer); overload; override;
    constructor Create; override;
  end;

  {tClickProcedure = procedure(X, Y: Single) of object;
  tStandardProcedure = procedure(ASender: TObject) of object;
  tSimpleProcedure = procedure of object; }

  tIntArray = array of integer;

  tPointArray = array of TPoint;

  tSpriteFrame = record
    num: integer; // Номер в ресурсе
    w,h: single; // Заранее опсчитанная ширина и высота
  end;

  tFramesArray = array of tSpriteFrame;

  tPositionArray = array of tPosition;
  tBitmapArray = array of tBitmap;
  tResourceArray = array of tSpriteResource;

{  tLogicProcedure = record
    proc: tClickProcedure;
    data: tIntArray;
  end;      }

  function IntArrInIntArr(Const AIntArr1, AIntArr2: TIntArray): TIntArray;
  function ClearPosition: TPosition;

implementation

uses
  uEngine2D;

function ClearPosition: TPosition;
var
  vRes: TPosition;
begin
  vRes.X := 0;
  vRes.Y := 0;
  vRes.Rotate := 0;
  vRes.ScaleX := 1;
  vRes.ScaleY := 1;
 // Opacity := 1;
  Result := vRes;
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
var
  vEngine: tEngine2d;
begin
  vEngine := Parent;
  vEngine.Critical.Enter;
  Result := inherited Add(AName, AValue);
  vEngine.Critical.Leave;
end;

function TEngine2DNamedList<T>.Add(AValue: T): Integer;
var
  vEngine: tEngine2d;
begin
  vEngine := Parent;
  vEngine.Critical.Enter;
  Result := inherited Add(AValue);
  vEngine.Critical.Leave;
end;

constructor TEngine2DNamedList<T>.Create;
begin
  inherited;

end;

procedure TEngine2DNamedList<T>.Delete(const AName: String);
var
  vEngine: tEngine2d;
begin
  vEngine := Parent;
  vEngine.Critical.Enter;
  inherited Delete(AName);
  vEngine.Critical.Leave;
end;

procedure TEngine2DNamedList<T>.Delete(const AT: T);
var
  vEngine: tEngine2d;
begin
  vEngine := Parent;
  vEngine.Critical.Enter;
  inherited Delete(AT);
  vEngine.Critical.Leave;
end;

procedure TEngine2DNamedList<T>.Delete(const AIndex: Integer);
var
  vEngine: tEngine2d;
begin
  vEngine := Parent;
  vEngine.Critical.Enter;
  inherited Delete(AIndex);
  vEngine.Critical.Leave;
end;

function TEngine2DNamedList<T>.Insert(const AIndex: Integer;
  const AName: String; const AValue: T): Integer;
var
  vEngine: tEngine2d;
begin
  vEngine := Parent;
  vEngine.Critical.Enter;
  Result := inherited Insert(AIndex, AName, AValue);
  vEngine.Critical.Leave;
end;

function TEngine2DNamedList<T>.Insert(const AIndex: Integer;
  AValue: T): Integer;
var
  vEngine: tEngine2d;
begin
  vEngine := Parent;
  vEngine.Critical.Enter;
  Result := inherited Insert(AIndex, AValue);
  vEngine.Critical.Leave;
end;

end.






