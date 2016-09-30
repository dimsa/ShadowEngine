// ‘астфильды это как бы стек быстрых значений, позвол€ющий освободить парсер
// выражений от необходимости поиска в  большом массиве спрайтов

unit uFastFields;

interface

uses
  System.Generics.Collections, System.SysUtils, uClasses, uCommonClasses,
  uParserValue, uEngine2DUnclickableObject, uNamedList;

type

  TFastField = class(TValue)
  protected
    function GetValue: Double; virtual; abstract;
    procedure SetValue(const Value: Double); virtual; abstract;
  public
    function IsBroken: Boolean; virtual;
    function Value: Double; override;
    property PropertyValue: Double read GetValue write SetValue;
  end;

  TFastFields = class(TNamedList<TFastField>)
  private
    FIsHor: TDelegate<Boolean>;
  public
    function IsHor: Boolean;
    procedure ClearForSubject(const AObject: tEngine2DUnclickableObject);
    procedure ClearBroken; // ”дал€ет все сломанные фастфильды
    destructor Destroy; override;
    constructor Create(AIsHor: TDelegate<Boolean>); reintroduce;
  end;

  TFastEngineField = class(TFastField)
  private
    FValue: PInteger;
//    fEngine: Pointer;
  public
    constructor Create(const AValue: PInteger); reintroduce; virtual;
//     Pointer: Pointer
    destructor Destroy; override;
  end;

  TFastEngineWidth = class(TFastEngineField)
  protected
    function GetValue: Double; override;
    procedure SetValue(const Value: Double); override;
  end;

  TFastEngineHeight = class(TFastEngineField)
  protected
    function GetValue: Double; override;
    procedure SetValue(const Value: Double); override;
  end;

  TFastObjectField = class(TFastField)
  private
    FObject: tEngine2DUnclickableObject;
  public
    property Subject: tEngine2DUnclickableObject read FObject;
    constructor Create(const AObject: tEngine2DUnclickableObject); reintroduce; virtual;
    function IsBroken: Boolean; override;
    destructor Destroy; override;
  end;

  // ќбратите внимание, что ширина €вл€етс€ –ид-онли проперти, таким образом
  // мен€€ высоту с помощью этого ‘аст‘ильда, вы лишь помен€ете масштаб
  TFastWidth = class(TFastObjectField)
  protected
    function GetValue: Double; override;
    procedure SetValue(const Value: Double); override;
  end;

  // ќбратите внимание, что высота €вл€етс€ –ид-онли проперти, таким образом
  // мен€€ высоту с помощью этого ‘аст‘ильда, вы лишь помен€ете масштаб
  TFastHeight = class(TFastObjectField)
  protected
    function GetValue: Double; override;
    procedure SetValue(const Value: Double); override;
  end;

  // * ќткровенное говор€, € уже не помню почему € сделал ширину и высоту
  // рид-онли пропертЄй. Ќа самом деле надо добавить проперти ќригинальный–азмер
  TFastLeftBorder = class(TFastObjectField)
  protected
    function GetValue: Double; override;
    procedure SetValue(const Value: Double); override;
  end;

  TFastRightBorder = class(TFastObjectField)
  protected
    function GetValue: Double; override;
    procedure SetValue(const Value: Double); override;
  end;

  TFastTopBorder = class(TFastObjectField)
  protected
    function GetValue: Double; override;
    procedure SetValue(const Value: Double); override;
  end;

  TFastBottomBorder = class(TFastObjectField)
  protected
    function GetValue: Double; override;
    procedure SetValue(const Value: Double); override;
  end;

  TFastX = class(TFastObjectField)
  protected
    function GetValue: Double; override;
    procedure SetValue(const Value: Double); override;
  end;

  TFastY = class(TFastObjectField)
  protected
    function GetValue: Double; override;
    procedure SetValue(const Value: Double); override;
  end;

  TFastScale = class(TFastObjectField)
  protected
    function GetValue: Double; override;
    procedure SetValue(const Value: Double); override;
  end;

  TFastRotate = class(TFastObjectField)
  protected
    function GetValue: Double; override;
    procedure SetValue(const Value: Double); override;
  end;

  TFastObjectType = class of TFastObjectField;

  function TypeOfFast(const AName: String): TFastObjectType;

implementation

uses
  uEngine2D;

function TypeOfFast(const AName: String): TFastObjectType;
begin
  Result := Nil;
  if (AName = 'width') or (AName = 'w') then Result := TFastWidth;
  if (AName = 'height') or (AName = 'h') then Result := TFastHeight;
  if (AName = 'scale') or (AName = 'sc') then Result := TFastScale;
  if (AName = 'rotate') or (AName = 'angle') then Result := TFastRotate;
  if (AName = 'left') or (AName = 'x') then Result := TFastX;
  if (AName = 'leftborder') then Result := TFastLeftBorder;
  if (AName = 'rightborder') then Result := TFastRightBorder;
  if (AName = 'topborder') then Result := TFastTopBorder;
  if (AName = 'bottomborder') then Result := TFastBottomBorder;
  if (AName = 'top') or (AName = 'y') then Result := TFastY;
end;


{ TFastFields }

procedure TFastFields.ClearBroken;
var
  i, vN: Integer;
begin
  vN := Self.Count - 1;

  for i := 0 to vN do
    if Self[i].IsBroken then
    begin
      Delete(i);
      Self[i].Free;
    end;

end;

procedure TFastFields.ClearForSubject(
  const AObject: tEngine2DUnclickableObject);
var
  i: Integer;
  vFF: TFastField;
begin
  for i := Self.Count - 1 downto 0 do
    if (Self[i] is TFastObjectField) then
      if TFastObjectField(Self[i]).Subject = AObject then
      begin
        vFF := Self[i];
        Self.Delete(vFF);
        vFF.Free;
      end;
end;

constructor TFastFields.Create(AIsHor: TDelegate<Boolean>);
begin
  inherited Create;
  FIsHor := AIsHor;
end;

destructor TFastFields.Destroy;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    Items[i].Free;

  inherited;
end;

function TFastFields.IsHor: Boolean;
begin
  Result := FIsHor;
end;

{ TFastWidth }

function TFastWidth.GetValue: Double;
begin
  Result := fObject.w * fObject.ScaleX;
end;

procedure TFastWidth.SetValue(const Value: Double);
begin
  fObject.Scale := Value / (fObject.w * fObject.ScaleX);
end;

{ TFastHeight }

function TFastHeight.GetValue: Double;
begin
  Result := fObject.h * fObject.ScaleY;
end;

procedure TFastHeight.SetValue(const Value: Double);
begin
  fObject.Scale := Value / (fObject.h * fObject.ScaleX);
//  fObject.h := Value;
end;

{ TFastX }

function TFastX.GetValue: Double;
begin
  Result := fObject.x;
end;

procedure TFastX.SetValue(const Value: Double);
begin
  fObject.x := Value;
end;

{ TFastY }

function TFastY.GetValue: Double;
begin
  Result := fObject.y;
end;

procedure TFastY.SetValue(const Value: Double);
begin
  fObject.y := Value;
end;

{ TFastScale }

function TFastScale.GetValue: Double;
begin
  Result := fObject.ScaleX;
end;

procedure TFastScale.SetValue(const Value: Double);
begin
  fObject.Scale := Value;
end;

{ TFastRotate }

function TFastRotate.GetValue: Double;
begin
  Result := fObject.Rotate;
end;

procedure TFastRotate.SetValue(const Value: Double);
begin
  fObject.Rotate := Value;
end;

{ TFastObjectFields }

constructor TFastObjectField.Create(const AObject: tEngine2DUnclickableObject);
begin
  fObject := AObject;
end;

destructor TFastObjectField.Destroy;
begin
  fObject := Nil;
  inherited;
end;

function TFastObjectField.IsBroken: Boolean;
begin
  if fObject = Nil then
    Result := True
  else
    Result := inherited;
end;

{ TFastEngineField }

constructor TFastEngineField.Create(const AValue: PInteger);
begin
  FValue := AValue;
  //fEngine := APointer;
end;

destructor TFastEngineField.Destroy;
begin
  FValue := Nil;
//  fEngine := Nil;
  inherited;
end;

{ TFastEngineWidth }

function TFastEngineWidth.GetValue: Double;
begin
  Result := FValue^;
//  Result := TEngine2d(fEngine).width;
end;

procedure TFastEngineWidth.SetValue(const Value: Double);
begin
  raise Exception.Create('You can not directly set Engine Width by Fast Field');
end;

{ TFastEngineHeight }

function TFastEngineHeight.GetValue: Double;
begin
  Result := FValue^;
  //Result := TEngine2d(fEngine).height;
end;

procedure TFastEngineHeight.SetValue(const Value: Double);
begin
  raise Exception.Create('You can not directly set Engine Height by Fast Field');
end;

{ TFastField }

function TFastField.IsBroken: Boolean;
begin
  Result := False;
end;

function TFastField.Value: Double;
begin
  Result := Self.GetValue;
end;

{ TFastLeftBorder }

function TFastLeftBorder.GetValue: Double;
begin
  Result := fObject.x - fObject.W * fObject.ScaleX * 0.5;
end;

procedure TFastLeftBorder.SetValue(const Value: Double);
begin
  fObject.x := Value + fObject.W * fObject.ScaleX * 0.5;
end;

{ TFastRightBorder }

function TFastRightBorder.GetValue: Double;
begin
  Result := fObject.x + fObject.W * fObject.ScaleX * 0.5;
end;

procedure TFastRightBorder.SetValue(const Value: Double);
begin
  fObject.x := Value - fObject.W * fObject.ScaleX * 0.5;
end;

{ TFastTopBorder }

function TFastTopBorder.GetValue: Double;
begin
  Result := fObject.y - fObject.H * fObject.ScaleY * 0.5;
end;

procedure TFastTopBorder.SetValue(const Value: Double);
begin
  fObject.y  := Value + fObject.H * fObject.ScaleY * 0.5;
end;

{ TFastBottomBorder }

function TFastBottomBorder.GetValue: Double;
begin
  Result := fObject.y + fObject.H * fObject.ScaleY * 0.5;
end;

procedure TFastBottomBorder.SetValue(const Value: Double);
begin
  fObject.y  := Value - fObject.H * fObject.ScaleY * 0.5;
end;

end.
