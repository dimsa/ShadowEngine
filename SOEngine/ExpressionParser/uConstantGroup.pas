unit uConstantGroup;

interface

uses
  System.SysUtils,
  uParserValue, uTextProc, uFastFields;

type
  // Несчитаемое значение, переменная или константа
  TConstantValue = class(TValue)
  public
    constructor Create(const AValue: String); reintroduce; virtual;
  end;

  TVariable = class(TConstantValue)
  strict private
//    FPlacedValue: Double;
//    FLowName: String; // Быстрое имя в нижнем регистре
    FPlaced: Boolean;
    FValue: TValue;
    FValueStack: TFastFields;
    function GetName: string;
//    procedure PlaceValue(const AValue: Double);// Просто выдает Text
  public
//    property LowName: string read FlowName; // Название это по сути текст переменной
    property Name: string read GetName; // Название это по сути текст переменной
    property ValueStack: TFastFields read FValueStack write FValueStack;
    //procedure PlaceValue(const AValue: TValue);
    function Value: Double; override;
    constructor Create(const AValue: String); override;
  end;



  TDouble = class(TConstantValue)
  public
    function Value: Double; override;
  end;

implementation

{ TVariable }

constructor TVariable.Create(const AValue: String);
begin
  inherited;
  Text := AValue;
//  FLowName := LowerCase(AValue);
  FPlaced := False;
  Self.BoundLeft := 1;
  Self.BoundRight := Length(AValue)
end;

function TVariable.GetName: string;
begin
  Result := LowerCase(Self.Text);
end;

{procedure TVariable.PlaceValue(const AValue: Double);
begin
  FPlaced := True;
  FPlacedValue := AValue;
end; }

function TVariable.Value: Double;
begin
  if not FPlaced then
  begin
    if FValueStack = Nil then
      raise Exception.Create('На задан стек значений для математического выражения.');

    if FValueStack.IsHere(Name) then
    begin
      FValue := FValueStack.Items[Name];
      FPlaced := True;
    end else
      raise Exception.Create('На задано значение переменной «' + Name + '»');
  end;

   Result := FValue.Value;
{ Вариант кода для другой архитектуры. Но он не нужен.
  if FPlaced then
    Result := FPlacedValue
  else
    raise Exception.Create('На задано значение переменной «' + Name + '»');}
end;

{ TDouble }

function TDouble.Value: Double;
var
  vA: Double;
  vErr: Integer;
begin
  Val(Text, vA, vErr);

  if vErr = 0 then
    Result := vA
  else
    raise Exception.Create('Ошибка константового значения, невозможно перевести в число с плавающей точкой «' + Text + '»');
end;

{ TConstantValue }

constructor TConstantValue.Create(const AValue: String);
begin
  Text := AValue;
end;

end.
