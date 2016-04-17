unit uExpressionParser;

interface

uses
  System.SysUtils, System.StrUtils,
  Generics.Collections, Classes, System.RegularExpressions,
  uTextProc, uParserValue, uNamedList, uFastFields;

type
  {Парсер выражений}
  TExpression = class(TValue)
  strict private
    FValues: TNamedList<TValue>;
    FValueStack: TFastFields; // TValueStack; // Стек, откуда парсер берет значение переменных
    FVarNames: TStringList;
    function LeftExp(APos: Integer): TValue; virtual;
    function RightExp(APos: Integer): TValue; virtual;
    function OuterBrackets: TList<TStrAndPos>; virtual;
    function GetAllElements: TNamedList<TValue>;
    function StartByBrackets(const AText: string): string;
  protected
    FParsed: Boolean; // Показывает, отпарсино ли выражение.
    FExpression: String; // В отличие от оригинального текста не имеет лишних символов
    function IsBound(const APos: Integer): Boolean; virtual; // Используется родителем для уточнения, не попадает ли новое значение в границы одного из существующих
    function WhatBound(const APos: Integer): TValue; virtual; // Тоже, что IsBound, только дает указатель
    function TypeOfValue(const AText: String): TValues; virtual;
    procedure DeleteSurfaceBrackets(var AText: String);
    procedure ParseOperands; virtual; // Находит все операнды
    procedure ParseOuterFunctions; virtual; // Находит все функции
    procedure ParseOuterBrackets; virtual; // Находит все первые скобки
    procedure ParseAll; virtual;
    procedure SetText(const Value: String); override;
    function AddConstantValue(const AText: string; const ALeft, ARight: Integer): TValue; virtual; // Добавляет константу или переменную
  public
    property Expression: string read FExpression; // Изменненый текст
    property ValueStack: TFastFields read FValueStack write FValueStack;
    property Values: TNamedList<TValue> read FValues write FValues; // Распарсенные значения, принадлежащие этому значению
    property AllElements: TNamedList<TValue> read GetAllElements;
    function Value: Double; override;
    constructor Create; override;
    destructor Destroy; override;
  end;

implementation

uses
  uOperandGroup, uConstantGroup, uFunctionGroup;

{ TExpression }

function TExpression.AddConstantValue(const AText: string; const ALeft, ARight: Integer): TValue;
var
  vTmp: TValue;
begin
  vTmp := TypeOfValue(AText).Create;
  vTmp.Text := AText;
  // Если это переменная, необходимо пополнить стек имён
  if vTmp is TVariable then
  begin
    // Можно добавлять имена сразу, чтобы не было эксепшина, но это не будет работать
    // FValueStack.Add(vTmp.Text, 0);

    TVariable(vTmp).ValueStack := FValueStack;
    FVarNames.Add(LowerCase(vTmp.Text));
  end;
  vTmp.BoundLeft := ALeft;
  vTmp.BoundRight :=  ARight;
  Result := vTmp;
end;

constructor TExpression.Create;
begin
  inherited;
  FValues := TNamedList<TValue>.Create;
  FParsed := False;
  FVarNames := TStringList.Create;
  FVarNames.Duplicates := dupIgnore;
//  FValueStack := TValueStack.Create;
end;

procedure TExpression.DeleteSurfaceBrackets(var AText: String);
var
  i, vN: Integer;
  vLevel: Integer;
  vBrac: string;
begin
  vN := Length(AText);

  { TODO : You need to add there deletion of function bracket like sqrt( }
  if ((Copy(AText, 1, 1) = '(')) and (Copy(AText, Length(AText), 1) = ')') then
  begin
    vLevel := 1;
    vBrac := StartByBrackets(AText);
    for i := 1 + Length(vBrac) to vN-1 do
    begin
      if Copy(AText, i, 1) = '(' then
        vLevel := vLevel + 1;
      if Copy(AText, i, 1) = ')' then
        vLevel := vLevel - 1;

      if vLevel <= 0 then
        Exit;
    end;

    if vLevel = 1 then
    begin
      Delete(AText, 1, Length(vBrac));
      Delete(AText, Length(AText), 1);
      DeleteSurfaceBrackets(AText);
    end;
  end;

end;

{procedure TExpression.DeleteSurfaceBrackets(var AText: String);
var
  i, vN: Integer;
  vLevel: Integer;
  vBrac: string;
begin
  vN := Length(AText);

  if ((Copy(AText, 1, 1) = '(') or (IsStartByFuncName(AText))) and (Copy(AText, Length(AText), 1) = ')') then
  begin
    vLevel := 1;
    vBrac := StartByBrackets(AText);
    for i := 1 + Length(vBrac) to vN-1 do
    begin
      if Copy(AText, i, 1) = '(' then
        vLevel := vLevel + 1;
      if Copy(AText, i, 1) = ')' then
        vLevel := vLevel - 1;

      if vLevel <= 0 then
        Exit;
    end;

    if vLevel = 1 then
    begin
      Delete(AText, 1, Length(vBrac));
      Delete(AText, Length(AText), 1);
      DeleteSurfaceBrackets(AText);
    end;
  end;

end; }

function TExpression.OuterBrackets: TList<TStrAndPos>;
var
  vRes: TList<TStrAndPos>;
  i, vN: Integer; // Номера символов
  vBrCur: Integer; // Максимальная глубина скобок и текущая
  vStr: TStrAndPos;
begin
  vRes := TList<TStrAndPos>.Create;

  vN := Length(FExpression);
  vBrCur := 0;
  for i := 1 to vN do
  begin
    // Если находим скобку, увеличиваем уровень вложенности
    if {FExpression[i]}Copy(FExpression, i, 1) = '(' then
    begin
      vBrCur := vBrCur + 1;
      if (vBrCur = 1) then
      begin
        vStr.Text := '';
        vStr.StartPos := i;
      end;
    end;

    // Если текущий уровень скобок 1, то сохраняем все что внутри скобок
    if (vBrCur >= 1) then
      vStr.Text := vStr.Text + {FExpression[i]}Copy(FExpression, i, 1);

    // Если скобка закрывается, уменьшаем уровень вложенности
    if ({FExpression[i]}Copy(FExpression, i, 1) = ')') then
    begin
      if (vBrCur = 1) then
      begin
        vStr.EndPos := i;
        vRes.Add(vStr);
      end;
      vBrCur := vBrCur - 1;

      if vBrCur < 0 then
        raise Exception.Create('Ошибка обработки выражения «' + FExpression + '». Неправильное положение закрывающих скобок');
    end;
  end;

  if vBrCur <> 0 then
    raise Exception.Create('Ошибка обработки выражения «' + FExpression + '». Ошибка в количестве открывающихся и закрывающихся скобок');
  Result := vRes;
end;

procedure TExpression.ParseOuterBrackets;
var
  vList: TList<TStrAndPos>;
  vTmp: TExpression;
  i, vN: Integer;
begin
  vList := Self.OuterBrackets;
  vN := vList.Count - 1;

  for i := 0 to vN do
    if Not IsBound(vList[i].StartPos) then
    begin
      vTmp := TExpression.Create;
      vTmp.ValueStack :=Self.ValueStack;
      vTmp.BoundLeft := vList[i].StartPos;
      vTmp.BoundRight := vList[i].EndPos;
      vTmp.Text := Copy(Expression, vTmp.BoundLeft, vTmp.BoundRight - vTmp.BoundLeft + 1);
      Values.Add({vTmp.Name, }vTmp);
    end;
 //   Self.AddExpression(vList[i].StartPos, vList[i].EndPos);
end;

procedure TExpression.ParseOuterFunctions;
var
  vList: TList<TStrAndPos>;
  i, vN, j: Integer;
  vTmp: TFunction;
begin
  vList := OuterBrackets;
  vN := vList.Count - 1;

  for i := 0 to vN do
    for j := 0 to CFuncNamesCount - 1 do
    begin
     // vFN := Copy(FExpression, vList[i].StartPos - Length(CFuncNames[j]), Length(CFuncNames[j]));
      if
        LowerCase(
          Copy(
            FExpression,
            vList[i].StartPos - Length(CFuncNames[j]),
            Length(CFuncNames[j])
          )) = CFuncNames[j]
       then
       begin
         // Создаем найденную функцию
         vTmp := CFastFuncNames[j].Create;
         vTmp.ValueStack := Self.ValueStack;
         vTmp.BoundLeft := vList[i].StartPos - Length(CFuncNames[j]);
         vTmp.BoundRight := vList[i].EndPos;
         vTmp.Text := Copy(Expression, vTmp.BoundLeft, vTmp.BoundRight - vTmp.BoundLeft + 1);

         Values.Add(vTmp);
       end;
    end;
end;

destructor TExpression.Destroy;
begin
  FValues.Free;
  FVarNames.Free;
  Self.FValueStack.ClearBroken;
  inherited;
end;

function TExpression.GetAllElements: TNamedList<TValue>;
var
  vV: TValue;
  vRes: TNamedList<TValue>;
begin
  vRes := TNamedList<TValue>.Create;
  if not FParsed then
    ParseAll;

  for vV in FValues do
  begin
    if vV is TExpression then
      vRes.AddListIfNo((vV as TExpression).AllElements);
  end;
  vRes.AddListIfNo(FValues);
  Result := vRes; //FVarNames;
end;

function TExpression.Value: Double;
begin
  if Not FParsed then
    ParseAll;

  Result := Self.Values[Self.Values.Count - 1].Value;
end;

function TExpression.LeftExp(APos: Integer): TValue;
var
  i: Integer;
  vPos: Integer;
  vTmp: TValue;
  ch: String;
  vText: String;
  vLeft, vRight: Integer;
  vPosMax: Integer;
  vOp: Integer; // Номер операнда
begin
  vTmp := WhatBound(APos);
  // Если попали в уже обработанный Value, то отдаем его
  // Иначе отдаем новосозданное значение
  if vTmp <> Nil then
    Exit(vTmp);

  vOp := 0;
  vPosMax := -1;
  for i := 0 to  OperandCharCount - 1 do
  begin
    //Надо еще учитывать какой ближайший символ
    vPos := PosFromRight(OperandChar[i], Expression, APos);
    if vPos > 0 then
      if (vPosMax = -1) or (vPos > vPosMax)then
      begin
        vPosMax:= vPos;
        vOp := i; // Запоминаем номер знака в массиве
      end;
  end;

  if vPosMax > 0 then
  begin
    vPos := vPosMax;
    vPos := vPos + Length(OperandChar[vOp]);
    ch := Copy(FExpression, vPos, 1);//Expression[vPos];
    // Т.к. надо получить границу до, а не начало вхождения
    vLeft := vPos;
    vRight := APos;
    vText := Copy(FExpression, vLeft, vRight - vLeft + 1);

    // Нужна проверка, вдруг такая переменная уже есть

    vTmp := AddConstantValue(vText, vLeft, vRight);


    Self.Values.AddIfNo(vTmp.Name, vTmp);
    Exit(vTmp);
  end;

  // Если мы дошли до этого места, то мы имеем или ошибку выражения или край выражения
  if APos <= 0 then
    raise Exception.Create('Левое выражение от ' + IntToStr(APos) + ' символа не существует.');

  vLeft := 1;
  vRight := APos;
  vText := Copy(FExpression, vLeft, vRight - vLeft + 1);
  vTmp := AddConstantValue(vText, vLeft, vRight);
  Self.Values.Add({vTmp.Name, }vTmp);
  Exit(vTmp);

end;

function TExpression.RightExp(APos: Integer): TValue;
var
  i: Integer;
  vPos, vPosMin: Integer;
  vTmp: TValue;
  vText: String;
  vLeft, vRight: Integer;
begin
  vTmp := WhatBound(APos);
  // Если попали в уже обработанный Value, то отдаем его
  // Иначе отдаем новосозданное значение
  if vTmp <> Nil then
    Exit(vTmp);

  vPosMin := -1;
  for i := 0 to OperandCharCount - 1 do
  begin
    vPos := PosFromLeft(OperandChar[i], Expression, APos);
    if vPos > 0 then
      if (vPosMin = -1) or ((vPos < vPosMin)) then
        vPosMin := vPos;
  end;

  if vPosMin > 0 then
  begin
    vPos := vPosMin;
    vLeft := APos;
    vRight := vPos - 1;
    vText := Copy(FExpression, vLeft, vRight - vLeft + 1);
    vTmp := AddConstantValue(vText, vLeft, vRight);
    Self.Values.AddIfNo(vTmp.Name, vTmp);
    Exit(vTmp);
  end;

  // Если мы дошли до этого места, то мы имеем или ошибку выражения или край выражения
  if APos > Length(Expression) then
    raise Exception.Create('Правое выражение от ' + IntToStr(APos) + ' символа не существует.');

  vLeft := APos;
  vRight := Length(FExpression);
  vText := Copy(FExpression, vLeft, vRight - vLeft + 1);
  vTmp := AddConstantValue(vText, vLeft, vRight);
  Self.Values.Add({vTmp.Name, }vTmp);
  Exit(vTmp);
end;

procedure TExpression.SetText(const Value: String);
begin
  inherited;
  FExpression := StringReplace(Text, ' ', '',[rfReplaceAll]);
  FParsed := False;
  DeleteSurfaceBrackets(FExpression);
  if not FParsed then
    ParseAll;
end;

function TExpression.StartByBrackets(const AText: string): string;
var
  i: Integer;
begin
  for i := 0 to CFuncNamesCount - 1 do
    if Copy(AText, 1, Length(CFuncNames[i]) + 1) = (CFuncNames[i] + '(')  then
      Exit(Copy(AText, 1, Length(CFuncNames[i]) + 1));
  if Copy(AText, 1, 1) = '(' then
    Exit('(');

  Result := '';
end;

function TExpression.TypeOfValue(const AText: String): TValues;
var
  vReg: TRegEx;
  vA: Double;
  vErr: Integer;
  vPattern: String;
begin
  // Проверяем, число ли это
  Val(AText, vA, vErr);
  if vErr = 0 then
    Exit(TDouble);

  // Если не число, то проверяем на допустимые символы
  vPattern := '[abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890_\.\-]';
  vReg := TRegEx.Create(vPattern);
  if vReg.IsMatch(AText) then
    Result := TVariable
  else
    raise Exception.Create('Ошибка обработки переменной «'+ AText +'». Название не соответствует регулярному выражению '+ vPattern);
end;

procedure TExpression.ParseAll;
var
  vTmp: TValue;
begin
  // Находим все функции в первом уровне. Пример: sin(45+90)
  ParseOuterFunctions;
  // Все оставшиеся скобки верхнего уровня обрабатываем
  ParseOuterBrackets;
  // Делаем так, типа нету скобок и все значения первого уровня
  // И парсим операнды
  ParseOperands;

  // Если нет ни одного операнда, ни одной  функции, то создаем константу
  if Self.Values.Count <= 0 then
  begin
    vTmp := AddConstantValue(FExpression, 1, Length(FExpression));
    Self.Values.Add(vTmp);
  end;

  FParsed := True;
end;


procedure TExpression.ParseOperands;
var
  j, i, vN: Integer;
  vTmp: TOperand;
  vList: TList<Integer>;
  vValLeft, vValRight: TValue;
begin
  // Убираем все пробелы
   vN := Length(Operands) - 1;

  for i := 0 to vN do
  begin
    vList := ListOfPos(Operands[i], Expression);
    for j := 0 to vList.Count - 1 do
      if Not IsBound(vList[j]) then
      begin
        vValLeft := LeftExp(vList[j] - 1);
        vValRight := RightExp(vList[j] + Length(Operands[i]));
        vTmp := FastOperandClass[i].Create(vValLeft, vValRight);

        vTmp.BoundLeft := vValLeft.BoundLeft;
        vTmp.BoundRight := vValRight.BoundRight;
        vTmp.Text := Copy(FExpression, vValLeft.BoundLeft, vValRight.BoundRight - vValLeft.BoundLeft + 1);

        Self.Values.Add({vTmp.Name,} vTmp);
      end;
  end;

end;

function TExpression.IsBound(const APos: Integer): Boolean;
var
  vN, I: Integer;
begin
  vN := FValues.Count - 1;
  for i := vN downto 0 do
    if (APos >= FValues[i].BoundLeft) and (APos <= FValues[i].BoundRight) then
      Exit(True);
  Result := False;
end;

function TExpression.WhatBound(const APos: Integer): TValue;
var
  vN, I: Integer;
begin
  vN := FValues.Count - 1;

  for i := vN downto 0 do
    if (APos >= FValues[i].BoundLeft) and (APos <= FValues[i].BoundRight) then
      Exit(FValues[i]);
  Result := Nil;
end;

end.










