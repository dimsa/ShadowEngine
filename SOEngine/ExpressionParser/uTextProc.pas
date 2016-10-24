unit uTextProc;

interface

uses
  System.SysUtils, System.Generics.Collections, uNamedList, System.Classes;

type

  TStrArray = array of String;

  {Значения для быстрого доступа}
  TStrAndPos = record
    Text: string;
    StartPos, EndPos: Integer;
  end;

//  TValueStack = class(TNamedList<Double>);

  function PosFromRight(const ASubStr, AStr: string; AOffset: Integer = 0): Integer;
  function PosFromLeft(const ASubStr, AStr: string;  AOffset: Integer = 1): Integer;
  function ListOfPos(const ASubstr, AStr: String): TList<Integer>;
  function ExplodeBy_(s, s1: string): tStrArray; // Разбивает как в пхп
  function Split(const AStr, ADelimiter: string): TStringList;

implementation

function Split(const AStr, ADelimiter: string): TStringList;
var
  vList: TStringList;
begin
  vList := TStringList.Create;
  vList.Clear;
  vList.LineBreak := ADelimiter;
  vList.Text{ .DelimitedText} := AStr;
  Result := vList;
//   ListOfStrings.Clear;
//   ListOfStrings.Delimiter     := Delimiter;
//   ListOfStrings.DelimitedText := Str;
end;

// Разбивает как в пхп
function Explodeby_(s, s1: string): tStrArray;
var
  i, ls1, ls, tempPos: integer;
  st, newC: string;
  res: tStrArray;
Begin
  ls1:=length(s1);
  ls:=length(s);

  tempPos := 1;
  for i := 1 to ls do
  begin
    st := copy(s, i, ls1);
    if st = s1 then
    begin
      newC := copy(s, tempPos, i - tempPos);
      setlength(res, length(res) + 1);
      res[high(res)] := newC;
      tempPos := i + ls1;
    end;
  end;

  if tempPos <= ls then
  begin
    setlength(res, length(res) + 1);
    res[high(res)] := copy(s, tempPos, ls - tempPos + 1);
  end;

  result := res;
End;

// Дает список поизиций вхождения подстроки
function ListOfPos(const ASubstr, AStr: String): TList<Integer>;
var
  vPos: Integer;
  vRes: TList<Integer>;
//  vN, vArr: Integer;
begin
  vPos := 0;
  vRes := TList<Integer>.Create;
  repeat
    vPos := Pos(ASubstr, AStr, vPos + 1);
    if vPos > 0 then
      vRes.Add(vPos);
  until vPos <= 0;

  Result := vRes;
end;

// Выдает те же значения, что и Pos, но ищет от оффсета включительно влево
// Первый символ так же имеет значени 1, а последний Length(s);
// Если оффсет больше длины, все норм, т.к. стринг как-то контроллирует всё
function PosFromRight(const ASubStr, AStr: string; AOffset: Integer = 0): Integer;
var
  vStrLen, vSubStrLen: Integer;
  vStrI, vSubStrI: Integer; // Счетчики
begin
  vStrLen := Length(AStr);
  vSubStrLen := Length(ASubStr);
  if AOffset = 0 then
    AOffset := vStrLen;

  for vStrI := AOffset downto vSubStrLen do
  begin
    for vSubStrI := vSubStrLen downto 1 do
    begin
      // Если не совпадает, то пропускаем итерацию
      if {AStr[vStrI + vSubStrI - vSubStrLen] <> ASubStr[vSubStrI])}
         copy(AStr, vStrI + vSubStrI - vSubStrLen, 1) <> copy(ASubStr, vSubStrI, 1) then
        Break;
      // Если новое значение и всё ок, то выходим со значением позиции
      if vSubStrI = 1 then
          Exit(vStrI - vSubStrLen + 1);
    end;
  end;

  Result := 0;
end;
{function PosFromRight(const ASubStr, AStr: string; AOffset: Integer = 0): Integer;
var
  vStrLen, vSubStrLen: Integer;
  vStrI, vSubStrI: Integer; // Счетчики
begin
  vStrLen := Length(AStr);
  vSubStrLen := Length(ASubStr);
  if AOffset = 0 then
    AOffset := vStrLen;

  for vStrI := AOffset downto vSubStrLen do
  begin
    for vSubStrI := vSubStrLen downto 1 do
    begin
      // Если не совпадает, то пропускаем итерацию
      if (AStr[vStrI + vSubStrI - vSubStrLen] <> ASubStr[vSubStrI]) then
        Break;
      // Если новое значение и всё ок, то выходим со значением позиции
      if vSubStrI = 1 then
          Exit(vStrI - vSubStrLen + 1);
    end;
  end;

  Result := 0;
end;       }

// Работает так же как Pos, но в случае,если указан оффсет 0, выдает результат
function PosFromLeft(const ASubStr, AStr: string; AOffset: Integer = 1): Integer;
var
  vStrLen, vSubStrLen: Integer;
  vStrI, vSubStrI: Integer; // Счетчики
begin
  vStrLen := Length(AStr);
  vSubStrLen := Length(ASubStr);

  for vStrI := AOffset to vStrLen - vSubStrLen do
  begin
    for vSubStrI := 1 to vSubStrLen do
    begin
      // Если не совпадает, то пропускаем итерацию
      if {(AStr[vStrI + vSubStrI - vSubStrLen] <> ASubStr[vSubStrI])}
        Copy(AStr, vStrI + vSubStrI - vSubStrLen, 1) <> Copy(ASubStr, vSubStrI, 1)
      then
        Break;
      // Если новое значение и всё ок, то выходим со значением позиции
      if vSubStrI = vSubStrLen then
          Exit(vStrI - vSubStrLen + 1);
    end;
  end;

  Result := 0;
end;

end.



