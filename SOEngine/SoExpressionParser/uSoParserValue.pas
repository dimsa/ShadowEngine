unit uSoParserValue;

interface

uses
  System.SysUtils;

type

 TValue = class
  strict private
    FBoundLeft, FBoundRight: Integer;
  protected
    FText: String;
    FName: String; // Название значения, обычно это LowerCase от FText
    procedure SetText(const Value: String); virtual;
   { function PosFromRight(const ASubStr, AStr: string;
      AOffset: Integer = 0): Integer;
    function PosFromLeft(const ASubStr, AStr: string;
      AOffset: Integer = 1): Integer;}
    // Делает какую-то часть текста значением
//    function AddValue(const AText: String; const ALeft, ARight: Integer): TValue;
  public
    property Name: string read FName;
    property Text: String read FText write SetText; // Оригинальный текст выражения
    // Границы в кототорых находятся положения подстроки значения в родителе
    property BoundLeft: Integer read FBoundLeft write FBoundLeft;
    property BoundRight: Integer read FBoundRight write FBoundRight;
    function Value: Double; virtual; abstract;
    constructor Create; virtual;
    destructor Destroy; override;
  end;

  TValues = class of TValue;

implementation

constructor TValue.Create;
begin
  inherited;

end;

destructor TValue.Destroy;
begin

  inherited;
end;

procedure TValue.SetText(const Value: String);
begin
  FText := Value;
  FName := LowerCase(Value);
end;
// Работает так же как Pos, но в случае,если указан оффсет 0, выдает результат
{function TValue.PosFromLeft(const ASubStr, AStr: string;
  AOffset: Integer = 1): Integer;
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
      if (AStr[vStrI + vSubStrI - vSubStrLen] <> ASubStr[vSubStrI]) then
        Break;
      // Если наткнулось на границу, то пропускаем итерацию
    //  if Self.IsBound(vStrI + vSubStrI - vSubStrLen) then
      //  Break;
      // Если новое значение и всё ок, то выходим со значением позиции
      if vSubStrI = 1 then
          Exit(vStrI - vSubStrLen + 1);
    end;
  end;

  Result := 0;
end; }

// Выдает те же значения, что и Pos, но ищет от оффсета включительно влево
// Первый символ так же имеет значени 1, а последний Length(s);
// Если оффсет больше длины, все норм, т.к. стринг как-то контроллирует всё
{function TValue.PosFromRight(const ASubStr, AStr: string;
  AOffset: Integer = 0): Integer;
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
      // Если наткнулось на границу, то пропускаем итерацию
     // if Self.IsBound(vStrI + vSubStrI - vSubStrLen) then
    //    Break;
      // Если новое значение и всё ок, то выходим со значением позиции
      if vSubStrI = 1 then
          Exit(vStrI - vSubStrLen + 1);
    end;
  end;

  Result := 0;
end; }

end.



