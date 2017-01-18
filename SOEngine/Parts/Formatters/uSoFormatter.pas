unit uSoFormatter;

interface

uses
  FMX.Types, System.Classes, System.Generics.Collections, System.RegularExpressions, System.SysUtils,
  uTextProc,
  uSoObject, uSoBasePart, uSoFormatterDirective, uSoObjectKeeper, uSoFastFields, uSoExpressionParser,
  uNamedList;

type
  TSoFormatter = class(TSoBasePart)
 private
    FObject: TSoObject;
    FList: TList<TFormatterDirective>;
    FWorld: TSoObject;
    FFastFields: TFastFields;
    FText: String;
    procedure SetText(const Value: String);
    function CreateDirective(const AText: string; AExp: TExpression): TFormatterDirective;
    function DefineSelf(const AText: String): String;
    function IsFunction(const AText: String): Boolean;
    function IsDotProperty(const AText: String): Boolean;
    function GetObjectNameFromWorld: string;
    function GetObjectFromWorldByName(const AName: string): TSoObject;
  public
    property Text: String read FText write SetText;
    property Subject: TSoObject read FObject;
    procedure Format; virtual;
    constructor Create(AObject: TSoObject; AWorld: TSoObject; AFastFiels: TFastFields); virtual;
    destructor Destroy; override;
  end;

implementation

uses
  uSoFunctionGroup, uSoParserValue, uSoConstantGroup, uSoContainer;

{ TSoFormatter }

constructor TSoFormatter.Create(AObject: TSoObject; AWorld: TSoObject; AFastFiels: TFastFields);
begin
  FWorld := AWorld;
  FFastFields := AFastFiels;
  FObject := AObject;
  FList := TList<TFormatterDirective>.Create;
end;

function TSoFormatter.DefineSelf(const AText: String): String;
var
  vReg: TRegEx;
  vMatches: TMatchCollection;
  vMatch: TMatch;
  vRes, vTmp, vName, vObjName: String;
  vOffset: Integer; // Сдвиг позиций при вставлении названия объекта и точки
begin
  vReg := TRegEx.Create('[a-zA-Z0-9]*[^0-9\*\+\-\/\^\)\(][a-zA-Z0-9]*');
  vMatches := vReg.Matches(AText);
  vRes := AText;
  vOffset := 0;

  for vMatch in vMatches do
    if not IsFunction(vMatch.Value) and (not IsDotProperty(vMatch.Value)) then
    begin
      vTmp := Copy(vRes, vMatch.Index, vMatch.Length);
      // You can get name of object from World Object
      vObjName := GetObjectNameFromWorld;
      if vObjName = '' then
        vObjName := 'shadow';
      vName := vObjName + '.' + vTmp;

      vRes :=
        Copy(vRes, 1, vMatch.Index + vOffset - 1) +
        vName +
        Copy(vRes, vMatch.Index + vOffset + Length(vTmp), Length(VRes));
      vOffset := vOffset + Length(vObjName + '.');
    end;

  Result := vRes;
end;

destructor TSoFormatter.Destroy;
var
  i: Integer;
begin
  for i := 0 to FList.Count - 1 do
    FList[i].Free;
  FList.Clear;
  FList.Free;

  inherited;
end;

procedure TSoFormatter.Format;
var
  i: Integer;
begin
  for i := 0 to FList.Count - 1 do
    FList[i].Format;
end;

function TSoFormatter.GetObjectFromWorldByName(const AName: string): TSoObject;
begin
  Result := nil;
end;

function TSoFormatter.GetObjectNameFromWorld: string;
begin
  // TODO: Get name of object from world
  Result := '';
  //TSoContainer(FWorld.Container) //FObjects.NameOf(FObject);
end;

function TSoFormatter.IsDotProperty(const AText: String): Boolean;
begin
  Result := Pos('.', AText) > 0;
end;

function TSoFormatter.IsFunction(const AText: String): Boolean;
begin
  Result := TFunction.IsFunction(AText);
end;

procedure TSoFormatter.SetText(const Value: String);
var
  vText: string;
  vArr: TStringList;
  vArrName: TStringList; //TStrArray;
  vArrFast: TStringList;//TStrArray; // Массив для содания фастфильдов
  i, vN: Integer;
  j, vNj: Integer;
  vExp: TExpression;
  vFast: TFastField;
  vTmpObject: TSoObject;
  vName, vNewName: String;
  vVarList: TNamedList<TValue>;
begin
  FText := Value;

  vText := StringReplace(Text, ' ', '',[rfReplaceAll]);
  vText := StringReplace(vText, ',', '.',[rfReplaceAll]);

   //  vText := DefineSelf(vText);
 // НАДО УЧЕСТЬ ПРОБЛЕМУ ДВОЕТОЧИЙ И ТОЧКИ С ЗАПЯТОЙ

  vArr := Split(vText, ';');
  vN := vArr.Count - 1;

  for i := 0 to vN do
  begin
    vArrName := Split(vArr[i], ':');
    vArrName[1] := DefineSelf(vArrName[1]);
    if vArrName.Count = 2 then
    begin
      vExp := TExpression.Create;
      vExp.ValueStack := FFastFields;
      vExp.Text := vArrName[1];
      FList.Add(CreateDirective(vArrName[0], vExp));

      vVarList := vExp.AllElements;


      vNj := vVarList.Count - 1;

      for j := 0 to vNj do
        if  vVarList[j]{ vExp.Values[j]} Is TVariable then

      begin
      // Если значения данной переменной нету, то добавляем щоз
        vName := vVarList[j].Name;
        if not vExp.ValueStack.IsHere(vName) then
        begin
          vArrFast := Split(vName, '.');
          if (vArrFast.Count = 2) then
          begin
            if LowerCase(vArrFast[0]) = 'self' then
            begin
              vNewName := GetObjectNameFromWorld; //FObjects.NameOf(FObject);
              vName := vNewName + '.' + vName;
            end;
            vTmpObject := GetObjectFromWorldByName(vArrFast[0]);//FObjects[vArrFast[0]];
            vFast := TypeOfFast(vArrFast[1]).Create(vTmpObject);
            FFastFields.AddIfNo(vName, vFast)
          end;

          if vArrFast <> Nil then
            vArrFast.Free;
        end;
      end;
    end;
    if vArrName <> Nil then vArrName.Free;
  end;
  if vArr <> Nil then vArr.Free;
end;

function TSoFormatter.CreateDirective(const AText: string; AExp: TExpression): TFormatterDirective;
var
  vText: String;
  vHor: Boolean;
begin
  vText := LowerCase(AText);
  Result := Nil;

  vHor := False;
  if Pos('ifhor', vText) > 0 then
  begin
    vText := StringReplace(vText, 'ifhor', '', [rfReplaceAll]);
    vHor := True;
  end;

  if (vText = 'width') or (vText = 'w') then Result := TWidthDir.Create(FObject, AExp);
  if (vText = 'height') or (vText = 'h') then Result := THeightDir.Create(FObject, AExp);
  if (vText = 'max-width') or (vText = 'maxwidth') then Result := TMaxWidthDir.Create(FObject, AExp);
  if (vText = 'max-height') or (vText = 'maxheight') then Result := TMaxHeightDir.Create(FObject, AExp);
  if (vText = 'min-width') or (vText = 'minwidth') then Result := TMinWidthDir.Create(FObject, AExp);
  if (vText = 'min-height') or (vText = 'minheight') then Result := TMinHeightDir.Create(FObject, AExp);
  if (vText = 'x') or (vText = 'left') then Result := TXDir.Create(FObject, AExp);
  if (vText = 'y') or (vText = 'top') then Result := TYDir.Create(FObject, AExp);
  if (vText = 'rotate') or (vText = 'angle') then Result := TRotateDir.Create(FObject, AExp);
  if (vText = 'scale') or (vText = 'sc') then Result := TScaleDir.Create(FObject, AExp);
  if (vText = 'scalex') or (vText = 'scx') then Result := TScaleXDir.Create(FObject, AExp);
  if (vText = 'scaley') or (vText = 'scy') then Result := TScaleYDir.Create(FObject, AExp);

  if vHor then
    Result := TIfHorCondition.Create(Result, FFastFields.IsHor);

end;

end.
