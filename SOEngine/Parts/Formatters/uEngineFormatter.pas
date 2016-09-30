unit uEngineFormatter;

interface

uses
  System.SysUtils,  System.RegularExpressions,
  uSoTypes, uGeometryClasses, uSpriteList, uCommonClasses,
  uExpressionParser, uNamedList, uEngine2DClasses, uTextProc, uEngine2DObject,
  uEngine2DUnclickableObject, uFastFields, uConstantGroup, uParserValue, uClasses;

type

  // It's storage of Expressions for fast formatting in Engine
  TFormatterDirective = class
  protected
    FObject: tEngine2DObject;
    FExpression: TExpression;
  public
    procedure Format; virtual; abstract;
    function Value: Double;
    constructor Create(const AObject: tEngine2DObject; const AExpression: TExpression);
  end;

  TWidthDir = class(TFormatterDirective)
  public
    procedure Format; override;
  end;

  THeightDir = class(TFormatterDirective)
  public
    procedure Format; override;
  end;

  TMaxWidthDir = class(TFormatterDirective)
  public
    procedure Format; override;
  end;

  TMaxHeightDir = class(TFormatterDirective)
  public
    procedure Format; override;
  end;

  TMinWidthDir = class(TFormatterDirective)
  public
    procedure Format; override;
  end;

  TMinHeightDir = class(TFormatterDirective)
  public
    procedure Format; override;
  end;

  TRotateDir = class(TFormatterDirective)
  public
    procedure Format; override;
  end;

  TScaleDir = class(TFormatterDirective)
  public
    procedure Format; override;
  end;

  TScaleXDir = class(TFormatterDirective)
  public
    procedure Format; override;
  end;

  TScaleYDir = class(TFormatterDirective)
  public
    procedure Format; override;
  end;

  TXDir = class(TFormatterDirective)
  public
    procedure Format; override;
  end;

  TYDir = class(TFormatterDirective)
  public
    procedure Format; override;
  end;

  TConditionalDirective = class(TFormatterDirective)
  protected
    FDirective: TFormatterDirective;
  public
    function IsSatisfy: Boolean; virtual; abstract;
    constructor Create(const ADirective: TFormatterDirective);
    procedure Format; override;
  end;

  TIfHorCondition = class(TConditionalDirective)
  strict private
    FIsHor: TDelegate<Boolean>;
  public
    function IsSatisfy: Boolean; override;
    constructor Create(const ADirective: TFormatterDirective; AIsHor: TDelegate<Boolean>);

  end;

  TEngineFormatter = class
  private
    FObject: tEngine2DObject;
    FList: TList<TFormatterDirective>;
    FObjects: TObjectsList;
    FFastFields: TFastFields;
    FText: String;
    procedure SetText(const Value: String);
    function CreateDirective(const AText: string; AExp: TExpression): TFormatterDirective;
    function DefineSelf(const AText: String): String;
    function IsFunction(const AText: String): Boolean;
    function IsDotProperty(const AText: String): Boolean;
  public
    property Text: String read FText write SetText;
    property Subject: tEngine2DObject read FObject;
    procedure Format; virtual;
    constructor Create(AObject: tEngine2DObject; AObjects: TObjectsList; AFastFiels: TFastFields); virtual;
    destructor Destroy; override;
  end;

implementation

uses
  uEngine2D, uFunctionGroup;

{ TEngineFormatter }

constructor TEngineFormatter.Create(AObject: tEngine2DObject; AObjects: TObjectsList; AFastFiels: TFastFields);
begin
  FObjects := AObjects;
  FFastFields := AFastFiels;
  FObject := AObject;
  FList := TList<TFormatterDirective>.Create;
end;

function TEngineFormatter.DefineSelf(const AText: String): String;
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
      vObjName := FObjects.NameIfHere(FObject);
      if vObjName = '' then
        vObjName := 'shadow';
      vName := vObjName+'.'+vTmp;

      vRes :=
        Copy(vRes, 1, vMatch.Index + vOffset - 1) +
        vName +
        Copy(vRes, vMatch.Index + vOffset + Length(vTmp), Length(VRes));
      vOffset := vOFfset + Length(FObjects.NameIfHere(FObject)+'.');
    end;

  Result := vRes;
end;

destructor TEngineFormatter.Destroy;
var
  i: Integer;
begin
  for i := 0 to FList.Count - 1 do
    FList[i].Free;
  FList.Clear;
  FList.Free;

  inherited;
end;

procedure TEngineFormatter.Format;
var
  i: Integer;
begin
  for i := 0 to FList.Count - 1 do
    FList[i].Format;
end;

function TEngineFormatter.IsDotProperty(const AText: String): Boolean;
begin
  Result := Pos('.', AText) > 0;
end;

function TEngineFormatter.IsFunction(const AText: String): Boolean;
begin
  Result := TFunction.IsFunction(AText);
end;

procedure TEngineFormatter.SetText(const Value: String);
var
  vText: string;
  vArr: TStringList;
  vArrName: TStringList; //TStrArray;
  vArrFast: TStringList;//TStrArray; // Массив для содания фастфильдов
  i, vN: Integer;
  j, vNj: Integer;
  vExp: TExpression;
  vFast: TFastField;
  vTmpObject: tEngine2DObject;
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
              vNewName := FObjects.NameOf(FObject);
              vName := vNewName + '.' + vName;
            end;
            vTmpObject := FObjects[vArrFast[0]];
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

function TEngineFormatter.CreateDirective(const AText: string; AExp: TExpression): TFormatterDirective;
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

{ TFormatterDirective }

constructor TFormatterDirective.Create(const AObject: tEngine2DObject; const AExpression: TExpression);
begin
  FExpression := AExpression;
  FObject := AObject;
end;

function TFormatterDirective.Value: Double;
begin
  Result := FExpression.Value;
end;

{ TWidthDir }

procedure TWidthDir.Format;
begin
  FObject.Scale := Self.Value / (FObject.w) ;
end;

{ THeightDir }

procedure THeightDir.Format;
begin
  FObject.Scale := Self.Value / (FObject.h);
end;

{ TMaxWidthDir }

procedure TMaxWidthDir.Format;
begin
  if FObject.w * FObject.ScaleX > Value then
    FObject.Scale := Value / FObject.w;
end;

{ TMaxHeightDir }

procedure TMaxHeightDir.Format;
begin
  if FObject.h * FObject.ScaleY  > Value then
    FObject.Scale := Value / FObject.h;
end;

{ TMinWidthDir }

procedure TMinWidthDir.Format;
begin
  if FObject.w * FObject.ScaleX  < Value then
    FObject.Scale := Value / FObject.w;
end;

{ TMinHeightDir }

procedure TMinHeightDir.Format;
begin
  if FObject.h * FObject.ScaleY  < Value then
    FObject.Scale := Value / FObject.h;
end;

{ TRotateDir }

procedure TRotateDir.Format;
begin
  FObject.Rotate := Value;
end;

{ TScaleDir }

procedure TScaleDir.Format;
begin
  FObject.Scale := Value;
end;

{ TScaleXDir }

procedure TScaleXDir.Format;
begin
  FObject.ScaleX := Value;
end;

{ TScaleYDir }

procedure TScaleYDir.Format;
begin
  FObject.ScaleY := Value;
end;

{ TXDir }

procedure TXDir.Format;
begin
  FObject.x := Value;
end;

{ TYDir }

procedure TYDir.Format;
begin
  FObject.y := Value;
end;

{ TConditionalDirective }

constructor TConditionalDirective.Create(const ADirective: TFormatterDirective);
begin
  FObject := ADirective.FObject; // Сейчас это дружественные классы, но вообще надо следить за таким
  FDirective := ADirective;
end;

procedure TConditionalDirective.Format;
begin
  if IsSatisfy then
    FDirective.Format;
end;

{ TIfHorCondition }

constructor TIfHorCondition.Create(const ADirective: TFormatterDirective;
  AIsHor: TDelegate<Boolean>);
begin
  FIsHor := AIsHor;
  inherited Create(ADirective);
end;

function TIfHorCondition.IsSatisfy: Boolean;
begin
  Result := FIsHor;
end;

end.








