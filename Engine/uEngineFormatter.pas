unit uEngineFormatter;

interface

uses
  System.SysUtils, System.Classes, System.RegularExpressions,
  uIntersectorClasses,
  uExpressionParser, uNamedList, uEngine2DClasses, uTextProc, uEngine2DObject,
  uEngine2DUnclickableObject, uFastFields, uConstantGroup, uParserValue;

type
  // Это по сути обёртка для TExpression, позволяющая задавать форматирование
  // объектов таким образом
  // Sprite.Format := 'Width:Engine.Width; Height:Sprite2.Height*0.5';
  TEngineFormatter = class
  private
    FObject: tEngine2DObject;
    FText: String;
    FParent: Pointer;
    FWidth, FHeight: TExpression;
    FMaxWidth, FMinWidth: TExpression;
    FMaxHeight, FMinHeight: TExpression;
    FX, FY: TExpression;
    FRotate: TExpression;
    FScale: TExpression;
    FScaleX, FScaleY: TExpression;
    FRotateIfHor: TExpression;
    FXIfHor: TExpression;
    FYIfHor: TExpression;
    function CreateIfNil(var vExp: TExpression): TExpression;
    procedure SetText(const Value: String);
    function WhatExpression(const AText: String): TExpression;
    function GetPosition: TPosition; // Выдает ссылку на нужный параметр
    function DefineSelf(const AText: String): String;
    function IsFunction(const AText: String): Boolean;
    function IsDotProperty(const AText: String): Boolean;
  public
    property Parent: Pointer read FParent write FParent; // Ссылка на Engine2D
    property Text: String read FText write SetText;
    property Width: TExpression read FWidth;
    property Height: TExpression read FHeight;
    property MaxWidth: TExpression read FMaxWidth;
    property MaxHeight: TExpression read FMaxHeight;
    property MinWidth: TExpression read FMinWidth;
    property MinHeight: TExpression read FMinHeight;
    property X: TExpression read FX;
    property Y: TExpression read FY;
    property Rotate: TExpression read FRotate;
    property Scale: TExpression read FScale; // Присваивает сразу ScaleX и ScaleY
    property ScaleX: TExpression read FScaleX;
    property ScaleY: TExpression read FScaleY;
   // Оказыается нужны координаты для вертикальной и горизонталньой версии
    property XIfHor: TExpression read FXIfHor;
    property YIfHor: TExpression read FYIfHor;
    property RotateIfHor: TExpression read FRotateIfHor;

    property Position: tPosition read GetPosition;
    property Subject: tEngine2DObject read FObject;

    procedure Format; virtual;
    constructor Create(AObject: tEngine2DObject); virtual;
    destructor Destroy; override;
  end;

implementation

uses
  uEngine2D, uFunctionGroup;

{ TEngineFormatter }

constructor TEngineFormatter.Create(AObject: tEngine2DObject);
begin
  FObject := AObject;
  FParent := AObject.Parent;
end;

function TEngineFormatter.CreateIfNil(var vExp: TExpression): TExpression;
begin
  if vExp = Nil then
    vExp := TExpression.Create;

  Result := vExp;
end;

function TEngineFormatter.DefineSelf(const AText: String): String;
var
  vReg: TRegEx;
  vMatches: TMatchCollection;
  vMatch: TMatch;
  vRes, vTmp, vName, vObjName: String;
  vOffset: Integer; // Сдвиг позиций при вставлении названия объекта и точки
  vEngine: tEngine2d;
begin
  vReg := TRegEx.Create('[a-zA-Z0-9]*[^0-9\*\+\-\/\^\)\(][a-zA-Z0-9]*');
  vMatches := vReg.Matches(AText);
  vRes := AText;
  vOffset := 0;
  vEngine := tEngine2d(FParent);

//  vEngine.SpriteList.NameIfHere(FObject);
  for vMatch in vMatches do
    if not IsFunction(vMatch.Value) and (not IsDotProperty(vMatch.Value)) then
    begin
      vTmp := Copy(vRes, vMatch.Index, vMatch.Length);
      vObjName := vEngine.SpriteList.NameIfHere(FObject);
      if vObjName = '' then
        vObjName := 'shadow';
      vName := vObjName+'.'+vTmp;

      vRes :=
        Copy(vRes, 1, vMatch.Index + vOffset - 1) +
        vName +
        Copy(vRes, vMatch.Index + vOffset + Length(vTmp), Length(VRes));
      vOffset := vOFfset + Length(vEngine.SpriteList.NameIfHere(FObject)+'.');
    end;

  Result := vRes;
end;

destructor TEngineFormatter.Destroy;
begin
  if FX <> Nil then FX.Free;
  if FY <> Nil then FY.Free;
  if FWidth <> Nil then FWidth.Free;
  if FHeight <> Nil then FHeight.Free;
  if FMaxWidth <> Nil then FMaxWidth.Free;
  if FMaxHeight <> Nil then FMaxHeight.Free;
  if FMinWidth <> Nil then FMinWidth.Free;
  if FMinHeight <> Nil then FMinHeight.Free;
  if FScale <> Nil then FScale.Free;
  if FScaleX <> Nil then FScaleX.Free;
  if FScaleY <> Nil then FScaleY.Free;
  if FRotate <> Nil then FRotate.Free;

  inherited;
end;

procedure TEngineFormatter.Format;
begin
  if FWidth <> Nil then FObject.Scale := FWidth.Value / FObject.w;
  if FHeight <> Nil then FObject.Scale := FHeight.Value / FObject.h;
  if FMaxWidth <> Nil then
    if FObject.w * FObject.ScaleX > FMaxWidth.Value then
      FObject.Scale := FMaxWidth.Value / FObject.w;
  if FMaxHeight <> Nil then
    if FObject.h * FObject.ScaleY  > FMaxHeight.Value then
      FObject.Scale := FMaxHeight.Value / FObject.h;
  if FMinWidth <> Nil then
    if FObject.w * FObject.ScaleX  < FMinWidth.Value then
      FObject.Scale := FMinWidth.Value / FObject.w;
  if FMinHeight <> Nil then
    if FObject.h * FObject.ScaleY  < FMinHeight.Value then
      FObject.Scale := FMinHeight.Value / FObject.h;
  if FRotate <> Nil then FObject.Rotate := FRotate.Value;
  if FScale <> Nil then
    FObject.Scale := FScale.Value;
  if FScaleX <> Nil then
    FObject.ScaleX := FScaleX.Value;
  if FScaleY <> Nil then
    FObject.ScaleY := FScaleY.Value;
  if FX <> Nil then FObject.x := FX.Value;
  if FY <> Nil then FObject.y := FY.Value;

  if tEngine2d(FParent).IfHor then
  begin
    if FXIfHor <> Nil then FObject.x := FXIfHor.Value;
    if FYIfHor <> Nil then FObject.y := FYIfHor.Value;
    if FRotateIfHor <> Nil then FObject.Rotate := FRotateIfHor.Value;
  end;


end;

function TEngineFormatter.GetPosition: tPosition;
var
  vRes: tPosition;
begin
  vRes := ClearPosition;
  if FX <> Nil then
    vRes.x := FObject.x;//Self.X.Value;
  if FY <> Nil then
    vRes.y := FObject.y;//Self.Y.Value;
  if FRotate <> Nil then
    vRes.rotate := FObject.Rotate;//Self.Rotate.Value;
  if FScale <> Nil then
  begin
    vRes.scaleX := FObject.ScaleX;// Self.Scale.Value;
    vRes.scaleY := FObject.ScaleY;
  end;
  if FScaleX <> Nil then
    vRes.scaleX := FObject.ScaleX;
  if FScaleY <> Nil then
    vRes.scaleY := FObject.ScaleY;



  Result := vRes;
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
  vEngine: tEngine2d; // Ссылка на движок
  vTmpObject: tEngine2DObject;
  vName, vNewName: String;
  vVarList: TNamedList<TValue>;
begin
  FText := Value;

  vText := StringReplace(Text, ' ', '',[rfReplaceAll]);
  vText := StringReplace(vText, ',', '.',[rfReplaceAll]);

   //  vText := DefineSelf(vText);
 // НАДО УЧЕСТЬ ПРОБЛЕМУ ДВОЕТОЧИЙ И ТОЧКИ С ЗАПЯТОЙ

  vArr := Split(vText, ';');// ExplodeBy(vText, ';');
  vN := vArr.Count - 1;

  for i := 0 to vN do
  begin
    vArrName := Split(vArr[i], ':');//ExplodeBy(vArr[i],':');
    vArrName[1] := DefineSelf(vArrName[1]);
    if vArrName.Count{Length(vArrName)} = 2 then
    begin
      vExp := WhatExpression(vArrName[0]);
      vEngine := TEngine2D(FParent);
      vExp.ValueStack := vEngine.FastFields;
      vExp.Text := vArrName[1];

//      vNj := vExp.Values.Count - 1;
      vVarList := vExp.AllElements;


      vNj := vVarList.Count - 1;
//      vNj := vExp.VarNames.Count - 1;

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
              vNewName := vEngine.SpriteList.NameOf(FObject);
              vName := vNewName + '.' + vName;
            end;
            vTmpObject := vEngine.SpriteList[vArrFast[0]];
            vFast := TypeOfFast(vArrFast[1]).Create(vTmpObject);
            vEngine.FastFields.AddIfNo(vName, vFast)
          end;// else

        {  if vArrFast.Count = 1 then
          begin

//            vNewName := vEngine.SpriteList.NameOf(FObject); //  -- косяк. не выдает нейм оф
            vNewName := vEngine.SpriteList.NameIfHere(FObject); //  -- косяк. не выдает нейм оф
            if (vNewName = '') and (FObject.ClonedFrom <> Nil) then
              vNewName := vEngine.SpriteList.NameIfHere(tEngine2DObject(FObject.ClonedFrom)); //  -- косяк. не выдает нейм оф
            if vNewName <> '' then
            begin

              vTest := vVarList[j].Name;
              vName := vNewName + '.' + vName;
            //Когда происходит эта штука, видимо сдвигаются парсинги
            // Данное место работает криво. Когда допустим в форматтерсы попадает
            // выражение вроде top:height+height; то возникает необходимость использовать
            // название и заменить всё на top:randname3453.Height+randname3453.Height
            // Но TNamedList не позволяет этого сделать. Потому что он заменяет
            // значение первого найденного имени.
            // Вывод: Не делайте выражение 2 одинквоых переменных!!!
            vTestName := vVarList[j].Name;
              vExp.Values[vTestName].Text := vName;
                         //ыыы

                        // Нужно поставить тут регулярку
              vArrFast := Split(vName, '.');
              if vArrFast.Count = 2 then
              begin
                vTmpObject := vEngine.SpriteList[vArrFast[0]];
                vFast := TypeOfFast(vArrFast[1]).Create(vTmpObject);

//      Было так          vEngine.FastFields.Add(vName, vFast)
                vEngine.FastFields.AddIfNo(vName, vFast)
              end;
            end;
          end else
          begin
            // Делаем что-то типа создания псевдооюъекта
          end; }
            if vArrFast <> Nil then vArrFast.Free;
      end;



      end;
    end;
    if vArrName <> Nil then vArrName.Free;
  end;
  if vArr <> Nil then vArr.Free;
end;

function TEngineFormatter.WhatExpression(const AText: String): TExpression;
var
  vText: String;
begin
  vText := LowerCase(AText);
  Result := Nil;

  if (vText = 'width') or (vText = 'w') then Result := CreateIfNil(FWidth);
  if (vText = 'height') or (vText = 'h') then Result := CreateIfNil(FHeight);
  if (vText = 'max-width') or (vText = 'maxwidth') then Result := CreateIfNil(FMaxWidth);
  if (vText = 'max-height') or (vText = 'maxheight') then Result := CreateIfNil(FMaxHeight);
  if (vText = 'min-width') or (vText = 'minwidth') then Result := CreateIfNil(FMinWidth);
  if (vText = 'min-height') or (vText = 'minheight') then Result := CreateIfNil(FMinHeight);
  if (vText = 'x') or (vText = 'left') then Result := CreateIfNil(FX);
  if (vText = 'y') or (vText = 'top') then Result := CreateIfNil(FY);
  if (vText = 'rotate') or (vText = 'angle') then Result := CreateIfNil(FRotate);
  if (vText = 'scale') or (vText = 'sc') then Result := CreateIfNil(FScale);
  if (vText = 'scalex') or (vText = 'scx') then Result := CreateIfNil(FScaleX);
  if (vText = 'scaley') or (vText = 'scy') then Result := CreateIfNil(FScaleY);

  if (vText = 'xifhor') or (vText = 'leftifhor') then Result := CreateIfNil(FXIfHor);
  if (vText = 'yifhor') or (vText = 'topifhor') then Result := CreateIfNil(FYIfHor);
  if (vText = 'rotateifhor') or (vText = 'angleifhor') then Result := CreateIfNil(FRotateIfHor);

end;

end.







