unit uEngine2D;

{******************************************************************************
Shadow Object Engine (SO Engine)
By Dmitriy Sorokin.

Some comments in English, some in Russian. And it depends on mood :-) Sorry!)
*******************************************************************************}

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Platform,
  FMX.Objects, Math, System.SyncObjs, {$I 'Utils\DelphiCompatability.inc'}
  uClasses, uEngine2DThread, uEngine2DObject, uEngine2DUnclickableObject,
  uEngine2DSprite, uEngine2DText, uEngine2DClasses, uFormatterList, uEngineFormatter,
  uSpriteList, uEngine2DManager,
  uEngine2DResources, uEngine2DAnimation, uNamedList, uEngine2DAnimationList,
  uFastFields, uEasyDevice;

type

  TEngine2d = class
  strict private
    FEngineThread: TEngineThread; // Поток в котором происходит отрисовка
    FOptions: TEngine2DOptions; // Настройки движка
    FObjects: TObjectsList; // Массив спрайтов для отрисовки
    FFastFields: TFastFields; // Содержит ссылки на TFastField, которые представляют собой найденные значения определенных спрайтов
    FObjectOrder: TIntArray; // Массив порядка отрисовки. Нужен для уменьшения кол-ва вычислений, содержит номер спрайта
    FResources: TEngine2DResources;//tResourceArray; // Массив битмапов
    FFormatters: TFormatterList; // Массив Форматтеров спрайтов
    FAnimationList: TEngine2DAnimationList; // Массив анимаций
    FObjectCreator: TEngine2DManager;
    FMouseDowned: TIntArray; // Массив спрайтов движка, которые находились под мышкой в момент нажатия
    FMouseUpped: TIntArray; // Массив спрайтов движка, которые находились под мышкой в момент отжатия
    FClicked: TIntArray; // Массив спрайтов движка, которые попали под мышь
    FStatus: Byte; // Состояние движка 0-пауза, 1-работа
    FlX, FlY: single; // o_O Для масштабирования на смартфоны что-то
    FIsMouseDowned: Boolean; // Хранит состояние нажатости мыши
    FImage: tImage; // Имедж, в котором происходит отрисовка
    FBackGround: tBitmap; // Бэкграунд. Всегда рисуется в Repaint на весь fImage
    FCritical: TCriticalSection; // Критическая секция движка
    FWidth, FHeight: integer; // Размер поля имеджа и движка
//    FDebug: Boolean; // Не очень нужно, но помогает отлаживать те места, когда непонятно когда появляется ошибка
    FBackgroundBehavior: TProcedure;
    FInBeginPaintBehavior: TProcedure;
    FInEndPaintBehavior: TProcedure;

    // Механизм теневого объекты необычен. Но кроме всего прочего TEngine2DObject не имеет способов определения
    {FShadowSprite: tSprite; //
    FShadowText: TEngine2dText; }
    FShadowObject: tEngine2DObject;

    procedure prepareFastFields;
    procedure prepareShadowObject;
    procedure setStatus(newStatus: byte);
    procedure setObject(index: integer; newSprite: tEngine2DObject);
    function getObject(index: integer): tEngine2DObject;
    procedure SetWidth(AWidth: integer); // Установка размера поля отрисовки движка
    procedure SetHeight(AHeight: integer); // Установка размера поля отрисовки движка
    procedure setBackGround(ABmp: tBitmap);

    procedure BackgroundDefaultBehavior;
    procedure InBeginPaintDefaultBehavior;
    procedure InEndPaintDefaultBehavior;

    procedure SetBackgroundBehavior(const Value: TProcedure);
  protected
    // Ключевые списки движка.
    property Resources: TEngine2DResources read FResources;
    property AnimationList: TEngine2DAnimationList read FAnimationList;
    property FormatterList: TFormatterList read FFormatters;
    property SpriteList: TObjectsList read FObjects;
    property SpriteOrder: TIntArray read FObjectOrder;
    property FastFields: tFastFields read FFastFields; // Быстрый вызов для экспрешенсов
    property Sprites[index: integer]: tEngine2DObject read getObject write setObject;

 //   property SpriteCount: integer read getSpriteCount;
  public
    // Ключевые свойства движка
    property EngineThread: TEngineThread read FEngineThread;
    property Image: TImage read FImage write FImage;
    property BackgroundBehavior: TProcedure read FBackgroundBehavior write SetBackgroundBehavior;
    property InBeginPaintBehavior: TProcedure read FInBeginPaintBehavior write FInBeginPaintBehavior;
    property InEndPaintBehavior: TProcedure read FInBeginPaintBehavior write FInBeginPaintBehavior;

    function IsMouseDowned: Boolean;// read FIsMouseDowned;
    property Status: byte read FStatus write setStatus;
    property Width: integer read FWidth write setWidth;
    property Height: integer read FHeight write setHeight;

    // Last Clicked, MouseDowned and MouseUpped objects id
    property Clicked: TIntArray read FClicked;
    property Downed: TIntArray read FMouseDowned;
    property Upped: TIntArray read FMouseUpped;

    property Critical: TCriticalSection read FCritical;

    property Background: TBitmap read FBackGround write setBackGround;
    property Options: TEngine2dOptions read FOptions write FOptions;

    function IsHor: Boolean; // Return True, if Engine.Width > Engine.Height
    procedure Resize;

    procedure MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; x, y: single; const ACount: Integer = -1); virtual; // ACount is quantity of sorted object that will be MouseDowned -1 is all.
    procedure MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; x, y: single; const ACount: Integer = -1; const AClickObjects: Boolean = True); virtual; // ACount is quantity of sorted object that will be MouseUpped -1 is all.
    procedure Click(const ACount: Integer = -1); virtual; // It must be Called after MouseUp if in MouseUp was AClickObjects = False;

    procedure AssignShadowObject(ASpr: tEngine2DObject); // Ассигнет спрайт в ShadowObject
    property ShadowObject: tEngine2DObject read FShadowObject;  // Указатель на Теневой объект.

    procedure ClearSprites; // Очищает массив спрайтов, т.е. является подготовкой к полной перерисовке
    procedure ClearTemp; // Очищает массивы выбора и т.д. короче делает кучу полезных вещей.

    procedure LoadResources(const AFileName: String);
    procedure LoadSECSS(const AFileName: String);
    procedure LoadSEJSON(const AFileName: String);

    procedure Init(AImage: tImage); // Инициализация движка, задаёт рисунок на форме, на которому присваиватся fImage
    procedure Repaint; virtual;

    procedure Start; virtual; // Включает движок
    procedure Stop; virtual;// Выключает движок

    constructor Create; virtual;
    destructor Destroy; override;

    // You should use Manager to Work with Engine
    property Manager: TEngine2DManager read FObjectCreator; // Позволяет быстрее и проще создавать объекты

    const
      CGameStarted = 1;
      CGameStopped = 255;
  end;

const
  pi180 = 0.0174532925; // (1/180)*pi для уменьшение количества пересчетов

implementation

uses
  System.RegularExpressions, System.JSON, uNewFigure;

{ tEngine2d }

procedure TEngine2d.AssignShadowObject(ASpr: tEngine2DObject);
begin
  //  В данном контексте следует различть наследников TEngine2DObject, т.к. может попасться текст
  FShadowObject.Position := ASpr.Position;
{  FShadowObject.ScaleX := ASpr.ScaleX;
  FShadowObject.ScaleY := ASpr.ScaleY;  }
  tSprite(FShadowObject).Resources := tSprite(ASpr).Resources;
end;

procedure TEngine2d.BackGroundDefaultBehavior;
begin
  with Self.Image do
    Bitmap.Canvas.DrawBitmap(
      FBackGround,
      RectF(0, 0, FBackGround.width, FBackGround.height),
      RectF(0, 0, bitmap.width, bitmap.height),
      1,
      true);
end;

procedure TEngine2d.clearSprites;
var
  i: integer;
begin
 for i := 0 to FObjects.Count - 1 do
    FObjects[i].free;

  setLength(FObjectOrder, 0);
end;

procedure TEngine2d.clearTemp;
begin
  setLength(self.FClicked, 0);
end;

procedure TEngine2d.Click(const ACount: Integer);
var
  i, vCount: Integer;
begin
  if ACount = -1 then
    vCount := Length(FClicked)
  else
    vCount := ACount;

  for i := 0 to Min(vCount, Length(FClicked)) - 1 do
    Sprites[FClicked[i]].OnClick(Sprites[FClicked[i]]);
end;

constructor TEngine2d.Create; // (createSuspended: boolean);
begin
  FCritical := TCriticalSection.Create;
  FEngineThread := tEngineThread.Create;
  FObjectOrder := TIntArray.Create(0);
  FResources := TEngine2DResources.Create(FCritical);
  FAnimationList := TEngine2DAnimationList.Create(FCritical);
  FFormatters := TFormatterList.Create(FCritical, Self);
  FObjects := TObjectsList.Create(FCritical);
  FOptions.Up([EAnimateForever]);
  FOptions.Down([EClickOnlyTop]);

  FBackgroundBehavior := BackgroundDefaultBehavior;
  FInBeginPaintBehavior := InBeginPaintDefaultBehavior;
  FInEndPaintBehavior := InEndPaintDefaultBehavior;
  prepareFastFields;
  clearSprites;
  FBackGround := tBitmap.Create;
end;

destructor TEngine2d.Destroy;
begin
  FObjectCreator.Free;
  clearSprites;
  FImage.free;
  FAnimationList.Free;
  FFormatters.Free;
  FFastFields.Free;
  FBackGround.free;

  inherited;
end;

procedure TEngine2d.Resize;
var
  i: Integer;
begin
  FCritical.Enter;
  // Форматирвание
  for i := 0 to FFormatters.Count - 1 do
    FFormatters[i].Format;
  FCritical.Leave;
end;

procedure TEngine2d.Repaint;
var
  i, l: integer;
  iA, lA: Integer; // Счетчики анимации и форматирования
  m: tMatrix;
  vAnimation: tAnimation;
begin

  // Анимация
  FCritical.Enter;
  lA := FAnimationList.Count - 1;
  for iA := lA downto 0 do
  begin
    if FAnimationList[iA].Animate = TAnimation.CAnimationEnd then
    begin
      vAnimation := FAnimationList[iA];
      FAnimationList.Delete(iA);
      vAnimation.Free;
    end;
  end;
  FCritical.Leave;

//  if FDebug then
//   FDebug := False;

  FCritical.Enter;
  if (lA > 0) or (FOptions.ToAnimateForever)  then
    with FImage do
    begin
      if Bitmap.Canvas.BeginScene() then
      try
        FInBeginPaintBehavior;
        FBackgroundBehavior;

        l := (FObjects.Count - 1);
        for i := 1 to l do
          if FObjects[FObjectOrder[i]].visible then
          begin
            m :=
              TMatrix.CreateTranslation(-FObjects[FObjectOrder[i]].x, -FObjects[FObjectOrder[i]].y) *
              TMatrix.CreateScaling(FObjects[FObjectOrder[i]].ScaleX, FObjects[FObjectOrder[i]].ScaleY) *
              TMatrix.CreateRotation(FObjects[FObjectOrder[i]].rotate * pi180) *
              TMatrix.CreateTranslation(FObjects[FObjectOrder[i]].x, FObjects[FObjectOrder[i]].y);
            Bitmap.Canvas.SetMatrix(m);

            FObjects[FObjectOrder[i]].Repaint;
            {$IFDEF DEBUG}
            if FOptions.ToDrawFigures then
               FObjects[FObjectOrder[i]].RepaintWithShapes;
            {$ENDIF}
          end;
      finally
        FInEndPaintBehavior;

        Bitmap.Canvas.EndScene();
        {$IFDEF POSIX}
          InvalidateRect(RectF(0, 0, Bitmap.Width , Bitmap.Height));
        {$ENDIF}
      end;
  end;

  FCritical.Leave;
end;

function TEngine2d.getObject(index: integer): tEngine2DObject;
begin
  FCritical.Enter;
  result := FObjects[index];
  FCritical.Leave;
end;

procedure TEngine2d.InBeginPaintDefaultBehavior;
begin

end;

procedure TEngine2d.InEndPaintDefaultBehavior;
begin
  //Exit;
  with FImage do
  begin
  // bitmap.Canvas.Blending:=true;
        bitmap.Canvas.SetMatrix(tMatrix.Identity);
        bitmap.Canvas.Fill.Color := TAlphaColorRec.Brown;
        Bitmap.Canvas.Font.Size := 12;
        Bitmap.Canvas.Font.Style := [TFontStyle.fsBold];
        Bitmap.Canvas.Font.Family := 'arial';
        {$IFDEF CONDITIONALEXPRESSIONS}
         {$IF CompilerVersion >= 19.0}
        bitmap.Canvas.FillText(
          RectF(15, 15, 165, 125),
          'FPS=' + floattostr(FEngineThread.fps),
          false, 1, [],
          TTextAlign.Leading
        );

        {  bitmap.Canvas.FillText(
          RectF(15, 85, 165, 125),
          'scale=' + floattostr(getScreenScale),
          false, 1, [],
          TTextAlign.Leading
        );  }

        {if length(self.fClicked) >= 1 then
        begin
          bitmap.Canvas.FillText(RectF(15, 45, 165, 145),
            'sel=' + inttostr(self.fClicked[0]), false, 1, [],
            TTextAlign.Leading);
        end;
        bitmap.Canvas.FillText(
          RectF(25, 65, 200, 200),
          floattostr(flX) + ' ' + floattostr(flY),
          false, 1, [],
          TTextAlign.Leading
        );                  }
        {$ENDIF}{$ENDIF}
        {$IFDEF VER260}
        bitmap.Canvas.FillText(
          RectF(15, 15, 165, 125),
          'FPS=' + floattostr(fEngineThread.fps),
          false, 1, [],
          TTextAlign.taLeading
        );

      {  if length(self.fClicked) >= 1 then
        begin
          bitmap.Canvas.FillText(RectF(15, 45, 165, 145),
            'sel=' + inttostr(self.fClicked[0]), false, 1, [],
            TTextAlign.taLeading);
        end;
        bitmap.Canvas.FillText(
          RectF(25, 65, 200, 200),
          floattostr(flX) + ' ' + floattostr(flY),
          false, 1, [],
          TTextAlign.taLeading
        );   }
        {$ENDIF}
  end;
end;

procedure TEngine2d.Init(AImage: tImage);
begin
  FImage := AImage;
  FWidth := Round(AImage.Width);
  FHeight := Round(AImage.Height);
  FImage.Bitmap.Width := Round(AImage.Width * getScreenScale);
  FImage.Bitmap.Height := ROund(AImage.Height * getScreenScale);

  SetLength(FObjectOrder, 4);

  FObjectCreator := TEngine2DManager.Create(
    Self,
    FImage,
    FCritical,
    FResources, FObjects, @FObjectOrder, FAnimationList, FFormatters, FFastFields, FEngineThread);
  prepareShadowObject;
end;

function TEngine2d.IsHor: Boolean;
begin
  Result := FWidth > FHeight;
end;

function TEngine2d.IsMouseDowned: Boolean;
begin
  REsult := FIsMouseDowned;
end;

procedure TEngine2d.LoadResources(const AFileName: String);
begin
  FResources.AddResFromLoadFileRes(AFileName);
end;

procedure TEngine2d.LoadSECSS(const AFileName: String);
begin
  FFormatters.LoadSECSS(AFileName);
end;

procedure TEngine2d.LoadSEJson(const AFileName: String);
var
  vJSON, vObj, vObjBody: TJSONObject;
  vObjects, vFigures: TJSONArray;
  vValue, vTmp: TJSONValue;
  vPos: TRect;
  vFile: TStringList;
  vImageFile, vObjName, vObjGroup: string;
  i, j: Integer;
  vS, vS1, vS2: string;
  vArr, vArr1, vArr2: TArray<string>;
begin
  vFile := TStringList.Create;
  vFile.LoadFromFile(AFileName);

  vJSON := TJSONObject.ParseJSONValue(vFile.Text) as TJsonObject;
  vImageFile := vJSON.GetValue('ImageFile').ToString;
  VObjects := vJSON.GetValue('Objects') as TJSONArray;

  for i := 0 to vObjects.Count - 1 do
  begin
    vObj := vObjects.Items[i] as TJSONObject;
    vObjName := vObj.GetValue('Name').ToString;
    vObjGroup:= vObj.GetValue('Group').ToString;
    vObjBody := vObj.GetValue('Body') as TJSONObject;
    if vObjBody <> nil then
      with vObjBody do
      begin
        vArr := (GetValue('Position').ToString).Split([';']);
        vArr1 := vArr[0].Split([',']);
        vArr2 := vArr[1].Split([',']);
        vPos := Rect(
                vArr1[0].ToInteger, vArr1[1].ToInteger,
                vArr2[0].ToInteger, vArr2[1].ToInteger);

        vFigures := GetValue('Figures') as TJSONArray;
        if vFigures <> nil then
          for j := 0 to vFigures.Count - 1 do
          begin

          end;
      end;
  end;

  vFile.Free;

end;

procedure TEngine2d.MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; x, y: single; const ACount: Integer = -1);
var
  i, l, vCount: integer;
begin
  FIsMouseDowned := True;

  FlX := x;// * getScreenScale;
  FlY := y; //* getScreenScale;
  l := FObjects.Count - 1;//length(fSprites) - 1;

  setLength(FClicked, 0);
  setLength(FMouseDowned, 0);

  for i := l downto 1 do
  begin
    if FObjects[FObjectOrder[i]].visible then
      if FObjects[FObjectOrder[i]].underTheMouse(FlX, FlY) then
      begin
        setLength(FMouseDowned, length(FMouseDowned) + 1);
        FMouseDowned[high(FMouseDowned)] := FObjectOrder[i];

      //ПЕРЕНЕСИ w и h в TEngine2DObject и сделай определение положения клика в спрайте

      end;
  end;

 // MouseUp and Clicks on Objects
  if ACount = -1 then
    vCount := Length(FMouseDowned)
  else
    vCount := Min(ACount, Length(FMouseDowned));

  for i := 0 to vCount - 1 do
    Sprites[FMouseDowned[i]].OnMouseDown(Sprites[FMouseDowned[i]], Button, Shift, x, y);
end;

procedure TEngine2d.MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; x, y: single; const ACount: Integer = -1; const AClickObjects: Boolean = True);
var
  i, l, vCount: integer;
begin
  FIsMouseDowned := False;

  FlX := x ;//* getScreenScale;
  FlY := y ;//* getScreenScale;
  l := FObjects.Count - 1;//length(fSprites) - 1;

  SetLength(FClicked, 0);
  SetLength(FMouseUpped, 0);

  for i := l downto 1 do
  begin
    if FObjects[FObjectOrder[i]].visible then
      if FObjects[FObjectOrder[i]].underTheMouse(FlX, FlY) then
      begin
        SetLength(FMouseUpped, length(FMouseUpped) + 1);
        FMouseUpped[high(FMouseUpped)] := FObjectOrder[i];
      end;
    end;

  FClicked := IntArrInIntArr(FMouseDowned, FMouseUpped);

  // MouseUp and Clicks on Objects
  if ACount = -1 then
    vCount := Length(FMouseUpped)
  else
    vCount := Min(ACount, Length(FMouseUpped));

  for i := 0 to vCount - 1 do
    Sprites[FMouseUpped[i]].OnMouseUp(Sprites[FMouseUpped[i]], Button, Shift, x, y);

  if AClickObjects then
    Click(ACount);
end;

procedure TEngine2d.prepareFastFields;
var
  vTmp: TFastField;
begin
  FFastFields := TFastFields.Create(IsHor);
//  fFastFields.Parent := Self;
  vTmp := TFastEngineWidth.Create(Self);
  FFastFields.Add('engine.width', vTmp);
  vTmp := TFastEngineHeight.Create(Self);
  FFastFields.Add('engine.height', vTmp);
end;

procedure TEngine2d.prepareShadowObject;
begin
  FShadowObject := tSprite.Create;
  FObjectCreator.Add(TSprite(FShadowObject), 'shadow');
end;

procedure TEngine2d.setBackGround(ABmp: tBitmap);
begin
  if width > height then
  begin
    FBackGround.Assign(ABmp);
    FBackGround.rotate(90);
  end
  else
    FBackGround.Assign(ABmp);
end;

procedure TEngine2d.SetBackgroundBehavior(const Value: TProcedure);
begin
  FBackgroundBehavior := Value;
end;

procedure TEngine2d.setHeight(AHeight: integer);
begin
  FImage.Bitmap.Height := Round(AHeight * getScreenScale + 0.4);
  FHeight := AHeight;
end;

procedure TEngine2d.setObject(index: integer; newSprite: tEngine2DObject);
begin
  FCritical.Enter;
  FObjects[index] := NewSprite;
  FCritical.Leave;
end;

procedure TEngine2d.setStatus(newStatus: byte);
begin
  FStatus := newStatus;
end;

procedure TEngine2d.setWidth(AWidth: integer);
begin
  FImage.Bitmap.Width := Round(AWidth * getScreenScale + 0.4);
  FWidth := AWidth;
end;

procedure TEngine2d.start;
begin
  status := CGameStarted;
end;

procedure TEngine2d.stop;
begin
  status := CGameStopped;
end;

end.

