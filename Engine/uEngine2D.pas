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
  FMX.Objects, Math, System.SyncObjs,
  {$IFDEF VER290} System.Math.Vectors, {$ENDIF}
  uClasses, uEngine2DThread, uEngine2DObject, uEngine2DUnclickableObject,
  uEngine2DSprite, uEngine2DText, uEngine2DClasses, uFormatterList, uSpriteList,
  uEngine2DResources, uEngine2DAnimation, uNamedList, uEngine2DAnimationList,
  uFastFields, uEasyDevice;

type

//  tSpriteArray = TNamedList<TEngine2DClickableObject>;//array of TEngine2DClickableObject;

  tEngine2d = class
  strict private
    fEngineThread: tEngineThread; // Поток в котором происходит отрисовка
    fOptions: TEngine2DOptions; // Настройки движка
    fObjects: TObjectsList; // Массив спрайтов для отрисовки
    fFastFields: tFastFields; // Содержит ссылки на TFastField, которые представляют собой найденные значения определенных спрайтов
    fSpriteOrder: array of Integer; // Массив порядка отрисовки. Нужен для уменьшения кол-ва вычислений, содержит номер спрайта
    fResources: TEngine2DResources;//tResourceArray; // Массив битмапов
    fFormatters: TFormatterList; // Массив Форматтеров спрайтов
    fAnimationList: TEngine2DAnimationList; // Массив анимаций
    fMouseDowned: TIntArray; // Массив спрайтов движка, которые находились под мышкой в момент нажатия
    fMouseUpped: TIntArray; // Массив спрайтов движка, которые находились под мышкой в момент отжатия
    fClicked: TIntArray; // Массив спрайтов движка, которые попали под мышь
    fStatus: byte; // Состояние движка 0-пауза, 1-работа
    flX, flY: single; // o_O Для масштабирования на смартфоны что-то
    FIsMouseDowned: Boolean; // Хранит состояние нажатости мыши
    fImage: tImage; // Имедж, в котором происходит отрисовка
    fBackGround: tBitmap; // Бэкграунд. Всегда рисуется в Repaint на весь fImage
    fCritical: TCriticalSection; // Критическая секция движка
    fWidth, fHeight: integer; // Размер поля имеджа и движка
    fDebug: Boolean; // Не очень нужно, но помогает отлаживать те места, когда непонятно когда появляется ошибка
    FBackgroundBehavior: TProcedure;
    FInBeginPaintBehavior: TProcedure;
    FInEndPaintBehavior: TProcedure;
    FAddedSprite: Integer; // Считает сколько спрайтов добавлено всего. Без учета удалений
    FDrawFigures: Boolean; // Требуется обычно для отладки. Рисует области фигур интерсектора

    // Механизм теневого объекты необычен. Но кроме всего прочего TEngine2DObject не имеет способов определения
    {FShadowSprite: tSprite; //
    FShadowText: TEngine2dText; }
    FShadowObject: tEngine2DObject;

    procedure prepareFastFields;
    procedure prepareShadowObject;
    procedure setStatus(newStatus: byte);
    procedure setObject(index: integer; newSprite: tEngine2DObject);
    function getObject(index: integer): tEngine2DObject;
    function getSpriteCount: integer; // Длина массив fSprites
    procedure SetWidth(AWidth: integer); // Установка размера поля отрисовки движка
    procedure SetHeight(AHeight: integer); // Установка размера поля отрисовки движка
    procedure setBackGround(ABmp: tBitmap);

    procedure BackgroundDefaultBehavior;
    procedure InBeginPaintDefaultBehavior;
    procedure InEndPaintDefaultBehavior;

    function GetIfHor: Boolean;
    function GetHeight: integer;
    function GetWidth: integer;
    procedure SetBackgroundBehavior(const Value: TProcedure);
  public
    // Ключевые свойства движка
    property EngineThread: TEngineThread read fEngineThread;
    property Image: TImage read FImage write FImage;
    property BackgroundBehavior: TProcedure read FBackgroundBehavior write SetBackgroundBehavior;
    property InBeginPaintBehavior: TProcedure read FInBeginPaintBehavior write FInBeginPaintBehavior;
    property InEndPaintBehavior: TProcedure read FInBeginPaintBehavior write FInBeginPaintBehavior;
    // Ключевые списки движка
    property Resources: TEngine2DResources read FResources;
    property AnimationList: TEngine2DAnimationList read fAnimationList;
    property FormatterList: TFormatterList read fFormatters;
    property SpriteList: TObjectsList read fObjects;
    property FastFields: tFastFields read FFastFields; // Быстрый вызов для экспрешенсов

    property IsMouseDowned: Boolean read FIsMouseDowned;
    property Status: byte read fStatus write setStatus;
    property Width: integer read {GetWidth} fWidth write setWidth;
    property Height: integer read {GetHeight}fHeight write setHeight;

    property Clicked: tIntArray read fClicked;
    property Downed: TIntArray read fMouseDowned;
    property Upped: TIntArray read fMouseUpped;
    property Critical: TCriticalSection read FCritical;
    property DrawFigures: Boolean read FDrawFigures write FDrawFigures;

    property SpriteCount: integer read getSpriteCount;
    property Sprites[index: integer]: tEngine2DObject read getObject write setObject;

    property Background: tBitmap read fBackGround write setBackGround;

    property IfHor: Boolean read GetIfHor; // Сообщает True, если Width > Height

    procedure SpriteToBack(const n: integer); // Передвигает в массиве отрисовки спрайт
    procedure SpriteToFront(const n: integer);// Передвигает в массиве отрисовки спрайт

    procedure Resize;
//    procedure Clear; // Удаляет все спрайты и после этого удаляет все ресурсы

    procedure MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; x, y: single); virtual;
    procedure MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; x, y: single); virtual;

    procedure DeleteObject(const AObject: tEngine2DObject); overload; // Убирает спрайт из отрисовки
    procedure AddObject(const AObject: tEngine2DObject; const AName: String = ''); // Добавляет спрайт на отрисовку

    procedure AssignShadowObject(ASpr: tEngine2DObject); // Ассигнет спрайт в ShadowObject
    property ShadowObject: tEngine2DObject read FShadowObject;  // Указатель на Теневой объект.

    procedure ClearSprites; // Очищает массив спрайтов, т.е. является подготовкой к полной перерисовке
//    procedure clearResources; // Очищает массив ресурсов, т.е. является подготовкой к завершению
    procedure ClearTemp; // Очищает массивы выбора и т.д. короче делает кучу полезных вещей.

    procedure Init(AImage: tImage); // Инициализация движка, задаёт рисунок на форме, на которому присваиватся fImage
    procedure Repaint; virtual;

    // Прячем или показывает группы
    procedure ShowGroup(const AGroup: String);
    procedure HideGroup(const AGroup: String);
    procedure SendToFrontGroup(const AGroup: String); // Ставит группу на передний план
    procedure BringToBackGroup(const AGroup: String); // Отодвигает группу на задний план

    procedure Start; virtual; // Включает движок
    procedure Stop; virtual;// Выключает движок

    constructor Create; virtual;
    destructor Destroy; override;

    const
      CGameStarted = 1;
      CGameStopped = 255;
  end;

const
  pi180 = 0.0174532925; // (1/180)*pi для уменьшение количества пересчетов

implementation

uses
  System.RegularExpressions;

{ tEngine2d }

{procedure tEngine2d.addObject(const AObject: tEngine2DObject);
begin
  Inc(FAddedSprite);

end;}

procedure tEngine2d.addObject(const AObject: tEngine2DObject; const AName: String);
var
  l: integer;
  vName: String;
begin
  Inc(FAddedSprite);
  if AName = '' then
    vName := 'genname'+IntToStr(FAddedSprite)+'x'+IntToStr(Random(65536))
  else
    vName := AName;

  if fObjects.IsHere(AObject) then
    raise Exception.Create('You are trying to add Object to Engine that already Exist')
  else
  begin
    fCritical.Enter;
    l := spriteCount;
    fObjects.Add(vName, AObject);
    setLength(fSpriteOrder, l + 1);
    fObjects[l].Image := fImage;
    fSpriteOrder[l] := l;
    fCritical.Leave;
  end;
end;

procedure tEngine2d.AssignShadowObject(ASpr: tEngine2DObject);
begin

  //  В данном контексте следует различть наследников TEngine2DObject, т.к. может попасться текст
  FShadowObject.Position := ASpr.Position;
{  FShadowObject.ScaleX := ASpr.ScaleX;
  FShadowObject.ScaleY := ASpr.ScaleY;  }
  tSprite(FShadowObject).Resources := tSprite(ASpr).Resources;


end;

procedure tEngine2d.BackGroundDefaultBehavior;
begin
  with Self.Image do
    Bitmap.Canvas.DrawBitmap(
      fBackGround,
      RectF(0, 0, fBackGround.width, fBackGround.height),
      RectF(0, 0, bitmap.width, bitmap.height),
      1,
      true);
end;

procedure tEngine2d.BringToBackGroup(const AGroup: String);
var
  i, iObject, iG: Integer;
  vReg: TRegEx;
  vStrs: TArray<string>;
  vN: Integer;
begin
  vReg := TRegEx.Create(',');
  vStrs := vReg.Split(AGroup);
  vN := fObjects.Count - 1;
  for iG := 0 to Length(vStrs) - 1 do
  begin
    i := vN;
    iObject := vN;
    vStrs[iG] := Trim(vStrs[iG]);
    while iObject > 1 do
    begin
      if fObjects[fSpriteOrder[i]].Group = vStrs[iG] then
      begin
        fObjects[fSpriteOrder[i]].BringToBack;
        Inc(i);
      end;
      Dec(i);
      Dec(iObject);
    end;
  end;
end;

procedure tEngine2d.clearSprites;
var
  i, l: integer;
begin
  l := spriteCount - 1;

  for i := 0 to l do
  begin
    fObjects[i].free;
  end;

//  fSprites.Count := 0;
//  setLength(fSprites, 0);
  setLength(fSpriteOrder, 0);
end;

procedure tEngine2d.clearTemp;
begin
  setLength(self.fClicked, 0);
end;

constructor tEngine2d.Create; // (createSuspended: boolean);
begin
  fEngineThread := tEngineThread.Create;
  fResources := TEngine2DResources.Create;
  fResources.Parent := Self;
  fAnimationList := TEngine2DAnimationList.Create;
  fAnimationList.Parent := Self;
  fFormatters := TFormatterList.Create;
  fFormatters.Parent := Self;
  fObjects := TObjectsList.Create;
  fObjects.Parent := Self;
  fOptions.ToAnimateForever := True;
  fOptions.ToClickOnlyTop := False;
  fCritical := TCriticalSection.Create;
  FBackgroundBehavior := BackgroundDefaultBehavior;
  FInBeginPaintBehavior := InBeginPaintDefaultBehavior;
  FInEndPaintBehavior := InEndPaintDefaultBehavior;
  FAddedSprite := 0;
  fDebug := False;
  prepareFastFields;
  clearSprites;
  prepareShadowObject;

  fBackGround := tBitmap.Create;
end;

procedure tEngine2d.deleteObject(const AObject: tEngine2DObject);
var
  i, vN, vNum, vPos: integer;
begin
  fCritical.Enter;
  vNum := fObjects.IndexOfItem(AObject, FromEnd);
  if vNum > -1 then
  begin
    vN := fObjects.Count - 1;
    fAnimationList.ClearForSubject(AObject);
    fFormatters.ClearForSubject(AObject);
    fFastFields.ClearForSubject(AObject);
    fObjects.Delete(vNum{AObject});

   // AObject.Free;

    vPos := vN + 1;
    // Находим позицию спрайта
    for i := vN downto 0 do
      if fSpriteOrder[i] = vNum then
      begin
        vPos := i;
        Break;
      end;

    // От этой позиции сдвигаем порядок отрисовки
    vN := vN - 1;
    for i := vPos to vN do
      fSpriteOrder[i] := fSpriteOrder[i+1];

    // Все индексы спрайтов, которые больше vNum надо уменьшить на 1
    for i := 0 to vN do
      if fSpriteOrder[i] >= vNum then
        fSpriteOrder[i] := fSpriteOrder[i] - 1;

    // Уменьшаем длину массива
    SetLength(fSpriteOrder, vN + 1);
  end;
  fCritical.Leave;
  fDebug := True;
end;

destructor tEngine2d.Destroy;
begin
  inherited;
  clearSprites;
//  clearResources;
  fImage.free;
  fAnimationList.Free;
  fFormatters.Free;
  fFastFields.Free;
  fBackGround.free;
end;

procedure tEngine2d.Resize;
var
  vSize: tPointF;
  i: Integer;
begin
  //vSize := getDisplaySizeInPx;

  //fImage.Width := v
 { fImage.Width := vSize.X;
  fImage.Height := vSize.Y;
  fImage.Bitmap.Width := Round(vSize.X);//Round(vSize.X + 0.4);
  fImage.Bitmap.Height := Round(vSize.Y);//Round(vSize.Y + 0.4);
  fWidth := fImage.Bitmap.Width;
  fHeight := fImage.Bitmap.Height;}
  // Форматирвание
  for i := 0 to fFormatters.Count - 1 do
    fFormatters[i].Format;// then
end;

function tEngine2d.GetHeight: integer;
begin
  Result := fHeight;//Round(Self.fImage.Bitmap.Height);
end;

function tEngine2d.GetIfHor: Boolean;
begin
  Result := fWidth > fHeight;
end;

procedure tEngine2d.Repaint;
var
  i, l: integer;
  iA, lA: Integer; // Счетчики анимации и форматирования
  m: tMatrix;
  vAnimation: tAnimation;
begin

  // Анимация
  fCritical.Enter;
  lA := fAnimationList.Count - 1;
  for iA := lA downto 0 do
  begin
    if fAnimationList[iA].Animate = TAnimation.CAnimationEnd then
    begin
      vAnimation := fAnimationList[iA];
      fAnimationList.Delete(iA);
      vAnimation.Free;
    end;
  end;
  fCritical.Leave;

  if fDebug then
   fDebug := False;

  fCritical.Enter;
  if (lA > 0) or (FOptions.ToAnimateForever)  then
    with fImage do
    begin
      if Bitmap.Canvas.BeginScene() then
      try
        FInBeginPaintBehavior;
        FBackgroundBehavior;

        l := (fObjects.Count - 1);
        for i := 1 to l do
        {  if fSpriteOrder[i] <= l then
            if fObjects[fSpriteOrder[i]] <> Nil then   }
          
          if fObjects[fSpriteOrder[i]].visible then
          begin
            m :=
              TMatrix.CreateTranslation(-fObjects[fSpriteOrder[i]].x, -fObjects[fSpriteOrder[i]].y) *
              TMatrix.CreateScaling(fObjects[fSpriteOrder[i]].ScaleX, fObjects[fSpriteOrder[i]].ScaleY) *
              TMatrix.CreateRotation(fObjects[fSpriteOrder[i]].rotate * pi180) *
              TMatrix.CreateTranslation(fObjects[fSpriteOrder[i]].x, fObjects[fSpriteOrder[i]].y);
            Bitmap.Canvas.SetMatrix(m);

            fObjects[fSpriteOrder[i]].Repaint;
          end;
      finally
        FInEndPaintBehavior;

        Bitmap.Canvas.EndScene();
        {$IFDEF POSIX}
          InvalidateRect(RectF(0, 0, Bitmap.Width , Bitmap.Height));
        {$ENDIF}
      end;
  end;

  fCritical.Leave;
end;

procedure tEngine2d.SendToFrontGroup(const AGroup: String);
var
  i, iObject, iG: Integer;
  vReg: TRegEx;
  vStrs: TArray<string>;
  vN: Integer;
begin
  vReg := TRegEx.Create(',');
  vStrs := vReg.Split(AGroup);
  vN := fObjects.Count - 1;
  for iG := 0 to Length(vStrs) - 1 do
  begin
    i := 1;
    iObject := 1;
    vStrs[iG] := Trim(vStrs[iG]);
    while iObject < vN do
    begin
      if fObjects[fSpriteOrder[i]].Group = vStrs[iG] then
      begin
        fObjects[fSpriteOrder[i]].SendToFront;
        Dec(i);
      end;
      Inc(i);
      Inc(iObject);
    end;
  end;
end;

{function tEngine2d.ResToSF(const AId: Integer): TSpriteFrame;
var
  vTmp: TSpriteFrame;
begin
  vTmp.num := AId;
  vTmp.w := fResources[AId].bmp.Width;
  vTmp.h := fResources[AId].bmp.Height;

  Result := vTmp;
end;}

{function tEngine2d.getBitmap(index: integer): tBitmap;
var
  temp: tBitmap;
begin
  temp := tBitmap.Create;
  temp.Assign(fResources[index].bmp);
  result := temp;
end; }

function tEngine2d.getObject(index: integer): tEngine2DObject;
begin
  fCritical.Enter;
  result := fObjects[index];
  fCritical.Leave;
end;

function tEngine2d.getSpriteCount: integer;
begin
  result := fObjects.Count;//length(fSprites)
end;

function tEngine2d.GetWidth: integer;
begin
  Result := fWidth;//Round(Self.fImage.Bitmap.Width);
end;

procedure tEngine2d.HideGroup(const AGroup: String);
var
  i, iG: Integer;
  vReg: TRegEx;
  vStrs: TArray<string>;
begin
  vReg := TRegEx.Create(',');
  vStrs := vReg.Split(AGroup);
  for iG := 0 to Length(vStrs) - 1 do
  begin
    vStrs[iG] := Trim(vStrs[iG]);
    for i := 0 to fObjects.Count - 1 do
      if fObjects[i].group = vStrs[iG] then
        fObjects[i].visible := False;
  end;
end;

procedure tEngine2d.InBeginPaintDefaultBehavior;
begin

end;

procedure tEngine2d.InEndPaintDefaultBehavior;
begin
  Exit;
  with FImage do
  begin
  // bitmap.Canvas.Blending:=true;
        bitmap.Canvas.SetMatrix(tMatrix.Identity);
        bitmap.Canvas.Fill.Color := TAlphaColorRec.Brown;
        Bitmap.Canvas.Font.Size := 12;
        Bitmap.Canvas.Font.Style := [TFontStyle.fsBold];
        Bitmap.Canvas.Font.Family := 'arial';
        {$IFDEF VER290}
        bitmap.Canvas.FillText(
          RectF(15, 15, 165, 125),
          'FPS=' + floattostr(fEngineThread.fps),
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
        {$ENDIF}
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

procedure tEngine2d.Init(AImage: tImage);
var
  vSize: TPointF;
begin
  fImage := AImage;
  fWidth := Round(AImage.Width);
  fHeight := Round(AImage.Height);
  fImage.Bitmap.Width := Round(AImage.Width * getScreenScale);
  fImage.Bitmap.Height := ROund(AImage.Height * getScreenScale);
end;

procedure tEngine2d.MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; x, y: single);
var
  i, l: integer;
begin
  FIsMouseDowned := True;

  flX := x;// * getScreenScale;
  flY := y; //* getScreenScale;
  l := fObjects.Count - 1;//length(fSprites) - 1;

  setLength(fClicked, 0);
  setLength(fMouseDowned, 0);

  for i := l downto 1 do
  begin
    if fObjects[fSpriteOrder[i]].visible then
      if fObjects[fSpriteOrder[i]].underTheMouse(flX, flY) then
      begin
        setLength(fMouseDowned, length(fMouseDowned) + 1);
        fMouseDowned[high(fMouseDowned)] := fSpriteOrder[i];

      //ПЕРЕНЕСИ w и h в TEngine2DObject и сделай определение положения клика в спрайте

      end;
  end;
end;

procedure tEngine2d.MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; x, y: single);
var
  i, l: integer;
begin
  FIsMouseDowned := False;

  flX := x ;//* getScreenScale;
  flY := y ;//* getScreenScale;
  l := fObjects.Count - 1;//length(fSprites) - 1;

  SetLength(fClicked, 0);
  SetLength(fMouseUpped, 0);

  for i := l downto 1 do
  begin
    if fObjects[fSpriteOrder[i]].visible then
      if fObjects[fSpriteOrder[i]].underTheMouse(flX, flY) then
      begin
        SetLength(fMouseUpped, length(fMouseUpped) + 1);
        fMouseUpped[high(fMouseUpped)] := fSpriteOrder[i];
      end;
    end;

  fClicked := IntArrInIntArr(fMouseDowned, fMouseUpped);
end;

procedure tEngine2d.prepareFastFields;
var
  vTmp: TFastField;
begin
  fFastFields := TFastFields.Create{(Self)};
  fFastFields.Parent := Self;
  vTmp := TFastEngineWidth.Create(Self);
  fFastFields.Add('engine.width', vTmp);
  vTmp := TFastEngineHeight.Create(Self);
  fFastFields.Add('engine.height', vTmp);
end;

procedure tEngine2d.prepareShadowObject;
begin
  FShadowObject := tSprite.Create(Self);
  FShadowObject.Parent := Self;
  Self.AddObject(FShadowObject, 'shadow');
end;

procedure tEngine2d.setBackGround(ABmp: tBitmap);
begin
  if width > height then
  begin
    fBackGround.Assign(ABmp);
    fBackGround.rotate(90);
  end
  else
    fBackGround.Assign(ABmp);
end;

procedure tEngine2d.SetBackgroundBehavior(const Value: TProcedure);
begin
  FBackgroundBehavior := Value;
end;

{procedure tEngine2d.setBitmap(index: integer; newBitmap: tBitmap);
begin
  fResources[index].bmp.Assign(newBitmap);
  fResources[index].rect := RectF(0, 0, newBitmap.width, newBitmap.height);
end; }

procedure tEngine2d.setHeight(AHeight: integer);
begin
  fImage.Bitmap.Height := Round(AHeight * getScreenScale + 0.4);
  fHeight := AHeight;
end;

procedure tEngine2d.setObject(index: integer; newSprite: tEngine2DObject);
begin
  fCritical.Enter;
  fObjects[index] := NewSprite;
  fCritical.Leave;
end;

procedure tEngine2d.setStatus(newStatus: byte);
begin
  fStatus := newStatus;
end;

procedure tEngine2d.setWidth(AWidth: integer);
begin
  fImage.Bitmap.Width := Round(AWidth * getScreenScale + 0.4);
  fWidth := AWidth;
end;

procedure tEngine2d.showGroup(const AGroup: String);
var
  i, iG: Integer;
  vReg: TRegEx;
  vStrs: TArray<string>;
begin
  vReg := TRegEx.Create(',');
  vStrs := vReg.Split(AGroup);
  for iG := 0 to Length(vStrs) - 1 do
  begin
    vStrs[iG] := Trim(vStrs[iG]);
    for i := 0 to fObjects.Count - 1 do
      if fObjects[i].group = vStrs[iG] then
        fObjects[i].visible := True;
  end;
end;

procedure tEngine2d.spriteToBack(const n: integer);
var
  i, l, oldOrder: integer;
begin
  l := length(fSpriteOrder);

  oldOrder := fSpriteOrder[n]; // Узнаём порядок отрисовки спрайта номер n

  for i := 1 to l - 1 do
    if fSpriteOrder[i] < oldOrder then
      fSpriteOrder[i] := fSpriteOrder[i] + 1;

  fSpriteOrder[n] := 1;
end;

procedure tEngine2d.spriteToFront(const n: integer);
var
  i, l, oldOrder: integer;
begin
  l := length(fSpriteOrder);
  oldOrder := l - 1;

  for i := 1 to l - 1 do
    if fSpriteOrder[i] = n then
    begin
      oldOrder := i;
      break;
    end;

  for i := oldOrder to l - 2 do
  begin
    fSpriteOrder[i] := fSpriteOrder[i + 1];
  end;

  fSpriteOrder[l - 1] := n;
end;

procedure tEngine2d.start;
begin
  status := CGameStarted;
end;

procedure tEngine2d.stop;
begin
  status := CGameStopped;
end;

end.

