unit uEngine2D;

{******************************************************************************
Shadow Object Engine (SO Engine)
By Dmitriy Sorokin.

Some comments in English, some in Russian. And it depends on mood :-) Sorry!)
You can write me by email: dimsa@inbox.ru
You can write me on skype: dimsa87

*******************************************************************************}

interface

uses
  System.SysUtils, FMX.Controls,
  FMX.Forms, FMX.Dialogs, FMX.Platform, FMX.Objects, Math,  {$I 'Utils\DelphiCompatability.inc'}
  uSoTypes, uClasses, uEngine2DThread, uEngine2DObject, uEngine2DSprite, uEngine2DText, uEngine2DClasses,
  uEngine2DManager, uEngine2DStatus, uEasyDevice, uEngine2DModel, uEngine2DAnimation, uFastFields,
  uEngine2DOptions;

type
  TEngine2d = class
  strict private
    FEngineThread: TEngineThread; // Thread that paint all sprites (But there are possibility to use not one thread)  // Поток в котором происходит отрисовка
    FCritical: TCriticalSection; // The critical section for multithread operation, to protect model on changind in paint time // Критическая секция движка
    FModel: TEngine2DModel; // All main lists are in It.
    FOptions: TEngine2DOptions; // All Engine options. If you add some feature to manage engine, it shoulb be here// Настройки движка
    FObjectCreator: TEngine2DManager; // This object work with Model items. It's controller/
    FMouseDowned: TIntArray; // Lists of sprites that were under the mouse on MouseDown  // Массив спрайтов движка, которые находились под мышкой в момент нажатия
    FMouseUpped: TIntArray; // Lists of sprites that were under the mouse on MouseUp // Массив спрайтов движка, которые находились под мышкой в момент отжатия
    FClicked: TIntArray; // Lists of sprites that were under the mouse on MouseDown and mouse on MouseUp // Массив спрайтов движка, которые попали под мышь
    FStatus: TEngine2DStatus; // All Engine status you can get from herem like width-height,speed and etc.
    FIsMouseDowned: Boolean; // True if Mouse is Downed  // Хранит состояние нажатости мыши
    FImage: TImage; // It's the Image the Engine Paint in. // Имедж, в котором происходит отрисовка
    FBackGround: TBitmap; // Background of Engine that paints on every tick. Not sure if it should be here // Бэкграунд. Всегда рисуется в Repaint на весь fImage
    FWidth, FHeight: integer; // Размер поля имеджа и движка
//    FDebug: Boolean; // There are some troubles to debug multithread app, so it for it // Не очень нужно, но помогает отлаживать те места, когда непонятно когда появляется ошибка
    FBackgroundBehavior: TProcedure; // Procedure to Paint Background. It can be default or Parallax(like in Asteroids example) or any type you want
    FInBeginPaintBehavior: TProcedure; // Method is called before Paint
    FInEndPaintBehavior: TProcedure; // Method is called after Paint

    FShadowObject: tEngine2DObject; // It's one of the main feature SO Engine (Shadow Object Engine). You can use formatters to change objects position. But in some situation you need you know object position after the formatter would have been applied. So this object can simulate this. // Механизм теневого объекты необычен. Но кроме всего прочего TEngine2DObject не имеет способов определения
    procedure PrepareFastFields;
    procedure PrepareShadowObject;
    procedure SetWidth(AWidth: integer); // Установка размера поля отрисовки движка
    procedure SetHeight(AHeight: integer); // Установка размера поля отрисовки движка
    procedure setBackGround(ABmp: TBitmap);
    procedure BackgroundDefaultBehavior;
    procedure InBeginPaintDefaultBehavior;
    procedure InEndPaintDefaultBehavior;
    procedure SetBackgroundBehavior(const Value: TProcedure);
    function IsHor: Boolean; // Return True, if Engine.Width > Engine.Height
  protected
    property EngineThread: TEngineThread read FEngineThread;
  public
    // Main properties of Engine. Ключевые свойства движка
    property Image: TImage read FImage write FImage;
    property BackgroundBehavior: TProcedure read FBackgroundBehavior write SetBackgroundBehavior;
    property InBeginPaintBehavior: TProcedure read FInBeginPaintBehavior write FInBeginPaintBehavior;
    property InEndPaintBehavior: TProcedure read FInBeginPaintBehavior write FInBeginPaintBehavior;
    property Critical: TCriticalSection read FCritical;

    property Width: integer read FWidth write setWidth;
    property Height: integer read FHeight write setHeight;

    property Background: TBitmap read FBackGround write setBackGround;
    property Options: TEngine2dOptions read FOptions write FOptions;
    procedure Resize;
    procedure MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; x, y: single; const ACount: Integer = -1); virtual; // ACount is quantity of sorted object that will be MouseDowned -1 is all.
    procedure MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; x, y: single; const ACount: Integer = -1; const AClickObjects: Boolean = True); virtual; // ACount is quantity of sorted object that will be MouseUpped -1 is all.
    procedure Click(const ACount: Integer = -1); virtual; // It must be Called after MouseUp if in MouseUp was AClickObjects = False;

    procedure AssignShadowObject(ASpr: tEngine2DObject); // Assign object properties to Shadow Object// Ассигнет спрайт в ShadowObject
    property ShadowObject: tEngine2DObject read FShadowObject; // Указатель на Теневой объект.

    procedure LoadResources(const AFileName: string); // Loads resources(animations frames) for sprites. Shoul be in Manager
    procedure LoadSECSS(const AFileName: string); // Loads SECSS filee to use it Engine. It should Be in Manager
    procedure LoadSEJSON(const AFileName: string);  experimental; // Working on it! It's loading of object that created by Sprite Shape Builder It should be in Manager
    procedure Init(AImage: TImage); // Initialization of SO Engine // Инициализация движка, задаёт рисунок на форме, на которому присваиватся fImage
    procedure WorkProcedure; virtual; // The main Paint procedure.
    procedure Start; virtual; // Включает движок
    procedure Stop; virtual;// Выключает движок

    constructor Create; virtual;
    destructor Destroy; override;

    // You should use Manager to Work with Engine
    property Manager: TEngine2DManager read FObjectCreator; // It helps to create object faster // Позволяет быстрее и проще создавать объекты
    property Status: TEngine2DStatus read FStatus;
    const
      CGameStarted = 1;
      CGameStopped = 255;
  end;

const
  pi180 = 0.0174532925; // (1/180)*pi  для уменьшение количества пересчетов

implementation

uses
  System.RegularExpressions, System.JSON;

{ tEngine2d }

procedure TEngine2d.AssignShadowObject(ASpr: tEngine2DObject);
begin
  // В данном контексте следует различть наследников TEngine2DObject, т.к. может попасться текст
  FShadowObject.Position := ASpr.Position;
  tSprite(FShadowObject).Resources := tSprite(ASpr).Resources;
end;

procedure TEngine2d.BackGroundDefaultBehavior;
begin
  with Self.Image do
    Bitmap.Canvas.DrawBitmap(FBackGround, TRectF.Create(0, 0, FBackGround.width, FBackGround.height), TRectF.Create(0, 0, bitmap.width, bitmap.height), 1, true);
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
    FModel.Objects[FClicked[i]].OnClick(FModel.Objects[FClicked[i]]);
end;

constructor TEngine2d.Create;
begin
  FOptions := tEngine2DOptions.Create;
  FCritical := TCriticalSection.Create;
  FEngineThread := tEngineThread.Create;
  FEngineThread.WorkProcedure := WorkProcedure;

  FStatus := TEngine2DStatus.Create(FEngineThread, @FWidth, @FHeight, @FIsMouseDowned, @FMouseDowned, @FMouseUpped, @FClicked);
  FModel := TEngine2DModel.Create(FCritical, IsHor);

  FOptions.Up([EAnimateForever, EUseCollider]);
  FOptions.Down([EClickOnlyTop]);

  FBackgroundBehavior := BackgroundDefaultBehavior;
  FInBeginPaintBehavior := InBeginPaintDefaultBehavior;
  FInEndPaintBehavior := InEndPaintDefaultBehavior;
  PrepareFastFields;
  FModel.ClearSprites;

  FBackGround := TBitmap.Create;
end;

destructor TEngine2d.Destroy;
begin
  FObjectCreator.Free;
  FImage.Free;
  FModel.Free;
  FBackGround.Free;
  FOptions.Free;

  inherited;
end;

procedure TEngine2d.Resize;
var
  i: Integer;
begin
  FCritical.Enter;
  // Appliyng of Formatters
  for i := 0 to FModel.FormatterList.Count - 1 do
    FModel.FormatterList[i].Format;
  FCritical.Leave;
end;

procedure TEngine2d.WorkProcedure;
var
  i, l: integer;
  iA, lA: Integer; // Animations and Formatters counters Счетчики анимации и форматирования
  m: tMatrix;
  vAnimation: tAnimation;
  {$IFDEF DEBUG}
  vSpr: TEngine2DObject;
  {$ENDIF}
begin
  if FOptions.ToUseCollider then
  begin
    FCritical.Enter;
    //FIntersector.DoWork;
    FCritical.Leave;
  end;

  // Анимация
  FCritical.Enter;
  with FModel do
  begin
    lA := AnimationList.Count - 1;
    for iA := lA downto 0 do
    begin
      if AnimationList[iA].Animate = TAnimation.CAnimationEnd then
      begin
        vAnimation := AnimationList[iA];
        AnimationList.Delete(iA);
        vAnimation.Free;
      end;
    end;
  end;
  FCritical.Leave;

  FCritical.Enter;
  if (lA > 0) or (FOptions.ToAnimateForever) then
    with FImage do
      with FModel do
      begin
        if Bitmap.Canvas.BeginScene() then
        try

          FInBeginPaintBehavior;
          FBackgroundBehavior;

          l := (ObjectList.Count - 1);
          for i := 1 to l do
            if ObjectList[ObjectOrder[i]].visible then
            begin
              m := TMatrix.CreateTranslation(
                -ObjectList[ObjectOrder[i]].x,
                -ObjectList[ObjectOrder[i]].y) * TMatrix.CreateScaling(ObjectList[ObjectOrder[i]].ScaleX,
                ObjectList[ObjectOrder[i]].ScaleY) * TMatrix.CreateRotation(ObjectList[ObjectOrder[i]].rotate * pi180) * TMatrix.CreateTranslation(ObjectList[ObjectOrder[i]].x,
                ObjectList[ObjectOrder[i]].y);


              Bitmap.Canvas.SetMatrix(m);

             {$IFDEF DEBUG}
             if FOptions.ToDrawFigures then
               vSpr := ObjectList[FModel.ObjectOrder[i]];
             {$ENDIF}

             ObjectList[FModel.ObjectOrder[i]].Repaint;

             {$IFDEF DEBUG}
             if FOptions.ToDrawFigures then
               vSpr.RepaintWithShapes;
             {$ENDIF}
            end;
        finally
          FInEndPaintBehavior;

          Bitmap.Canvas.EndScene();

          {$IFDEF POSIX}
          InvalidateRect(TRectF.Create(0, 0, Bitmap.Width, Bitmap.Height));
          {$ENDIF}
        end;
      end;

  FCritical.Leave;
end;

procedure TEngine2d.InBeginPaintDefaultBehavior;
begin

end;

procedure TEngine2d.InEndPaintDefaultBehavior;
begin
  {$IFDEF RELEASE}
  Exit;
  {$ENDIF}
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
    bitmap.Canvas.FillText(TRectF.Create(15, 15, 165, 125), 'FPS=' + floattostr(FEngineThread.fps), false, 1, [], TTextAlign.Leading);

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
    bitmap.Canvas.FillText(RectF(15, 15, 165, 125), 'FPS=' + floattostr(fEngineThread.fps), false, 1, [], TTextAlign.taLeading);

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

procedure TEngine2d.Init(AImage: TImage);
begin
  FImage := AImage;
  FWidth := Round(AImage.Width);
  FHeight := Round(AImage.Height);
  FImage.Bitmap.Width := Round(AImage.Width * getScreenScale);
  FImage.Bitmap.Height := ROund(AImage.Height * getScreenScale);

  FObjectCreator := TEngine2DManager.Create(Self.Status, FImage, FCritical, FModel, FEngineThread, Resize);
  PrepareShadowObject;
end;

function TEngine2d.IsHor: Boolean;
begin
  Result := FWidth > FHeight;
end;

procedure TEngine2d.LoadResources(const AFileName: string);
begin
  FModel.Resources.AddResFromLoadFileRes(AFileName);
end;

procedure TEngine2d.LoadSECSS(const AFileName: string);
begin
  FModel.FormatterList.LoadSECSS(AFileName);
end;

procedure TEngine2d.LoadSEJson(const AFileName: string);
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
    vObjGroup := vObj.GetValue('Group').ToString;
    vObjBody := vObj.GetValue('Body') as TJSONObject;
    if vObjBody <> nil then
      with vObjBody do
      begin
        vArr := (GetValue('Position').ToString).Split([';']);
        vArr1 := vArr[0].Split([',']);
        vArr2 := vArr[1].Split([',']);
        vPos := TRect.Create(vArr1[0].ToInteger, vArr1[1].ToInteger, vArr2[0].ToInteger, vArr2[1].ToInteger);

        vFigures := GetValue('Figures') as TJSONArray;
        if vFigures <> nil then
          for j := 0 to vFigures.Count - 1 do
          begin

          end;
      end;
  end;

  vFile.Free;

end;

procedure TEngine2d.MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; x, y: single; const ACount: Integer = -1);
var
  i, l, vCount: integer;
begin
  FIsMouseDowned := True;

  l := FModel.ObjectList.Count - 1;

  setLength(FClicked, 0);
  setLength(FMouseDowned, 0);

  for i := l downto 1 do
  begin
    if FModel.ObjectList[FModel.ObjectOrder[i]].visible then
      if FModel.ObjectList[FModel.ObjectOrder[i]].underTheMouse(x, y) then
      begin
        setLength(FMouseDowned, length(FMouseDowned) + 1);
        FMouseDowned[high(FMouseDowned)] := FModel.ObjectOrder[i];

      //ПЕРЕНЕСИ w и h в TEngine2DObject и сделай определение положения клика в спрайте

      end;
  end;

 // MouseUp and Clicks on Objects
  if ACount = -1 then
    vCount := Length(FMouseDowned)
  else
    vCount := Min(ACount, Length(FMouseDowned));

  for i := 0 to vCount - 1 do
    FModel.Objects[FMouseDowned[i]].OnMouseDown(FModel.Objects[FMouseDowned[i]], Button, Shift, x, y);
end;

procedure TEngine2d.MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; x, y: single; const ACount: Integer = -1; const AClickObjects: Boolean = True);
var
  i, l, vCount: integer;
begin
  FIsMouseDowned := False;

  l := FModel.ObjectList.Count - 1; //length(fSprites) - 1;

  SetLength(FClicked, 0);
  SetLength(FMouseUpped, 0);

  for i := l downto 1 do
  begin
    if FModel.ObjectList[FModel.ObjectOrder[i]].visible then
      if FModel.ObjectList[FModel.ObjectOrder[i]].underTheMouse(x, y) then
      begin
        SetLength(FMouseUpped, length(FMouseUpped) + 1);
        FMouseUpped[high(FMouseUpped)] := FModel.ObjectOrder[i];
      end;
  end;

  FClicked := IntArrInIntArr(FMouseDowned, FMouseUpped);

  // MouseUp and Clicks on Objects
  if ACount = -1 then
    vCount := Length(FMouseUpped)
  else
    vCount := Min(ACount, Length(FMouseUpped));

  for i := 0 to vCount - 1 do
    FModel.Objects[FMouseUpped[i]].OnMouseUp(FModel.Objects[FMouseUpped[i]], Button, Shift, x, y);

  if AClickObjects then
    Click(ACount);
end;

procedure TEngine2d.PrepareFastFields;
var
  vTmp: TFastField;
begin
  vTmp := TFastEngineWidth.Create(@FWidth);
  FModel.FastFields.Add('engine.width', vTmp);
  vTmp := TFastEngineHeight.Create(@FHeight);
  FModel.FastFields.Add('engine.height', vTmp);
end;

procedure TEngine2d.PrepareShadowObject;
begin
  FShadowObject := tSprite.Create;
  FObjectCreator.Add(TSprite(FShadowObject), 'shadow');
end;

procedure TEngine2d.setBackGround(ABmp: TBitmap);
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

procedure TEngine2d.setWidth(AWidth: integer);
begin
  FImage.Bitmap.Width := Round(AWidth * getScreenScale + 0.4);
  FWidth := AWidth;
end;

procedure TEngine2d.start;
begin
  FEngineThread.Resume;
  FStatus.Status := CGameStarted;
end;

procedure TEngine2d.stop;
begin
  FStatus.Status := CGameStopped;
end;

end.

