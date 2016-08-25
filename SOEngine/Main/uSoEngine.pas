unit uSoEngine;

interface

uses
  System.SyncObjs, FMX.Objects, FMX.Graphics, System.UITypes, System.Classes,
  uClasses, uEngine2DClasses, uEngine2DThread, uSoModel, uEngine2DOptions,
  uEngine2DManager, uEngine2DStatus;

type
  TSoEngine = class
  strict private
    FEngineThread: TEngineThread; // Thread that paint all sprites (But there are possibility to use not one thread)  // Поток в котором происходит отрисовка
    FCritical: TCriticalSection; // The critical section for multithread operation, to protect model on changind in paint time // Критическая секция движка
    FModel: TSoModel; // All main lists are in It.
    FOptions: TEngine2DOptions; // All Engine options. If you add some feature to manage engine, it shoulb be here// Настройки движка
    FObjectCreator: TEngine2DManager; // This object work with Model items. It's controller/
    FStatus: TEngine2DStatus; // All Engine status you can get from herem like width-height,speed and etc.
    FIsMouseDowned: Boolean; // True if Mouse is Downed  // Хранит состояние нажатости мыши
    FImage: TImage; // It's the Image the Engine Paint in. // Имедж, в котором происходит отрисовка
    FBackGround: TBitmap; // Background of Engine that paints on every tick. Not sure if it should be here // Бэкграунд. Всегда рисуется в Repaint на весь fImage
    FWidth, FHeight: integer; // Размер поля имеджа и движка
//    FDebug: Boolean; // There are some troubles to debug multithread app, so it for it // Не очень нужно, но помогает отлаживать те места, когда непонятно когда появляется ошибка
    FBackgroundBehavior: TProcedure; // Procedure to Paint Background. It can be default or Parallax(like in Asteroids example) or any type you want
    FInBeginPaintBehavior: TProcedure; // Method is called before Paint
    FInEndPaintBehavior: TProcedure; // Method is called after Paint

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

//    procedure AssignShadowObject(ASpr: tEngine2DObject); // Assign object properties to Shadow Object// Ассигнет спрайт в ShadowObject
//    property ShadowObject: tEngine2DObject read FShadowObject; // Указатель на Теневой объект.

//    procedure LoadResources(const AFileName: string); // Loads resources(animations frames) for sprites. Shoul be in Manager
//    procedure LoadSECSS(const AFileName: string); // Loads SECSS filee to use it Engine. It should Be in Manager
//    procedure LoadSEJSON(const AFileName: string);  experimental; // Working on it! It's loading of object that created by Sprite Shape Builder It should be in Manager
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

implementation

{ TSoEngine }

procedure TSoEngine.BackgroundDefaultBehavior;
begin

end;

procedure TSoEngine.Click(const ACount: Integer);
begin

end;

constructor TSoEngine.Create;
begin

end;

destructor TSoEngine.Destroy;
begin

  inherited;
end;

procedure TSoEngine.InBeginPaintDefaultBehavior;
begin

end;

procedure TSoEngine.InEndPaintDefaultBehavior;
begin

end;

procedure TSoEngine.Init(AImage: TImage);
begin

end;

function TSoEngine.IsHor: Boolean;
begin

end;

procedure TSoEngine.MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; x, y: single; const ACount: Integer);
begin

end;

procedure TSoEngine.MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; x, y: single; const ACount: Integer;
  const AClickObjects: Boolean);
begin

end;

procedure TSoEngine.Resize;
begin

end;

procedure TSoEngine.setBackGround(ABmp: TBitmap);
begin

end;

procedure TSoEngine.SetBackgroundBehavior(const Value: TProcedure);
begin

end;

procedure TSoEngine.SetHeight(AHeight: integer);
begin

end;

procedure TSoEngine.SetWidth(AWidth: integer);
begin

end;

procedure TSoEngine.Start;
begin

end;

procedure TSoEngine.Stop;
begin

end;

procedure TSoEngine.WorkProcedure;
begin

end;

end.
