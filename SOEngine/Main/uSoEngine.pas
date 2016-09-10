unit uSoEngine;

interface

uses
  System.SyncObjs, FMX.Objects, FMX.Graphics, System.UITypes, System.Classes,
  uClasses, uEngine2DClasses, uEngine2DThread, uSoModel, uEngine2DOptions,
  uEngine2DManager, uEngine2DStatus, uSoContainer,
  uWorldManager, uUnitManager, uTemplateManager;

type
  TManageDelegate = function(const AContainer: TSoContainer): TUnitManager;

  TSoEngine = class
  strict private
    FEngineThread: TEngineThread; // Thread that paint all sprites (But there are possibility to use not one thread)  // Поток в котором происходит отрисовка
    FCritical: TCriticalSection; // The critical section for multithread operation, to protect model on changind in paint time // Критическая секция движка
    FModel: TSoModel; // All main lists are in It.
    FOptions: TEngine2DOptions; // All Engine options. If you add some feature to manage engine, it shoulb be here// Настройки движка
    FObjectCreator: TEngine2DManager; // This object work with Model items. It's controller/
    FStatus: TEngine2DStatus; // All Engine status you can get from herem like width-height,speed and etc.
    FIsMouseDowned: Boolean; // True if Mouse is Downed  // Хранит состояние нажатости мыши
    FImage: TImage; // It's the Image the Engine Paint in. // Имедж, в котором происходит отрисовка\
    FWidth, FHeight: Single; // Размер поля имеджа и движка
//    FDebug: Boolean; // There are some troubles to debug multithread app, so it for it // Не очень нужно, но помогает отлаживать те места, когда непонятно когда появляется ошибка
    FBackgroundBehavior: TProcedure; // Procedure to Paint Background. It can be default or Parallax(like in Asteroids example) or any type you want
    FInBeginPaintBehavior: TProcedure; // Method is called before Paint
    FInEndPaintBehavior: TProcedure; // Method is called after Paint
    FUnitManager: TUnitManager;
    FWorldManager: TWorldManager;
    FTemplateManager: TTemplateManager;
    procedure OnImageResize(ASender: TObject);
    function IsHor: Boolean;
    procedure SetImage(const Value: TImage);
  private
    function GetFps: Single;
  protected
    property EngineThread: TEngineThread read FEngineThread;
    procedure WorkProcedure; virtual; // The main Paint procedure.
  public
    // Main properties of Engine. Ключевые свойства движка
    property Image: TImage read FImage write SetImage;

    property Width: Single read FWidth;
    property Height: Single read FHeight;

    property Options: TEngine2dOptions read FOptions;

    procedure Start; virtual; // Включает движок
    procedure Stop; virtual;// Выключает движок

    constructor Create(const AImage: TAnonImage); virtual;
    destructor Destroy; override;

    // You should use Managers to Work with Engine
    property WorldManager: TWorldManager read FWorldManager;
    property UnitManager: TUnitManager read FUnitManager;
    property TemplateManager: TTemplateManager read FTemplateManager;
    property Status: TEngine2DStatus read FStatus;
    property Fps: Single read GetFps;
    const
      CGameStarted = 1;
      CGameStopped = 255;
  end;

implementation

{ TSoEngine }

constructor TSoEngine.Create(const AImage: TAnonImage);
begin
  FImage := AImage;
  FOptions := TEngine2DOptions.Create;

  FCritical := TCriticalSection.Create;
  FEngineThread := tEngineThread.Create;
  FEngineThread.WorkProcedure := WorkProcedure;

  FModel := TSoModel.Create(TAnonImage(FImage), FCritical, IsHor);
  FUnitManager := TUnitManager.Create(FModel);
  FWorldManager := TWorldManager.Create(FModel);
  FTemplateManager := TTemplateManager.Create(FModel);

  FOptions.Up([EAnimateForever, EUseCollider]);
  FOptions.Down([EClickOnlyTop]);
end;

destructor TSoEngine.Destroy;
begin
  FModel.Free;
  FEngineThread.Free;
  FCritical.Free;
  FOptions.Free;

  inherited;
end;

function TSoEngine.GetFps: Single;
begin
  Result := FEngineThread.FPS;
end;

{procedure TSoEngine.Init(AImage: TImage);
begin
  Image := AImage;
end;   }

function TSoEngine.IsHor: Boolean;
begin
  Result := FWidth > FHeight;
end;

{rocedure TSoEngine.OnMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; x, y: single);
begin
  FModel.ExecuteMouseDown()
end;

procedure TSoEngine.OnMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; x, y: single);
begin

end;  }

procedure TSoEngine.OnImageResize(ASender: TObject);
begin
  FWidth := TImage(ASender).Width;
  FHeight := TImage(ASender).Height;
end;

procedure TSoEngine.SetImage(const Value: TImage);
begin
  if Assigned(FImage) then
  begin
    FImage.OnResize := nil;
    FImage.OnMouseDown := nil;
    FImage.OnMouseUp := nil;
  end;

  FImage := Value;

  FImage.OnResize := OnImageResize;
  FImage.OnMouseDown := FModel.ExecuteMouseDown;
  FImage.OnMouseUp := FModel.ExecuteMouseUp;
end;

procedure TSoEngine.Start;
begin

end;

procedure TSoEngine.Stop;
begin

end;

procedure TSoEngine.WorkProcedure;
begin
  FModel.ExecuteOnTick;
end;

end.
