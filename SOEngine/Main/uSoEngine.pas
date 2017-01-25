unit uSoEngine;

interface

uses
  System.SyncObjs,
  uSoTypes, uCommonClasses, uEasyDevice,
  uClasses, uEngine2DClasses, uEngine2DThread, uSoModel, uSoEngineOptions,
  uEngine2DStatus, uSoObject,
  uSoObjectDefaultProperties, uSoEngineEvents,
  uSoEngineSize;

type
//  TManageDelegate = function(const AContainer: TSoObject): TUnitManager;

  TSoEngine = class
  strict private
    FEngineThread: TEngineThread; // Thread that paint all sprites (But there are possibility to use not one thread)  // Поток в котором происходит отрисовка
    FCritical: TCriticalSection; // The critical section for multithread operation, to protect model on changind in paint time // Критическая секция движка
    //FSize: TSoEngineSize;
    FModel: TSoModel; // All main lists are in It.
    FOptions: TSoEngineOptions; // All Engine options. If you add some feature to manage engine, it shoulb be here// Настройки движка
    FStatus: TEngine2DStatus; // All Engine status you can get from herem like width-height,speed and etc.
    FImage: TAnonImage; // It's the Image the Engine Paint in. // Имедж, в котором происходит отрисовка\
//    FDebug: Boolean; // There are some troubles to debug multithread app, so it for it // Не очень нужно, но помогает отлаживать те места, когда непонятно когда появляется ошибка
    //FEvents: TSoEngineEvents;
//    FManager: TSoManager; // Contains all managers
    FEngineObject: TSoObject; // it's object of engine
    procedure SetImage(const Value: TAnonImage);
    function GetFps: Single;
    procedure InitEngineObject;
    procedure InitDefaultOptions;
  protected
    property EngineThread: TEngineThread read FEngineThread;
    procedure WorkProcedure; virtual; // The main Paint procedure.
  public
    // Main properties of Engine. Ключевые свойства движка
    property Options: TSoEngineOptions read FOptions;

    procedure Start; virtual; // Start Engine. Need to run only once Включает движок
    procedure Suspend; virtual;// Suspend main thread
    procedure Resume; virtual;// Resume main thread

    constructor Create(const AImage: TAnonImage); virtual;
    destructor Destroy; override;

    // You should use Managers to Work with Engine
    //property Manager: TSoManager read FManager;

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
  InitDefaultOptions;

  FCritical := TCriticalSection.Create;
  FEngineThread := tEngineThread.Create;
  FEngineThread.WorkProcedure := WorkProcedure;

  FModel := TSoModel.Create(TAnonImage(FImage), FCritical, FOptions);

//  SubscribeImageEvent;

  //FManager := TSoManager.Create(FModel, FEvents);

  InitEngineObject;
end;

destructor TSoEngine.Destroy;
begin
  FModel.Free;
  FEngineThread.Free;
  FCritical.Free;
  FOptions.Free;
  {FEvents.Free;
  FManager.Free;
  FSize.Free; }

  inherited;
end;

function TSoEngine.GetFps: Single;
begin
  Result := FEngineThread.FPS;
end;

procedure TSoEngine.InitDefaultOptions;
begin
   FOptions := TSoEngineOptions.Create;
end;

procedure TSoEngine.InitEngineObject;
begin
  {with FManager.UnitManager.New('World') do begin
    FEngineObject := ActiveContainer;
    FEngineObject.AddProperty(RenditionRect).Obj := FSize.Rect;
  end;}
end;

{procedure TSoEngine.OnImageResize(ASender: TObject);
begin
  FEngineObject[RenditionRect].RaiseOnChange;

  FImage.Bitmap.Width := Round(FImage.Width * getScreenScale);
  FImage.Bitmap.Height := Round(FImage.Height * getScreenScale);
end;  }

procedure TSoEngine.Resume;
begin
  FEngineThread.Suspended := False;
end;

procedure TSoEngine.SetImage(const Value: TAnonImage);
begin
  FImage := Value;
//  SubscribeImageEvent;
end;

procedure TSoEngine.Start;
begin
//  OnImageResize(FImage);
  FImage.OnResize(FImage);
  FEngineThread.Start;
end;

procedure TSoEngine.Suspend;
begin
  FEngineThread.Suspended := True;
end;

procedure TSoEngine.WorkProcedure;
begin
  FModel.ExecuteOnTick;
end;

end.

