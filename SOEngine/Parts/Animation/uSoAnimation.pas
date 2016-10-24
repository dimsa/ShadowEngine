unit uSoAnimation;

interface

uses
  uSoObject, uSoTypes,
  uEngine2DStatus, uEngine2DClasses, uSoBasePart;

type
  TEngineThreadParams = class
  private
    FEngineStatus: TEngine2DStatus;
    function GetFPS: Single;
    function GetSpeed: Single;
  public
    property Speed: Single read GetSpeed;
    property FPS: Single read GetFPS;
    constructor Create(const AEngineStatus: TEngine2DStatus);
  end;

  TSoAnimation = class abstract(TSoBasePart)
  strict private
    FThreadParams: TEngineThreadParams;
    FTimeTotal: Integer; // How many milliseconds this animation will run
    FTimePassed: Double; // How many millisecnds passed
    FOnFinish: TNotifyEvent;
    FOnCancel: TNotifyEvent;
    FOnStart: TNotifyEvent; // Set the initial condition for object // Данный метод вызывается при вызове метода ClearForSubject в TAnimationList. Он должен приводить состояние объекта к начальному.
    procedure Finalize; virtual; abstract; // Set the final values that animates for object  // Так бывает, что анимация пролетает свою конечную точку из-за скачков фпс. Так вот, это для того, чтобы присвоить конечные координаты
    procedure RecoverStart; virtual; abstract;
   public
    property OnStart: TNotifyEvent read FOnStart write FOnStart;
    property OnFinish: TNotifyEvent read FOnFinish write FOnFinish;
    property OnCancel: TNotifyEvent read FOnCancel write FOnCancel;
    property Subject: TSoObject read FSubject;// write SetSubject;//GetSubject write
    property TimeTotal: Integer read FTimeTotal; // Время в мс, сколько анимация будет длиться
    property TimePassed: Double read FTimePassed;// Время в мс, сколько анимация уже длится
    procedure Cancel; virtual; abstract; // Returns subject properties to Start and stop animation;
    function Animate: Byte; virtual; // Main method to animate object // Главная рабочая функция. Когда True, то значит анимация объекта завершена
    constructor Create(const ASubject: TSoObject; const AThreadParams: TEngineThreadParams); virtual;
    destructor Destroy; override;
  const
    CDefaultTotalTime = 500; // Default time for animation // Время на анимацию, по умолчанию. Если хотите создать непрерывную, придется отнаследоваться и переписать метод Animate
  end;

implementation

{ TEngineThreadParams }

constructor TEngineThreadParams.Create(const AEngineStatus: TEngine2DStatus);
begin
  FEngineStatus := AEngineStatus;
end;

function TEngineThreadParams.GetFPS: Single;
begin
  Result := FEngineStatus.EngineFPS;
end;

function TEngineThreadParams.GetSpeed: Single;
begin
  Result := FEngineStatus.EngineSpeed;
end;

{ TSoAnimation }

function TSoAnimation.Animate: Byte;
begin

end;

constructor TSoAnimation.Create(const ASubject: TSoObject;
  const AThreadParams: TEngineThreadParams);
begin
  inherited Create(ASubject);

  FSubject.AddDestroyHandler(OnSubjectDestroy);
  FThreadParams := AThreadParams;
end;

destructor TSoAnimation.Destroy;
begin
  FThreadParams := nil;
  inherited;
end;

end.

