unit uEngine2DAnimation;

interface

uses
  uSoTypes,
  uNamedList, uEngine2DClasses, uClasses, uGeometryClasses, uEngine2DStatus;

type
  // Здесь используется термин DelayedCreate, но он отличается от понятия
  // DelayedAnimation. DelayedCreate это создании анимации без сиюминутного
  // запоминания начальных условия, т.е. без Setup'а. Это используется когда
  // анимация должна запомнить свои начальные параметры после начала
  // анимирования, т.е. например если данная анимация указана как Next,
  // т.е. следующая за какой-то анимацией.

  // I use Term "Delayed create" but it has another term than DelayedAnimation
  // "Delayed create" creates animation at this moment, but it don't save the
  // Initial conditions, because it dont use Setup in this moment/
  // It can be used in situation like when it's the second animation in animation queue

  TAnimation = class
  strict private
    FSubject: Pointer; // Pointer to object for animation. Указатель на объект анимации
    FOnDeleteSubject: TNotifyEvent;
    FStatus: TEngine2DStatus;
    FNextAnimation: TAnimation;
    FSetupped: Boolean;
    FStopped: Boolean;
    FFinalized: Boolean;
    FStartPos: TPosition;
    FTimeTotal: Integer;
    FTimePassed: Double;
    FOnSetup: TProcedure;
    FOnDestroy: TProcedure;
    FOnFinalize: TProcedure;
    procedure SetSubject(const Value: Pointer);
   public
    property OnDeleteSubject: TNotifyEvent read FOnDeleteSubject write FOnDeleteSubject;
    property Status: TEngine2DStatus read FStatus write FStatus;
    property Stopped: Boolean read FStopped write FStopped; // Если анимация остановлена, она ничего не делает, что логично
    property Finalized: Boolean read FFinalized; // Финализирована ли анимация
    property Subject: Pointer read FSubject write SetSubject;//GetSubject write
    property NextAnimation: TAnimation read FNextAnimation write FNextAnimation; // Анимация, которая начинается после конца данной
    property Setupped: Boolean read FSetupped;// write FSetupped; // Для отложенной инициализации
    property StartPosition: TPosition read FStartPos write FStartPos;
    property TimeTotal: Integer read FTimeTotal write FTimeTotal; // Время в мс, сколько анимация будет длиться
    property TimePassed: Double read FTimePassed write FTimePassed; // Время в мс, сколько анимация уже длится
    property OnDestroy: TProcedure read FOnDestroy write FOnDestroy; // Процедура на уничтожение анимации
    property OnSetup: TProcedure read FOnSetup write FOnSetup; // Процедура на сетап анимации
    property OnFinalize: TProcedure read FOnFinalize write FOnFinalize; // Процедура на сетап анимации
    procedure RecoverStart; virtual; // Set the initial condition for object // Данный метод вызывается при вызове метода ClearForSubject в TAnimationList. Он должен приводить состояние объекта к начальному.
    procedure Finalize; virtual; // Set the final condtion for object  // Так бывает, что анимация пролетает свою конечную точку из-за скачков фпс. Так вот, это для того, чтобы присвоить конечные координаты
    function Animate: Byte; virtual; // Main method to animate object // Главная рабочая функция. Когда True, то значит анимация объекта завершена

    function AddNextAnimation(AAnimation: TAnimation): Integer; // Добавляет следующую анимацию, а если следующая анимация уже есть, то добавляет следующую анимацию следующей анимации и т.д. Выдает порядквый номер следующей анимации
    procedure Setup; virtual; // It's used for delayed create // Нужно для отложенного сетапа. На этот метод анимация запоминает стартовые параметры, например начальное положение
    procedure DeleteSubject;
    procedure HideSubject;
    constructor Create; virtual;
    destructor Destroy; override;
  const
    CDefaultTotalTime = 500; // Default time for animation // Время на анимацию, по умолчанию. Если хотите создать непрерывную, придется отнаследоваться и переписать метод Animate
    // Animation statues:
    CAnimationEnd = 0;  // Когда анимация закончена, она удаляется из списка анимация
    CAnimationInProcess = 1;  // Пока анимация не закончилась
    CNextAnimationInProcess = 2;  // Пока анимация не закончилась
  end;

implementation

uses
  uEngine2D, uEngine2DObject;

{ tEngine2DAnimation }

function TAnimation.AddNextAnimation(AAnimation: TAnimation): Integer;
begin
  Result := 0;
  if FNextAnimation = nil then
  begin
    FNextAnimation := AAnimation;
    Result := 1;
  end
  else
    Result := Result + FNextAnimation.AddNextAnimation(AAnimation);
end;

function tAnimation.Animate: Byte;
begin
  if FSetupped = False then
    Setup;

  Result := CAnimationInProcess;
  if TimePassed < TimeTotal then
  begin
    TimePassed := TimePassed + (1000 / FStatus.EngineFPS {vEngine.EngineThread.FPS});
    if TimePassed > TimeTotal then
      Result := CAnimationEnd;
  end else
  begin
    if FNextAnimation <> Nil then
    begin
      if not FFinalized then
        Finalize;
      Result := FNextAnimation.Animate;
      if Result <> CAnimationEnd then
        Result := CNextAnimationInProcess;
    end
    else begin
      if not FFinalized then
        Finalize;
      Result := CAnimationEnd;
    end;
  end;

end;

constructor tAnimation.Create;
begin
  FSetupped := False;
  FStopped := True; // Анимация создается остановленной, а перестает быть остановленной только после присвоения объекта
  FFinalized := False;
  FTimeTotal := CDefaultTotalTime;
  FTimePassed := 0;
end;

{constructor tAnimation.DelayedCreate;
begin
  FSetupped := False;
  FStopped := True; // Анимация создается остановленной, а перестает быть остановленной только после присвоения объекта
  FTimeTotal := CDefaultTotalTime;
  FTimePassed := 0;
end;   }

procedure TAnimation.DeleteSubject;
var
  vObj: tEngine2DObject;
begin
  vObj := FSubject;
  FOnDeleteSubject(vObj);
  vObj.Free;
end;

destructor tAnimation.Destroy;
begin
  if FNextAnimation <> Nil then
    FNextAnimation.Free;
  if Assigned(FOnDestroy) then
    FOnDestroy;
  inherited;
end;

procedure TAnimation.Finalize;
begin
  FFinalized := True;
  if Assigned(FOnFinalize) then
    FOnFinalize;
end;

procedure TAnimation.HideSubject;
begin
  TEngine2DObject(FSubject).Visible := False;
end;

procedure tAnimation.RecoverStart;
begin
  TEngine2DObject(FSubject).Position := FStartPos;
end;

procedure TAnimation.SetSubject(const Value: Pointer);
begin
  FSubject := Value;
  FStopped := False; //Внимание! После задания субьекта, анимация перестает быть остановленной
//  FStartPos := tEngine2DObject(Value).Position;
end;

procedure tAnimation.Setup;
begin
  if Not Assigned(FSubject) then
    Exit;
  if Assigned(FOnSetup) then
    FOnSetup;
  FStartPos := TEngine2DObject(FSubject).Position;
  FSetupped := True;
end;

end.
