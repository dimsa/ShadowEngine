unit uEngine2DThread;

interface

uses
  System.Classes,
  uClasses, uEngine2DClasses;

type
  TEngineThread = class(TThread) // Наследник треда, считает фпс
  private
    FTickCount, FTickBegin, FTickEnd: Int64;
    FLastFPS: Single;
    FSleep: Integer; // FPS Modificator to leave time for messages Модификатор FPS
    FWorkProcedure: TProcedure;
    procedure SetTickEnd(AValue: Int64);
    procedure SetTickBegin(AValue: Int64);
    procedure DoNothing;
    function GetSpeed: Single;
    procedure SetWorkProcedure(const Value: TProcedure);
  protected
    procedure Execute; override; // Процедура выполнения потока
  public
    property WorkProcedure: TProcedure write SetWorkProcedure; // Procedure that will be done in thread
    property TickBegin: int64 read FTickBegin write SetTickBegin;
    property TickEnd: int64 read FTickEnd write SetTickEnd;
    function FPS: Single;
    property Speed: Single read GetSpeed;
    constructor Create;
    destructor Destroy; override;
  const
    CMinSleep = 2;
  {$IFDEF WIN32}
    CLeftFPSBorder = 30;
    CRightFPSBorder = 120;
    CMiddleFPS = 60;
  {$ENDIF WIN32}
  {$IFDEF MACOS}
    CLeftFPSBorder = 30;
    CRightFPSBorder = 120;
    CMiddleFPS = 60;
  {$ENDIF MACOS}
  {$IFDEF ANDROID}
    CLeftFPSBorder = 20;
    CRightFPSBorder = 40;
    CMiddleFPS = 30;
  {$ENDIF ANDROID}
  end;

implementation

{ tEngineThread }

constructor TEngineThread.Create;
begin
  inherited Create(True);
  FreeOnTerminate := True;

  FWorkProcedure := doNothing;
{$IFDEF WINDOWS}
  Priority := tpNormal;
{$ENDIF WINDOWS}
  FTickCount := 0;
  FTickBegin := 0;
  FTickEnd := 0;
  FLastFPS := CLeftFPSBorder;
  FSleep := CMinSleep;
end;

destructor tEngineThread.Destroy;
begin
  inherited;

end;

procedure tEngineThread.DoNothing;
begin

end;

procedure tEngineThread.Execute;
begin
//  inherited;
  while not Terminated do
  begin
    TickBegin := GetTickCount;
    Synchronize(FWorkProcedure);
    Sleep(FSleep);
    TickEnd := GetTickCount;
  end;

end;

function TEngineThread.FPS: Single;
begin
  if FLastFps >= 1 then
    Exit(FLastFPS)
  else begin
    Result := 1;
  end;
end;

function TEngineThread.GetSpeed: Single;
begin
  if (FPS > CRightFPSBorder) or (FPS < CLeftFPSBorder) then
    Exit(60 / CMiddleFPS);

  Result := 60 / FPS;
end;

procedure tEngineThread.setTickBegin(AValue: int64);
begin
  if fTickCount = 0 then
    fTickBegin := AValue;
end;

procedure tEngineThread.setTickEnd(AValue: Int64);
begin
  if (fTickCount >= 10) {and (fTickEnd <> fTickBegin)} then
  begin
    fTickEnd := AValue;
    fLastFps := 1000 / ((fTickEnd - fTickBegin) / fTickCount);
    fTickCount := 0;
    if fLastFps > CRightFPSBorder then
      fSleep := FSleep + 1;
    if (fLastFps < CLeftFPSBorder) and (fLastFps > 0) and (fSleep > CMinSleep) then
      fSleep := FSleep - 1;
  end
  else
    fTickCount := fTickCount + 1;
end;

procedure TEngineThread.SetWorkProcedure(const Value: TProcedure);
begin
  FWorkProcedure := Value;
end;

end.



