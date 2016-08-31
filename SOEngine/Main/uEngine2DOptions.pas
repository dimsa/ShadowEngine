unit uEngine2DOptions;

interface

type

  tEngine2DOptionsEnum = (EClickOnlyTop, EAnimateForever, EDrawFigures, EAutoloadFormatter, EUseCollider);

  tEngine2DOptions = class
  private
    FToClickOnlyTop: Boolean; // If true, Engine will be call OnClick only on Top Object  // События кликов передаются только верхнему объекту в клике
    FToAnimateForever: Boolean; // If True, Engine will repaint scene despite of there are nothing changed by animations // Сообщает, что надо перерисовывать сцену, даже если нет анимаций
    FToDrawFigures: Boolean; // If true Engine will paint Collider Shape of object // Рисовать ли форму объектов
    FToAutoloadFormatter: Boolean; // If true it will apply Formatters to object on adding // Подключать ли форматтерсы сразу же при добавлении объекта
    FToUseCollider: Boolean; // If true Engine will use collider
  public
    property ToClickOnlyTop: Boolean read FToClickOnlyTop; //write FToClickOnlyTop;
    property ToDrawFigures: Boolean read FToDrawFigures;// write FToDrawFigures;
    property ToAnimateForever: Boolean read FToAnimateForever;// write FToAnimateForever;
    property ToAutoloadFormatter: Boolean read FToAutoloadFormatter; // write FToAutoloadFormatter;
    property ToUseCollider: Boolean read FToUseCollider;
    procedure Swap(const AOptions: array of tEngine2DOptionsEnum);
    procedure Up(const AOptions: array of tEngine2DOptionsEnum);
    procedure Down(const AOptions: array of tEngine2DOptionsEnum);
    constructor Create;
  end;

implementation

{ tEngine2DOptions }

constructor tEngine2DOptions.Create;
begin

end;

procedure tEngine2DOptions.Down(const AOptions: array of tEngine2DOptionsEnum);
var
  i: Integer;
begin
  for i := 0 to Length(AOptions)-1 do
    case AOptions[i] of
      EClickOnlyTop: FToClickOnlyTop := False;
      EAnimateForever: FToAnimateForever := False;
      EDrawFigures: FToDrawFigures := False;
      EAutoloadFormatter: FToAutoloadFormatter := False;
      EUseCollider: FToUseCollider := False;
    end;
end;

procedure tEngine2DOptions.Swap(const AOptions: array of tEngine2DOptionsEnum);
var
  i: Integer;
begin
  for i := 0 to Length(AOptions)-1 do
    case AOptions[i] of
      EClickOnlyTop: FToClickOnlyTop := not FToClickOnlyTop;
      EAnimateForever: FToAnimateForever := not FToAnimateForever;
      EDrawFigures: FToDrawFigures := not FToDrawFigures;
      EAutoloadFormatter: FToAutoloadFormatter := not FToAutoloadFormatter;
      EUseCollider: FToUseCollider := not FToUseCollider;
    end;
end;

procedure tEngine2DOptions.Up(const AOptions: array of tEngine2DOptionsEnum);
var
  i: Integer;
begin
  for i := 0 to Length(AOptions)-1 do
    case AOptions[i] of
      EClickOnlyTop: FToClickOnlyTop := True;
      EAnimateForever: FToAnimateForever := True;
      EDrawFigures: FToDrawFigures := True;
      EAutoloadFormatter: FToAutoloadFormatter := True;
      EUseCollider: FToUseCollider := True;
    end;
end;

end.
