unit uSoObject;

interface

uses
  uGeometryClasses, System.Types, System.Classes, uCommonClasses;

type
  TSoObject = class
  private
 {   FOnExecute: TNotifyEvent<TSoContainer>;
    FExecutable: Boolean;   }
    //procedure SetExecutable(const Value: Boolean);
  protected
    FPosition: TPosition;
    FOnDestroyHandlers: TNotifyEventList;
    function GetCenter: TPointF;
    function GetScalePoint: TPointF;
    procedure SetCenter(const Value: TPointF);
    procedure SetPosition(const Value: TPosition);
    procedure SetRotate(const Value: Single);
    procedure SetScale(const Value: Single);
    procedure SetScalePoint(const Value: TPointF);
    procedure SetScaleX(const Value: Single);
    procedure SetScaleY(const Value: Single);
    procedure SetX(const Value: Single);
    procedure SetY(const Value: Single);
  public
    // Geometrical properties Геометрические свойства
    property Position: TPosition read FPosition write SetPosition; // Быстрое получение всех данных о позиции спрайта
    property X: Single read FPosition.x write SetX; // Координата X на главном битмапе
    property Y: Single read FPosition.y write SetY; // Координата Y на главном битмапе
    property Center: TPointF read GetCenter write SetCenter;
    property ScalePoint: TPointF read GetScalePoint write SetScalePoint;
    property Rotate: Single read FPosition.Rotate write SetRotate; // Угол поворота относительно центра
    property ScaleX: Single read FPosition.ScaleX write SetScaleX;  // Масштаб спрайта во время отрисовки
    property ScaleY: Single read FPosition.ScaleY write SetScaleY;  // Масштаб спрайта во время отрисовки
    property Scale: Single write SetScale;  // Масштаб спрайта во время отрисовки
    procedure AddDestroyHandler(const AHandler: TNotifyEvent);
    procedure RemoveDestroyHandler(const AHandler: TNotifyEvent);
    constructor Create;
    destructor Destroy; override;
  end;

implementation

{ TBaseUnitContainer }

procedure TSoObject.AddDestroyHandler(const AHandler: TNotifyEvent);
begin
  FOnDestroyHandlers.Add(AHandler);
end;

constructor TSoObject.Create;
begin
  FOnDestroyHandlers := TNotifyEventList.Create;
end;

destructor TSoObject.Destroy;
begin
  FOnDestroyHandlers.RaiseEvent(Self);
  FOnDestroyHandlers.Free;

  inherited;
end;

function TSoObject.GetCenter: TPointF;
begin
  Result := FPosition.XY;
end;

function TSoObject.GetScalePoint: TPointF;
begin
  Result := FPosition.Scale;
end;

procedure TSoObject.RemoveDestroyHandler(const AHandler: TNotifyEvent);
begin
  FOnDestroyHandlers.Remove(AHandler);
end;

procedure TSoObject.SetCenter(const Value: TPointF);
begin
  FPosition.X := Value.X;
  FPosition.Y := Value.Y;
end;

procedure TSoObject.SetPosition(const Value: TPosition);
begin
  FPosition := Value;
end;

procedure TSoObject.SetRotate(const Value: Single);
begin
  FPosition.Rotate := Value;
end;

procedure TSoObject.SetScale(const Value: Single);
var
  vSoot: Single;
begin
  if (FPosition.ScaleX) <> 0 then
  begin
    vSoot := FPosition.ScaleY / FPosition.scaleX;
  end
  else begin
    vSoot := 1;
  end;

  FPosition.scaleX := Value;
  FPosition.scaleY := vSoot * Value;
end;

procedure TSoObject.SetScalePoint(const Value: TPointF);
begin
  FPosition.ScaleX := Value.X;
  FPosition.ScaleY := Value.Y
end;

procedure TSoObject.SetScaleX(const Value: Single);
begin
  FPosition.ScaleX := Value;
end;

procedure TSoObject.SetScaleY(const Value: Single);
begin
  FPosition.ScaleY := Value;
end;

procedure TSoObject.SetX(const Value: Single);
begin
  FPosition.X := Value;
end;

procedure TSoObject.SetY(const Value: Single);
begin
  FPosition.Y := Value;
end;

end.
