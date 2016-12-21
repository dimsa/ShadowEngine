unit uSoPosition;

interface

uses
  uSoBasePosition, uGeometryClasses, uCommonClasses, uSoPositionAdapter;

type
  TSoPosition = class sealed(TSoBasePosition)
  private
    FPosition: TPosition;
    FPositionAdapter: TSoPositionAdapter;
    FChanged: TNotifyEventList;
    function GetRotate: Single;
    function GetScaleX: Single;
    function GetScaleY: Single;
    function GetX: Single;
    function GetY: Single;
    procedure SetRotate(const Value: Single);
    procedure SetScaleX(const Value: Single);
    procedure SetScaleY(const Value: Single);
    procedure SetX(const Value: Single);
    procedure SetY(const Value: Single);
    procedure RaiseChanged;
  public
    property X: Single read GetX write SetX;
    property Y: Single read GetY write SetY;
    property Rotate: Single read GetRotate write SetRotate;
    property ScaleX: Single read GetScaleX write SetScaleX;
    property ScaleY: Single read GetScaleY write SetScaleY;
    property Changed: TNotifyEventList read FChanged;
    constructor Create;
    destructor Destroy; override;
  end;

implementation

{ TSoPosition }

constructor TSoPosition.Create;
begin
  FChanged := TNotifyEventList.Create;
end;

destructor TSoPosition.Destroy;
begin
  FChanged.Free;
  inherited;
end;

function TSoPosition.GetRotate: Single;
begin
  Result := FPositionAdapter.AdaptRotate(FPosition.Rotate);
end;

function TSoPosition.GetScaleX: Single;
begin
  Result := FPositionAdapter.AdaptScaleX(FPosition.ScaleX);
end;

function TSoPosition.GetScaleY: Single;
begin
  Result := FPositionAdapter.AdaptScaleY(FPosition.ScaleY);
end;

function TSoPosition.GetX: Single;
begin
  Result := FPositionAdapter.AdaptX(FPosition.X);
end;

function TSoPosition.GetY: Single;
begin
  Result := FPositionAdapter.AdaptY(FPosition.Y);
end;

procedure TSoPosition.RaiseChanged;
begin
  FChanged.RaiseEvent(Self);
end;

procedure TSoPosition.SetRotate(const Value: Single);
begin
  FPosition.Rotate := FPositionAdapter.AdaptRotate(Value);
  RaiseChanged;
end;

procedure TSoPosition.SetScaleX(const Value: Single);
begin
  FPosition.ScaleX := FPositionAdapter.AdaptScaleX(Value);
  RaiseChanged;
end;

procedure TSoPosition.SetScaleY(const Value: Single);
begin
  FPosition.ScaleY := FPositionAdapter.AdaptScaleY(Value);
  RaiseChanged;
end;

procedure TSoPosition.SetX(const Value: Single);
begin
  FPosition.X := FPositionAdapter.AdaptX(Value);
  RaiseChanged;
end;

procedure TSoPosition.SetY(const Value: Single);
begin
  FPosition.Y := FPositionAdapter.AdaptY(Value);
  RaiseChanged;
end;

end.
