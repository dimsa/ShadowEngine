unit uSoPosition;

interface

uses
  uGeometryClasses, uCommonClasses, uSoTypes,
  uISoPositionAdapter, uSoPositionAdapter;

type
  TSoPosition = class
  private
    FPosition: TPosition;
    FPositionAdapter: ISoPositionAdapter;
    FChanged: TNotifyEventList;
    FAbsolutePositionChanged: TEventList<TPosition>;
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
    function GetCenter: TPointF;
    function GetScalePoint: TPointF;
    procedure SetCenter(const Value: TPointF);
    procedure SetScalePoint(const Value: TPointF);
    procedure SetScale(const Value: Single);
  public
    property X: Single read GetX write SetX;
    property Y: Single read GetY write SetY;
    property Center: TPointF read GetCenter write SetCenter;
    property Rotate: Single read GetRotate write SetRotate;
    property ScaleX: Single read GetScaleX write SetScaleX;
    property ScaleY: Single read GetScaleY write SetScaleY;
    property Scale: Single write SetScale;
    property ScalePoint: TPointF read GetScalePoint write SetScalePoint;
    property Changed: TNotifyEventList read FChanged;
    property AbsolutePositionChanged: TEventList<TPosition> read FAbsolutePositionChanged;
    procedure SetPositionSilent(const AX, AY: Single; const ARotate: Single);
    constructor Create(const APositionAdapter: ISoPositionAdapter); overload;
    constructor Create; overload;
    destructor Destroy; override;
  end;

implementation

{ TSoPosition }

constructor TSoPosition.Create(const APositionAdapter: ISoPositionAdapter);
begin
  FChanged := TNotifyEventList.Create;
  FAbsolutePositionChanged := TEventList<TPosition>.Create;
  FPositionAdapter := APositionAdapter;

  FPosition.X := 0;
  FPosition.Y := 0;
  FPosition.Rotate := 0;
  FPosition.ScaleX := 1;
  FPosition.ScaleY := 1;
end;

constructor TSoPosition.Create;
begin
  Create(TAbsolutePositionAdapter.Create);
end;

destructor TSoPosition.Destroy;
begin
  FChanged.Free;
  FAbsolutePositionChanged.Free;
  FPositionAdapter := nil;
  inherited;
end;

function TSoPosition.GetCenter: TPointF;
begin
  Result := FPositionAdapter.AdaptCenter(FPosition.XY);
end;

function TSoPosition.GetRotate: Single;
begin
  Result := FPositionAdapter.AdaptRotate(FPosition.Rotate);
end;

function TSoPosition.GetScalePoint: TPointF;
begin
  Result := FPositionAdapter.AdaptScale(FPosition.Scale);
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
  FAbsolutePositionChanged.RaiseEvent(Self, FPosition);
end;

procedure TSoPosition.SetCenter(const Value: TPointF);
begin
  FPosition.X := FPositionAdapter.AdaptX(Value.X);
  FPosition.Y := FPositionAdapter.AdaptY(Value.Y);
  RaiseChanged;
end;

procedure TSoPosition.SetPositionSilent(const AX, AY, ARotate: Single);
begin
  FPosition.X := AX;
  FPosition.Y := AY;
  FPosition.Rotate := ARotate;
end;

procedure TSoPosition.SetRotate(const Value: Single);
begin
  FPosition.Rotate := FPositionAdapter.AdaptRotate(Value);
  RaiseChanged;
end;

procedure TSoPosition.SetScale(const Value: Single);
begin
  FPosition.X := FPositionAdapter.AdaptScaleX(Value);
  FPosition.Y := FPositionAdapter.AdaptScaleX(Value);
  RaiseChanged;
end;

procedure TSoPosition.SetScalePoint(const Value: TPointF);
begin
  FPosition.ScaleX := FPositionAdapter.AdaptScaleX(Value.X);
  FPosition.ScaleY := FPositionAdapter.AdaptScaleY(Value.Y);
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
