unit uSoPositionAdapter;

interface

uses
  uSoBasePart;

type
  TSoPositionAdapter = class(TSoBasePart)
  private
    FRotate: Single;
    FScaleX: Single;
    FScaleY: Single;
    FX: Single;
    FY: Single;
    procedure SetRotate(const Value: Single);
    procedure SetScaleX(const Value: Single);
    procedure SetScaleY(const Value: Single);
    procedure SetX(const Value: Single);
    procedure SetY(const Value: Single);
  public
    property X: Single read FX write SetX;
    property Y: Single read FY write SetY;
    property Rotate: Single read FRotate write SetRotate;
    property ScaleX: Single read FScaleX write SetScaleX;
    property ScaleY: Single read FScaleY write SetScaleY;
  end;

implementation

{ TSoPositionAdapter }

procedure TSoPositionAdapter.SetRotate(const Value: Single);
begin
  FRotate := Value;
end;

procedure TSoPositionAdapter.SetScaleX(const Value: Single);
begin
  FScaleX := Value;
end;

procedure TSoPositionAdapter.SetScaleY(const Value: Single);
begin
  FScaleY := Value;
end;

procedure TSoPositionAdapter.SetX(const Value: Single);
begin
  FX := Value;
end;

procedure TSoPositionAdapter.SetY(const Value: Single);
begin
  FY := Value;
end;

end.
