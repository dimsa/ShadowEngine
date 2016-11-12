unit uSoColliderOptions;

interface

uses
  uSoTypes;

type
  TSoColliderOptions = class
  private
    FNeedToGenerateObjectsEvents: Boolean;
    FNeedToGenerateWorldEvents: Boolean;
    FGravity: TPointF;
    FIsUsingBox2D: Boolean;
    FIsPositionRefreshing: Boolean;
    procedure SetNeedToGenerateObjectsEvents(const Value: Boolean);
    procedure SetNeedToGenerateWorldEvents(const Value: Boolean);
    procedure SetGravity(const Value: TPointF);
    procedure SetIsUsingBox2D(const Value: Boolean);
    procedure SetIsPositionRefreshing(const Value: Boolean);
  public
    property IsUsingBox2D: Boolean read FIsUsingBox2D write SetIsUsingBox2D;
    property IsPositionRefreshing: Boolean read FIsPositionRefreshing write SetIsPositionRefreshing;
    property NeedToGenerateWorldEvents: Boolean read FNeedToGenerateWorldEvents write SetNeedToGenerateWorldEvents;
    property NeedToGenerateObjectsEvents: Boolean read FNeedToGenerateObjectsEvents write SetNeedToGenerateObjectsEvents;
    property Gravity: TPointF read FGravity write SetGravity;

    constructor Create;
  end;

implementation

{ TSoColliderOptions }

constructor TSoColliderOptions.Create;
begin
  FNeedToGenerateObjectsEvents := True;
  FNeedToGenerateWorldEvents := True;
  FIsPositionRefreshing := True;
  FIsUsingBox2D := True;
  FGravity := TPointF.Zero;
end;

procedure TSoColliderOptions.SetGravity(const Value: TPointF);
begin
  FGravity := Value;
end;

procedure TSoColliderOptions.SetIsPositionRefreshing(const Value: Boolean);
begin
  FIsPositionRefreshing := Value;
end;

procedure TSoColliderOptions.SetIsUsingBox2D(const Value: Boolean);
begin
  FIsUsingBox2D := Value;
end;

procedure TSoColliderOptions.SetNeedToGenerateObjectsEvents(
  const Value: Boolean);
begin
  FNeedToGenerateObjectsEvents := Value;
end;

procedure TSoColliderOptions.SetNeedToGenerateWorldEvents(const Value: Boolean);
begin
  FNeedToGenerateWorldEvents := Value;
end;

end.
