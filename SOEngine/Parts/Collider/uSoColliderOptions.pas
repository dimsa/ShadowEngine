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
    procedure SetNeedToGenerateObjectsEvents(const Value: Boolean);
    procedure SetNeedToGenerateWorldEvents(const Value: Boolean);
    procedure SetGravity(const Value: TPointF);
  public
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
  FGravity := TPointF.Zero;
end;

procedure TSoColliderOptions.SetGravity(const Value: TPointF);
begin
  FGravity := Value;
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
