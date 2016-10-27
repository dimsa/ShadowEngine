unit uSoBox2DUtils;

interface

uses
  UPhysics2DTypes,
  uSoTypes;

function Vector2FromPoint(APoint: TPointF): TVector2; overload;
function Vector2FromPoint(AX, AY: Single): TVector2; overload;

implementation

function Vector2FromPoint(APoint: TPointF): TVector2;
begin
  Result.x := APoint.X;
  Result.y := APoint.Y;
end;

function Vector2FromPoint(AX, AY: Single): TVector2; overload;
begin
  Result.x := AX;
  Result.y := AY;
end;

end.
