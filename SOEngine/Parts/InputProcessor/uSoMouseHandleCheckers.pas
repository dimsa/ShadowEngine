unit uSoMouseHandleCheckers;

interface

uses
  uSoObject, uSoObjectDefaultProperties, uSoTypes, uSoColliderObject;


type

TCheckMouseHandleBehavior = function(ASender: TSoObject; const APoint: TPointF): Boolean;

function CanMouseHandleByColliderCheck(ASender: TSoObject; const APoint: TPointF): Boolean;
function CanMouseHandleBySqrMaxRadiusCheck(ASender: TSoObject; const APoint: TPointF): Boolean;
function CanMouseHandleByStaticRectCheck(ASender: TSoObject; const APoint: TPointF): Boolean;

implementation

function CanMouseHandleByColliderCheck(ASender: TSoObject; const APoint: TPointF): Boolean;
begin
  Result := ASender[Collider].Val<TSoColliderObj>.IsContainsPoint(APoint)
end;

function CanMouseHandleBySqrMaxRadiusCheck(ASender: TSoObject; const APoint: TPointF): Boolean;
begin
  Result := (Sqr(APoint.X) + Sqr(APoint.Y)) < ASender[RenditionRect].Val<TRectObject>.SqrMaxRadius * ASender.ScaleX;
end;

function CanMouseHandleByStaticRectCheck(ASender: TSoObject; const APoint: TPointF): Boolean;
begin
  Result := ASender[RenditionRect].Val<TRectObject>.Rect.Multiply(ASender.ScalePoint).Contains(APoint);
end;

end.

