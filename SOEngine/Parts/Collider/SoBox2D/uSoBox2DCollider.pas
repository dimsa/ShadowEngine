unit uSoBox2DCollider;

interface

uses
  UPhysics2D, UPhysics2DTypes, uSoTypes,
  uSoColliderObject, uSoObject;

type
  TSoBox2DColliderObj = class(TSoColliderObj)
  private
    FBody: TB2Body;
    FWorld: Tb2World;
    function B2TransformFromSubject: Tb2Transform;
  public
    function IsContainsPoint(const AX, AY: Single): Boolean; overload; override;
    function IsContainsPoint(const APoint: TPointF): Boolean; overload; override;
    constructor Create(const ASubject: TSoObject; const AWorld: Tb2World);
  end;

implementation

{ TSoBox2DColliderObj }

function TSoBox2DColliderObj.B2TransformFromSubject: Tb2Transform;
begin
  with Result do begin
   // p := TVector2.From();
  end;
end;

constructor TSoBox2DColliderObj.Create(const ASubject: TSoObject; const AWorld: Tb2World);
begin
  inherited Create(ASubject);

  FWorld := AWorld;
  FBody := Tb2Body.Create(Tb2BodyDef.Create, FWorld);
end;

function TSoBox2DColliderObj.IsContainsPoint(const AX, AY: Single): Boolean;
begin
  //FBody.GetFixtureList.GetShape.TestPoint(B2TransformFromSubject, TVecont2.From(AX, AY));

end;

function TSoBox2DColliderObj.IsContainsPoint(const APoint: TPointF): Boolean;
begin

end;

end.
