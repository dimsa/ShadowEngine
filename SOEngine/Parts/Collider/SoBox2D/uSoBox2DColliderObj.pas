unit uSoBox2DColliderObj;

interface

uses
  UPhysics2D, UPhysics2DTypes, uSoTypes,
  uSoColliderObject, uSoObject, uSoBox2DUtils, uColliderDefinition;

type
  TSoBox2DColliderObj = class(TSoColliderObj)
  private
    FBody: TB2Body;
    FWorld: Tb2World;
    FColliderDefinition: TColliderDefinition;
    function B2TransformFromSubject: Tb2Transform;
  public
    function IsContainsPoint(const AX, AY: Single): Boolean; overload; override;
    function IsContainsPoint(const APoint: TPointF): Boolean; overload; override;
    constructor Create(const ASubject: TSoObject; const AWorld: Tb2World; const AColliderDef: TColliderDefinition);
    destructor Destroy; override;
  end;

implementation

{ TSoBox2DColliderObj }

function TSoBox2DColliderObj.B2TransformFromSubject: Tb2Transform;
begin
  with Result do begin
    p.x := FSubject.X;
    p.y := FSubject.Y;
  end;
end;

constructor TSoBox2DColliderObj.Create(const ASubject: TSoObject; const AWorld: Tb2World; const AColliderDef: TColliderDefinition);
begin
  inherited Create(ASubject);

  FColliderDefinition := AColliderDef;
  FWorld := AWorld;
  FBody := Tb2Body.Create(Tb2BodyDef.Create, FWorld);
end;

destructor TSoBox2DColliderObj.Destroy;
begin
  FBody.Free;
//  FColliderDefinition.Free;
  inherited;
end;

function TSoBox2DColliderObj.IsContainsPoint(const AX, AY: Single): Boolean;
begin
  FBody.GetFixtureList.GetShape.TestPoint(B2TransformFromSubject, Vector2FromPoint(AX, AY));
end;

function TSoBox2DColliderObj.IsContainsPoint(const APoint: TPointF): Boolean;
begin
  FBody.GetFixtureList.GetShape.TestPoint(B2TransformFromSubject, Vector2FromPoint(APoint));
end;

end.
