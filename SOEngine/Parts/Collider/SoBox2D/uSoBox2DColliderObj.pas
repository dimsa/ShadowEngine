unit uSoBox2DColliderObj;

interface

uses
  UPhysics2D, UPhysics2DTypes, uSoTypes, uGeometryClasses,
  uSoColliderObject, uSoObject, uSoBox2DUtils, uColliderDefinition, uRawShapeBox2DConverter;

type
  TSoBox2DColliderObj = class(TSoColliderObj)
  private
    FBodyDef: Tb2BodyDef;
    FBody: TB2Body;
    FWorld: Tb2World;
    function B2TransformFromSubject: Tb2Transform;
    procedure OnPositionChanged(ASender: TObject; APosition: TPosition);
  public
    procedure RefreshSubjectPosition; override;
    function IsContainsPoint(const AX, AY: Single): Boolean; overload; override;
    function IsContainsPoint(const APoint: TPointF): Boolean; overload; override;
    procedure ApplyForce(const AX, AY: Single; const ACenterX: Single = 0; const ACenterY: Single = 0); override;
    constructor Create(const ASubject: TSoObject; const AWorld: Tb2World; const AColliderDef: TColliderDefinition);
    destructor Destroy; override;
  end;

implementation

{ TSoBox2DColliderObj }

procedure TSoBox2DColliderObj.ApplyForce(const AX, AY, ACenterX,
  ACenterY: Single);
var
  vVecForce, vVecCenterOfForce: TVector2;
begin
  vVecForce.x := AX;
  vVecForce.y := AY;
  vVecCenterOfForce.x := ACenterX;
  vVecCenterOfForce.y := ACenterY;

  FBody.ApplyForce(vVecForce, vVecCenterOfForce);
end;

function TSoBox2DColliderObj.B2TransformFromSubject: Tb2Transform;
begin
  with Result do begin
    p.x := FSubject.X;
    p.y := FSubject.Y;
  end;
end;

constructor TSoBox2DColliderObj.Create(const ASubject: TSoObject; const AWorld: Tb2World; const AColliderDef: TColliderDefinition);
var
  i: Integer;
  vFixture: Tb2FixtureDef;
  vShape: Tb2Shape;
  vVec: TVector2;
begin
  inherited Create(ASubject);

  ASubject.AddChangePositionHandler(OnPositionChanged);

  FWorld := AWorld;
  FBodyDef := Tb2BodyDef.Create;
  FBodyDef.bodyType := b2_dynamicBody;

  FBody := FWorld.CreateBody(FBodyDef, False);

  for i := 0 to AColliderDef.Shape.Count - 1 do
  begin
    vFixture := Tb2FixtureDef.Create;
    vShape := TRawShapeBox2DShapeConverter.ConvertTo(AColliderDef.Shape[i]);
    vFixture.shape := vShape;
    vFixture.density := AColliderDef.Density;
    vFixture.friction := AColliderDef.Friction;
    vFixture.restitution := 1;//AColliderDef.Restitution;
    FBody.CreateFixture(vFixture);
  end;
end;

destructor TSoBox2DColliderObj.Destroy;
begin
  FBody.Free;
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

procedure TSoBox2DColliderObj.OnPositionChanged(ASender: TObject;
  APosition: TPosition);
var
  vVector: TVector2;
begin
  vVector.x := APosition.X;
  vVector.y := APosition.Y;

  FBody.SetTransform(vVector, APosition.Rotate);

end;

procedure TSoBox2DColliderObj.RefreshSubjectPosition;
begin
  inherited;
  FSubject.SetPositionSilent(FBody.GetPosition.x, FBody.GetPosition.y, FBody.GetAngle);
end;

end.
