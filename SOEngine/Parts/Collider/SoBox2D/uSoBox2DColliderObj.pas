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
    FLastScale: TPointF;
    FOriginalShapes: TList<Tb2Shape>;
    function B2TransformFromSubject: Tb2Transform;
    procedure OnPositionChanged(ASender: TObject; APosition: TPosition);
    function BodyTypeToBox2DBodyType(const ABodyType: TBodyType): Tb2BodyType;
    procedure Scale(const AScale: TPointF);
  protected
    property Body: Tb2Body read FBody;
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

function TSoBox2DColliderObj.BodyTypeToBox2DBodyType(const ABodyType: TBodyType): Tb2BodyType;
begin
  case ABodyType of
    btStatic: Exit(Tb2BodyType.b2_staticBody);
    btKinematic: Exit(Tb2BodyType.b2_kinematicBody);
    btDynamic: Exit(Tb2BodyType.b2_dynamicBody);
  end;

  raise Exception.Create('Not supported body type');
end;

constructor TSoBox2DColliderObj.Create(const ASubject: TSoObject; const AWorld: Tb2World; const AColliderDef: TColliderDefinition);
var
  i: Integer;
  vFixture: Tb2FixtureDef;
  vShape: Tb2Shape;
  vVec: TVector2;
begin
  inherited Create(ASubject);

  FOriginalShapes := TList<Tb2Shape>.Create;
  FLastScale := ASubject.ScalePoint;

  ASubject.AddChangePositionHandler(OnPositionChanged);

  FWorld := AWorld;
  FBodyDef := Tb2BodyDef.Create;
  FBodyDef.bodyType := BodyTypeToBox2DBodyType(AColliderDef.BodyType);

  FBody := FWorld.CreateBody(FBodyDef, False);

  for i := 0 to AColliderDef.Shape.Count - 1 do
  begin
    vFixture := Tb2FixtureDef.Create;
    vShape := TRawShapeBox2DShapeConverter.ConvertTo(AColliderDef.Shape[i]);
    vFixture.shape := vShape;
    vFixture.density := AColliderDef.Density;
    vFixture.friction := AColliderDef.Friction;
    vFixture.restitution := AColliderDef.Restitution;
    vFixture.isSensor := AColliderDef.IsSensor;
    FBody.CreateFixture(vFixture);
    FOriginalShapes.Add(TRawShapeBox2DShapeConverter.ConvertTo(AColliderDef.Shape[i]))
  end;
  FOriginalShapes.Reverse;
end;

destructor TSoBox2DColliderObj.Destroy;
var
  i: Integer;
begin
  FBody.Free;
  for i := 0 to FOriginalShapes.Count - 1 do
    FOriginalShapes[i].Free;
  FOriginalShapes.Free;
  inherited;
end;

function TSoBox2DColliderObj.IsContainsPoint(const AX, AY: Single): Boolean;
begin
  Result := IsContainsPoint(TPointF.Create(AX, AY));
end;

function TSoBox2DColliderObj.IsContainsPoint(const APoint: TPointF): Boolean;
var
  vFix: Tb2Fixture;
begin
  vFix := FBody.GetFixtureList;

  while vFix <> nil do begin
    if vFix.GetShape.TestPoint(B2TransformFromSubject, Vector2FromPoint(APoint)) then
      Exit(True);
    vFix := vFix.GetNext;
  end;

  Result := False;
end;

procedure TSoBox2DColliderObj.OnPositionChanged(ASender: TObject;
  APosition: TPosition);
var
  vVector: TVector2;
begin
  vVector.x := APosition.X;
  vVector.y := APosition.Y;

  FBody.SetTransform(vVector, APosition.Rotate);
  Scale(APosition.Scale);

end;

procedure TSoBox2DColliderObj.RefreshSubjectPosition;
begin
  inherited;
  FSubject.SetPositionSilent(FBody.GetPosition.x, FBody.GetPosition.y, FBody.GetAngle);
end;

procedure TSoBox2DColliderObj.Scale(const AScale: TPointF);
var
  vFix: Tb2Fixture;
  i, j: Integer;
begin
  if AScale = FLastScale then
    Exit;

  FLastScale := AScale;

  vFix := FBody.GetFixtureList;
  i := 0;
  while vFix <> nil do begin
    case vFix.GetShape.GetType of
      e_circleShape:
      begin
        Tb2CircleShape(vFix.GetShape).m_radius := FOriginalShapes[i].m_radius * AScale.X;
        Tb2CircleShape(vFix.GetShape).m_p.x := Tb2CircleShape(FOriginalShapes[i]).m_p.x * AScale.X;
        Tb2CircleShape(vFix.GetShape).m_p.y := Tb2CircleShape(FOriginalShapes[i]).m_p.y * AScale.Y;
      end;
      e_polygonShape: begin
        Tb2PolygonShape(vFix.GetShape).m_centroid :=
          Vector2FromPoint(
            Tb2PolygonShape(FOriginalShapes[i]).m_centroid.x * AScale.X,
            Tb2PolygonShape(FOriginalShapes[i]).m_centroid.y * AScale.Y
          );
        for j := 0 to Tb2PolygonShape(vFix.GetShape).m_count - 1 do
        begin
          Tb2PolygonShape(vFix.GetShape).m_vertices[j].x := Tb2PolygonShape(FOriginalShapes[i]).m_vertices[j].x * AScale.X;
          Tb2PolygonShape(vFix.GetShape).m_vertices[j].y := Tb2PolygonShape(FOriginalShapes[i]).m_vertices[j].y * AScale.Y;
        end;
      end;
      e_edgeShape, e_chainShape: raise Exception.Create('Can not scale this type');
    end;

    vFix := vFix.GetNext;
    i := i + 1;
  end;
end;

end.
