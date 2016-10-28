unit uSoBox2DWrapper;

interface

uses
  UPhysics2D, UPhysics2DTypes,
  uCommonClasses, uSoTypes, uSoObject, uSoBox2DListener, uSoColliderTypes,
  uSoColliderWrapper, uSoColliderOptions;

type
  TSoBox2DWrapper = class(TSoColliderWrapper)
  private
    FBox2DWorld: Tb2World;
    FContactListener: TSoBox2DContactListener;
    FFixtureToObjectReferenceDict: TDict<Tb2Fixture, TSoObject>;
    FOptions: TSoColliderOptions;
    function WorldEventArgsFromContact(const AContact: Tb2Contact): TPairCollidedEventArgs;
    function ObjectEventArgsFromContact(const AContact: Tb2Contact): TObjectCollidedEventArgs;
    procedure OnBeginContactHandler(AContact: Tb2Contact);
    procedure OnEndContactHandler(AContact: Tb2Contact);
  public
    procedure ProcessStep; override;

    constructor Create(const AOptions: TSoColliderOptions);
    destructor Destroy; override;
  end;

implementation

{ TSoBox2DWrapper }

constructor TSoBox2DWrapper.Create(const AOptions: TSoColliderOptions);
var
  vGravVector: TVector2;
  vTest: Tb2PolygonShape;
begin

  FFixtureToObjectReferenceDict := TDict<Tb2Fixture, TSoObject>.Create;

  // Initialization of Box2D
  vGravVector.x := AOptions.Gravity.X;
  vGravVector.y := AOptions.Gravity.Y;
  FBox2DWorld := Tb2World.Create(vGravVector);

  FContactListener := TSoBox2DContactListener.Create;
  FContactListener.OnBeginContact := OnBeginContactHandler;
  FContactListener.OnEndContact := OnEndContactHandler;
  FBox2DWorld.SetContactListener(FContactListener);
end;

destructor TSoBox2DWrapper.Destroy;
begin
  FBox2DWorld.Free;
  FContactListener.Free;

  FFixtureToObjectReferenceDict.Free;

  inherited;
end;

function TSoBox2DWrapper.WorldEventArgsFromContact(
  const AContact: Tb2Contact): TPairCollidedEventArgs;
begin
  with Result do begin
    Friction := AContact.m_friction;
    Restitution := AContact.m_restitution;
    TangentSpeed := AContact.m_tangentSpeed;
    ObjectA := FFixtureToObjectReferenceDict[AContact.m_fixtureA];
    ObjectB := FFixtureToObjectReferenceDict[AContact.m_fixtureB];
  end;
end;

function TSoBox2DWrapper.ObjectEventArgsFromContact(
  const AContact: Tb2Contact): TObjectCollidedEventArgs;
begin
  with Result do begin
    Friction := AContact.m_friction;
    Restitution := AContact.m_restitution;
    TangentSpeed := AContact.m_tangentSpeed;
    Opponent := FFixtureToObjectReferenceDict[AContact.m_fixtureB];
  end;
end;

procedure TSoBox2DWrapper.OnBeginContactHandler(AContact: Tb2Contact);
begin
  if FOptions.NeedToGenerateWorldEvents then
    RaiseOnBeginContact(WorldEventArgsFromContact(AContact));
end;

procedure TSoBox2DWrapper.OnEndContactHandler(AContact: Tb2Contact);
begin
  if FOptions.NeedToGenerateWorldEvents then
    RaiseOnEndContact(WorldEventArgsFromContact(AContact));
end;

procedure TSoBox2DWrapper.ProcessStep;
begin
  FBox2DWorld.Step(1, 1, 1);
end;

end.
