unit uSoBox2DExtender;

interface

uses
  UPhysics2D, UPhysics2DTypes,
  uCommonClasses, uSoTypes, uSoBox2DListener, uSoColliderTypes, uSoColliderObjectTypes,
  uSoColliderExtender, uSoColliderOptions, uSoColliderObject, uColliderDefinition, uSoBox2DColliderObj,
  uSoObject;

type
  TSoBox2DExtender = class(TSoColliderExtender)
  private type
    TSoColliderObjFriend = class(TSoColliderObj);
    TSoBox2DColliderObjFriend = class(TSoBox2DColliderObj);
  private
    FBox2DWorld: Tb2World;
    FContactListener: TSoBox2DContactListener;
    FFixtureToColliderObjReferenceDict: TDict<Tb2Body, TSoColliderObj>;
    FOptions: TSoColliderOptions;
    function WorldEventArgsFromContact(const AContact: Tb2Contact): TPairCollidedEventArgs;
    function ObjectEventArgsFromContact(const AContact: Tb2Contact; const AOpponent: Tb2Body): TObjectCollidedEventArgs;
    procedure OnBeginContactHandler(AContact: Tb2Contact);
    procedure OnEndContactHandler(AContact: Tb2Contact);
  public
    function ProduceColliderObj(const ASubject: TSoObject; const AColliderDef: TColliderDefinition): TSoColliderObj; override;
    procedure ProcessStep; override;

    constructor Create(const AOptions: TSoColliderOptions);
    destructor Destroy; override;
  end;

implementation

{ TSoBox2DWrapper }

function TSoBox2DExtender.ProduceColliderObj(const ASubject: TSoObject; const AColliderDef: TColliderDefinition): TSoColliderObj;
begin
  Result := TSoBox2DColliderObj.Create(ASubject, FBox2DWorld, AColliderDef);
  FFixtureToColliderObjReferenceDict.Add(TSoBox2DColliderObjFriend(Result).Body, Result);
end;

constructor TSoBox2DExtender.Create(const AOptions: TSoColliderOptions);
var
  vGravVector: TVector2;
begin
  FOptions := AOptions;
  FFixtureToColliderObjReferenceDict := TDict<Tb2Body, TSoColliderObj>.Create;

  // Initialization of Box2D
  vGravVector.x := FOptions.Gravity.X;// + random * 10;
  vGravVector.y := FOptions.Gravity.Y;// + random * 10;
  FBox2DWorld := Tb2World.Create(vGravVector);

  FContactListener := TSoBox2DContactListener.Create;
  FContactListener.OnBeginContact := OnBeginContactHandler;
  FContactListener.OnEndContact := OnEndContactHandler;
  FBox2DWorld.SetContactListener(FContactListener);
end;

destructor TSoBox2DExtender.Destroy;
begin
  FBox2DWorld.Free;
  FContactListener.Free;

  FFixtureToColliderObjReferenceDict.Free;

  inherited;
end;

function TSoBox2DExtender.WorldEventArgsFromContact(
  const AContact: Tb2Contact): TPairCollidedEventArgs;
begin
  with Result do begin
    Friction := AContact.m_friction;
    Restitution := AContact.m_restitution;
    TangentSpeed := AContact.m_tangentSpeed;
    ObjectA := FFixtureToColliderObjReferenceDict[AContact.m_fixtureA.GetBody];
    ObjectB := FFixtureToColliderObjReferenceDict[AContact.m_fixtureB.GetBody];
  end;
end;

function TSoBox2DExtender.ObjectEventArgsFromContact(
  const AContact: Tb2Contact; const AOpponent: Tb2Body): TObjectCollidedEventArgs;
begin
  with Result do begin
    Friction := AContact.m_friction;
    Restitution := AContact.m_restitution;
    TangentSpeed := AContact.m_tangentSpeed;
    Opponent := FFixtureToColliderObjReferenceDict[AOpponent].Subject;
  end;
end;

procedure TSoBox2DExtender.OnBeginContactHandler(AContact: Tb2Contact);
begin
  if FOptions.NeedToGenerateWorldEvents then
    RaiseOnBeginContact(WorldEventArgsFromContact(AContact));

  if FOptions.NeedToGenerateObjectsEvents then
  with AContact do
  begin
    TSoColliderObjFriend(
      FFixtureToColliderObjReferenceDict[AContact.m_fixtureA.GetBody]).
        RaiseOnBeginContact(ObjectEventArgsFromContact(AContact, AContact.m_fixtureB.GetBody));

    TSoColliderObjFriend(
      FFixtureToColliderObjReferenceDict[AContact.m_fixtureB.GetBody]).
        RaiseOnBeginContact(ObjectEventArgsFromContact(AContact, AContact.m_fixtureA.GetBody));
  end;
end;

procedure TSoBox2DExtender.OnEndContactHandler(AContact: Tb2Contact);
begin
  if FOptions.NeedToGenerateWorldEvents then
    RaiseOnEndContact(WorldEventArgsFromContact(AContact));

  if FOptions.NeedToGenerateObjectsEvents then
  with AContact do
  begin
    TSoColliderObjFriend(
      FFixtureToColliderObjReferenceDict[AContact.m_fixtureA.GetBody]).
        RaiseOnEndContact(ObjectEventArgsFromContact(AContact, AContact.m_fixtureB.GetBody));

    TSoColliderObjFriend(
      FFixtureToColliderObjReferenceDict[AContact.m_fixtureB.GetBody]).
        RaiseOnEndContact(ObjectEventArgsFromContact(AContact, AContact.m_fixtureA.GetBody));
  end;
end;

procedure TSoBox2DExtender.ProcessStep;
begin
  FBox2DWorld.Step(1, 1, 1);
end;

end.
