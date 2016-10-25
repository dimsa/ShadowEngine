unit uSoBox2DWrapper;

interface

uses
  UPhysics2D, UPhysics2DTypes,
  uCommonClasses, uSoTypes, uSoObject, uSoBox2DListener, uSoColliderTypes;

type
  TSoBox2DWrapper = class
  private
    FBox2DWorld: Tb2World;
    FContactListener: TSoBox2DContactListener;
    FFixtureToObjectReferenceDict: TDict<Tb2Fixture, TSoObject>;
    FOnEndContact: TEventList<TCollideEventArgs>;
    FOnBeginContact: TEventList<TCollideEventArgs>;
    function EventArgsFromContact(const AContact: Tb2Contact): TCollideEventArgs;
    procedure OnBeginContactHandler(AContact: Tb2Contact);
    procedure OnEndContactHandler(AContact: Tb2Contact);
  public
    property OnBeginContact: TEventList<TCollideEventArgs> read FOnBeginContact;
    property OnEndContact: TEventList<TCollideEventArgs> read FOnEndContact;

    constructor Create(const AGravity: TPointF);
    destructor Destroy; override;
  end;

implementation

{ TSoBox2DWrapper }

constructor TSoBox2DWrapper.Create(const AGravity: TPointF);
var
  vGravVector: TVector2;
begin
  FOnBeginContact := TEventList<TCollideEventArgs>.Create;
  FOnEndContact := TEventList<TCollideEventArgs>.Create;

  FFixtureToObjectReferenceDict := TDict<Tb2Fixture, TSoObject>.Create;

  // Initialization of Box2D
  vGravVector.x := AGravity.X;
  vGravVector.y := AGravity.Y;
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

  FOnEndContact.Free;
  FOnBeginContact.Free;
  FFixtureToObjectReferenceDict.Free;

  inherited;
end;

function TSoBox2DWrapper.EventArgsFromContact(
  const AContact: Tb2Contact): TCollideEventArgs;
begin
  with Result do begin
    Friction := AContact.m_friction;
    Restitution := AContact.m_restitution;
    TangentSpeed := AContact.m_tangentSpeed;
    ObjectA := FFixtureToObjectReferenceDict[AContact.m_fixtureA];
    ObjectB := FFixtureToObjectReferenceDict[AContact.m_fixtureB];
  end;
end;

procedure TSoBox2DWrapper.OnBeginContactHandler(AContact: Tb2Contact);
begin
  FOnBeginContact.RaiseEvent(Self, EventArgsFromContact(AContact));
end;

procedure TSoBox2DWrapper.OnEndContactHandler(AContact: Tb2Contact);
begin
  FOnEndContact.RaiseEvent(Self, EventArgsFromContact(AContact));
end;

end.
