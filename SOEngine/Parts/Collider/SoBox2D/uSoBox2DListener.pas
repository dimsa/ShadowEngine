unit uSoBox2DListener;

interface

uses
  UPhysics2D,
  uSoTypes, uCommonClasses, uSoColliderTypes, uSoObject;

type
  // Use Box2D code for comments.
  TSoBox2DListener = class(Tb2ContactListener)
  private
    FFixtureToObjectReferenceDict: TDict<Tb2Fixture, TSoObject>;
    FOnEndContact: TEventList<TCollideEventArgs>;
    FOnBeginContact: TEventList<TCollideEventArgs>;
  public
    procedure BeginContact(var contact: Tb2Contact); override;
    procedure EndContact(var contact: Tb2Contact); override;
    procedure PreSolve(var contact: Tb2Contact; const oldManifold: Tb2Manifold); override;
    procedure PostSolve(var contact: Tb2Contact; const impulse: Tb2ContactImpulse); override;

    property OnBeginContact: TEventList<TCollideEventArgs> read FOnBeginContact;
    property OnEndContact: TEventList<TCollideEventArgs> read FOnEndContact;
    constructor Create(AFixtureToObjectReferenceDict: TDict<Tb2Fixture, TSoObject>);
    destructor Destroy; override;
  end;

implementation

{ TSoBox2DListener }

procedure TSoBox2DListener.BeginContact(var contact: Tb2Contact);
begin

end;

constructor TSoBox2DListener.Create(AFixtureToObjectReferenceDict: TDict<Tb2Fixture, TSoObject>);
begin
  FFixtureToObjectReferenceDict := AFixtureToObjectReferenceDict;
  FOnBeginContact := TEventList<TCollideEventArgs>.Create;
  FOnEndContact := TEventList<TCollideEventArgs>.Create;
end;

destructor TSoBox2DListener.Destroy;
begin
  FOnEndContact.Free;
  FOnBeginContact.Free;
  inherited;
end;

procedure TSoBox2DListener.EndContact(var contact: Tb2Contact);
begin

end;

procedure TSoBox2DListener.PostSolve(var contact: Tb2Contact;
  const impulse: Tb2ContactImpulse);
begin
  // For now I don't know how to use it. Time will tell
end;

procedure TSoBox2DListener.PreSolve(var contact: Tb2Contact;
  const oldManifold: Tb2Manifold);
begin
  // For now I don't know how to use it. Time will tell
end;

end.

