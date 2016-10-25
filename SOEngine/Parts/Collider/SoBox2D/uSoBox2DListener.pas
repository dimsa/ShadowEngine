unit uSoBox2DListener;

interface

uses
  UPhysics2D,
  uCommonClasses;

type
  // Look Box2D code for comments.
  TSoBox2DContactListener = class(Tb2ContactListener)
  private
    FOnBeginContact: TNotifyEvent<Tb2Contact>;
    FOnEndContact: TNotifyEvent<Tb2Contact>;
    procedure BeginContact(var contact: Tb2Contact); override;
    procedure EndContact(var contact: Tb2Contact); override;
    procedure PreSolve(var contact: Tb2Contact; const oldManifold: Tb2Manifold); override;
    procedure PostSolve(var contact: Tb2Contact; const impulse: Tb2ContactImpulse); override;
  public
    property OnBeginContact: TNotifyEvent<Tb2Contact> read FOnBeginContact write FOnBeginContact;
    property OnEndContact: TNotifyEvent<Tb2Contact> read FOnEndContact write FOnEndContact;

    constructor Create;
    destructor Destroy; override;
  end;

implementation

{ TSoBox2DListener }

procedure TSoBox2DContactListener.BeginContact(var contact: Tb2Contact);
begin
  if Assigned(FOnBeginContact) then
    FOnBeginContact(contact);
end;

constructor TSoBox2DContactListener.Create;
begin

end;

destructor TSoBox2DContactListener.Destroy;
begin

  inherited;
end;

procedure TSoBox2DContactListener.EndContact(var contact: Tb2Contact);
begin
  if Assigned(FOnEndContact) then
    FOnEndContact(contact);
end;

procedure TSoBox2DContactListener.PostSolve(var contact: Tb2Contact;
  const impulse: Tb2ContactImpulse);
begin
  // For now I don't know how to use it. Time will tell
end;

procedure TSoBox2DContactListener.PreSolve(var contact: Tb2Contact;
  const oldManifold: Tb2Manifold);
begin
  // For now I don't know how to use it. Time will tell
end;

end.

