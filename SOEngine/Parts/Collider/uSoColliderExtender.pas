unit uSoColliderExtender;

interface
uses
  uCommonClasses, uSoColliderTypes, uColliderDefinition, uSoColliderObject, uSoObject;

type

TSoColliderExtender = class abstract
private
  FOnEndContact: TEvent<TPairCollidedEventArgs>;
  FOnBeginContact: TEvent<TPairCollidedEventArgs>;
protected
  procedure RaiseOnBeginContact(const AEventArgs: TPairCollidedEventArgs);
  procedure RaiseOnEndContact(const AEventArgs: TPairCollidedEventArgs);
public
  property OnEndContact: TEvent<TPairCollidedEventArgs> read FOnEndContact write FOnEndContact;
  property OnBeginContact: TEvent<TPairCollidedEventArgs> read FOnBeginContact write FOnBeginContact;

  procedure ProcessStep; virtual; abstract;
  function ProduceColliderObj(const ASubject: TSoObject; const AColliderDef: TColliderDefinition): TSoColliderObj; virtual; abstract;

  destructor Destroy; override;
end;

implementation

{ TSoColliderWrapper }

destructor TSoColliderExtender.Destroy;
begin
  FOnEndContact := nil;
  FOnBeginContact := nil;
  inherited;
end;

procedure TSoColliderExtender.RaiseOnBeginContact(
  const AEventArgs: TPairCollidedEventArgs);
begin
  if Assigned(FOnEndContact) then
    FOnBeginContact(Self, AEventArgs);
end;

procedure TSoColliderExtender.RaiseOnEndContact(
  const AEventArgs: TPairCollidedEventArgs);
begin
  if Assigned(FOnBeginContact) then
    FOnBeginContact(Self, AEventArgs);
end;


end.
