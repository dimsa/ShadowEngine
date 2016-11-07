unit uSoColliderWrapper;

interface
uses
  uCommonClasses, uSoColliderTypes, uColliderDefinition, uSoColliderObject, uSoObject;

type

TSoColliderExtender = class abstract
private
  FOnEndContact: TEventList<TPairCollidedEventArgs>;
  FOnBeginContact: TEventList<TPairCollidedEventArgs>;
protected
  procedure RaiseOnBeginContact(const AEventArgs: TPairCollidedEventArgs);
  procedure RaiseOnEndContact(const AEventArgs: TPairCollidedEventArgs);
public
  procedure AddOnBeginContactHandler(AEventHandler: TEvent<TPairCollidedEventArgs>);
  procedure RemoveOnBeginContactHandler(AEventHandler: TEvent<TPairCollidedEventArgs>);
  procedure AddOnEndContactHandler(AEventHandler: TEvent<TPairCollidedEventArgs>);
  procedure RemoveOnEndContactHandler(AEventHandler: TEvent<TPairCollidedEventArgs>);

  procedure ProcessStep; virtual; abstract;
  function ProduceColliderObj(const ASubject: TSoObject; const AColliderDef: TColliderDefinition): TSoColliderObj; virtual; abstract;

  constructor Create;
  destructor Destroy; override;
end;

implementation

{ TSoColliderWrapper }

procedure TSoColliderExtender.AddOnBeginContactHandler(AEventHandler: TEvent<TPairCollidedEventArgs>);
begin
  FOnBeginContact.Add(AEventHandler);
end;

procedure TSoColliderExtender.AddOnEndContactHandler(AEventHandler: TEvent<TPairCollidedEventArgs>);
begin
  FOnEndContact.Add(AEventHandler);
end;

constructor TSoColliderExtender.Create;
begin
  FOnBeginContact := TEventList<TPairCollidedEventArgs>.Create;
  FOnEndContact := TEventList<TPairCollidedEventArgs>.Create;
end;

destructor TSoColliderExtender.Destroy;
begin
  FOnBeginContact.Free;
  FOnEndContact.Free;
  inherited;
end;

procedure TSoColliderExtender.RaiseOnBeginContact(
  const AEventArgs: TPairCollidedEventArgs);
begin
  FOnBeginContact.RaiseEvent(Self, AEventArgs);
end;

procedure TSoColliderExtender.RaiseOnEndContact(
  const AEventArgs: TPairCollidedEventArgs);
begin
  FOnEndContact.RaiseEvent(Self, AEventArgs);
end;

procedure TSoColliderExtender.RemoveOnBeginContactHandler(AEventHandler: TEvent<TPairCollidedEventArgs>);
begin
  FOnBeginContact.Remove(AEventHandler);
end;

procedure TSoColliderExtender.RemoveOnEndContactHandler(AEventHandler: TEvent<TPairCollidedEventArgs>);
begin
  FOnEndContact.Remove(AEventHandler);
end;

end.
