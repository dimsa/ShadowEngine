unit uSoColliderWrapper;

interface
uses
  uCommonClasses, uSoColliderTypes, uColliderDefinition;

type

TSoColliderWrapper = class abstract
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
  procedure AddColliderDefinition(const AColliderDef: TColliderDefinition); virtual; abstract;

  constructor Create;
  destructor Destroy; override;
end;

implementation

{ TSoColliderWrapper }

procedure TSoColliderWrapper.AddOnBeginContactHandler(AEventHandler: TEvent<TPairCollidedEventArgs>);
begin
  FOnBeginContact.Add(AEventHandler);
end;

procedure TSoColliderWrapper.AddOnEndContactHandler(AEventHandler: TEvent<TPairCollidedEventArgs>);
begin
  FOnEndContact.Add(AEventHandler);
end;

constructor TSoColliderWrapper.Create;
begin
  FOnBeginContact := TEventList<TPairCollidedEventArgs>.Create;
  FOnEndContact := TEventList<TPairCollidedEventArgs>.Create;
end;

destructor TSoColliderWrapper.Destroy;
begin
  FOnBeginContact.Free;
  FOnEndContact.Free;
  inherited;
end;

procedure TSoColliderWrapper.RaiseOnBeginContact(
  const AEventArgs: TPairCollidedEventArgs);
begin
  FOnBeginContact.RaiseEvent(Self, AEventArgs);
end;

procedure TSoColliderWrapper.RaiseOnEndContact(
  const AEventArgs: TPairCollidedEventArgs);
begin
  FOnEndContact.RaiseEvent(Self, AEventArgs);
end;

procedure TSoColliderWrapper.RemoveOnBeginContactHandler(AEventHandler: TEvent<TPairCollidedEventArgs>);
begin
  FOnBeginContact.Remove(AEventHandler);
end;

procedure TSoColliderWrapper.RemoveOnEndContactHandler(AEventHandler: TEvent<TPairCollidedEventArgs>);
begin
  FOnEndContact.Remove(AEventHandler);
end;

end.
