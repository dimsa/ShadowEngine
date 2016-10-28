unit uSoColliderObject;

interface

uses
  uCommonClasses, uSoTypes, uSoBasePart, uSoColliderObjectTypes, uSoObject;

type
  TSoColliderObj = class abstract(TSoBasePart)
  private
    FOnEndContact: TEventList<TObjectCollidedEventArgs>;
    FOnBeginContact: TEventList<TObjectCollidedEventArgs>;
    procedure SetOnExecute(const Value: TNotifyEvent<TSoObject>);
  protected
    procedure RaiseOnBeginContact(const ACollideArgs: TObjectCollidedEventArgs);
    procedure RaiseOnEndContact(const ACollideArgs: TObjectCollidedEventArgs);
  public
    procedure AddOnBeginContactHandler(AEventHandler: TEvent<TObjectCollidedEventArgs>);
    procedure RemoveOnBeginContactHandler(AEventHandler: TEvent<TObjectCollidedEventArgs>);
    procedure AddOnEndContactHandler(AEventHandler: TEvent<TObjectCollidedEventArgs>);
    procedure RemoveOnEndContactHandler(AEventHandler: TEvent<TObjectCollidedEventArgs>);
    //property OnCollide: TEvent<TSoObject> read FOnCollide write FOnCollide;
  //  property OnBeginContact: TEventList<TObjectCollidedEventArgs> read FOnBeginContact;
 //   property OnEndContact: TEventList<TObjectCollidedEventArgs> read FOnEndContact;
    function IsContainsPoint(const AX, AY: Single): Boolean; overload; virtual; abstract;
    function IsContainsPoint(const APoint: TPointF): Boolean; overload; virtual; abstract;
    constructor Create(const ASubject: TSoObject); override;
    destructor Destroy; override;
  end;

implementation

{ TSoColliderObj }

procedure TSoColliderObj.AddOnBeginContactHandler(
  AEventHandler: TEvent<TObjectCollidedEventArgs>);
begin
  FOnBeginContact.Add(AEventHandler);
end;

procedure TSoColliderObj.AddOnEndContactHandler(
  AEventHandler: TEvent<TObjectCollidedEventArgs>);
begin
  FOnEndContact.Add(AEventHandler);
end;

constructor TSoColliderObj.Create(const ASubject: TSoObject);
begin
  inherited Create(ASubject);

  FOnBeginContact := TEventList<TObjectCollidedEventArgs>.Create;
  FOnEndContact := TEventList<TObjectCollidedEventArgs>.Create;
end;

destructor TSoColliderObj.Destroy;
begin
  FOnBeginContact.Free;
  FOnEndContact.Free;
  inherited;
end;

procedure TSoColliderObj.RaiseOnBeginContact(
  const ACollideArgs: TObjectCollidedEventArgs);
begin
  FOnBeginContact.RaiseEvent(Self, ACollideArgs);
end;

procedure TSoColliderObj.RaiseOnEndContact(
  const ACollideArgs: TObjectCollidedEventArgs);
begin
  FOnEndContact.RaiseEvent(Self, ACollideArgs);
end;

procedure TSoColliderObj.RemoveOnBeginContactHandler(
  AEventHandler: TEvent<TObjectCollidedEventArgs>);
begin
  FOnBeginContact.Add(AEventHandler);
end;

procedure TSoColliderObj.RemoveOnEndContactHandler(
  AEventHandler: TEvent<TObjectCollidedEventArgs>);
begin
  FOnEndContact.Add(AEventHandler);
end;

procedure TSoColliderObj.SetOnExecute(const Value: TNotifyEvent<TSoObject>);
begin

end;

end.

