unit uSoEngineOptions;

interface
uses
  uSoTypes,
  uSoColliderOptions;

type
  TSoEngineOptions = class
  private
    FColliderOptions: TSoColliderOptions;
    FOnOptionsChanged: TNotifyEvent;
    procedure RaiseOptionsChanged;
    procedure SetCollideOptions(const Value: TSoColliderOptions);
  public
    property OnOptionsChanged: TNotifyEvent read FOnOptionsChanged write FOnOptionsChanged;
    property ColliderOptions: TSoColliderOptions read FColliderOptions write SetCollideOptions;
    constructor Create;
    destructor Destroy; override;
  end;

implementation

{ TSoEngineConfigOptions }

constructor TSoEngineOptions.Create;
begin
  FColliderOptions := TSoColliderOptions.Create;
end;

destructor TSoEngineOptions.Destroy;
begin
  FOnOptionsChanged := nil;
  inherited;
end;

procedure TSoEngineOptions.RaiseOptionsChanged;
begin
  if Assigned(FOnOptionsChanged) then
    FOnOptionsChanged(Self);
end;

procedure TSoEngineOptions.SetCollideOptions(
  const Value: TSoColliderOptions);
begin
  FColliderOptions := Value;
  RaiseOptionsChanged;
end;

end.
