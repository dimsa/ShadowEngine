unit uSoCollider;

interface

uses
  System.SysUtils, System.JSON,
  uSoTypes, uCommonClasses, uSoColliderTypes, uSoColliderObject, uSoBaseOperator, uSoObject, uSoContainerTypes, uSoBasePart,
  uSoColliderTemplate, uSoColliderExtender, uColliderDefinition;

type
  TSoCollider = class abstract(TSoOperator<TSoColliderObj>)
  private
    FExtender: TSoColliderExtender;
    FTemplates: TDict<string, TSoColliderTemplate>;
    FColliderObjBySubject: TDict<TSoObject, TList<TSoColliderObj>>;
    FOnEndContact: TEventList<TPairCollidedEventArgs>;
    FOnBeginContact: TEventList<TPairCollidedEventArgs>;
    procedure OnItemDestroy(ASender: TObject);
    procedure Add(const AItem: TSoColliderObj; const AName: string = ''); overload; override;
    procedure OnBeginContact(ASender: TObject; AEventArgs: TPairCollidedEventArgs);
    procedure OnEndContact(ASender: TObject; AEventArgs: TPairCollidedEventArgs);
  public
    procedure AddOnBeginContactHandler(AEventHandler: TEvent<TPairCollidedEventArgs>);
    procedure RemoveOnBeginContactHandler(AEventHandler: TEvent<TPairCollidedEventArgs>);
    procedure AddOnEndContactHandler(AEventHandler: TEvent<TPairCollidedEventArgs>);
    procedure RemoveOnEndContactHandler(AEventHandler: TEvent<TPairCollidedEventArgs>);

    procedure Execute; // Test for collide on tick
    function Contains(const AX, AY: Single): TArray<TSoObject>;
    procedure Add(const ASubject: TSoObject; const AColliderDef: TColliderDefinition; const AName: string = ''); overload;
    procedure AddTemplateFromJson(const AJson: TJsonValue);
    constructor Create(const ACritical: TCriticalSection; const AExtender: TSoColliderExtender);
    destructor Destroy; override;
  end;

implementation

{ TSoCollider }

procedure TSoCollider.Add(const AItem: TSoColliderObj; const AName: string);
var
  vName: string;
begin
  {$I .\Template\uItemAdd.inc}
end;

procedure TSoCollider.Add(const ASubject: TSoObject; const AColliderDef: TColliderDefinition; const AName: string);
begin
  Add(FExtender.ProduceColliderObj(ASubject, AColliderDef), AName)
end;

procedure TSoCollider.AddOnBeginContactHandler(
  AEventHandler: TEvent<TPairCollidedEventArgs>);
begin
  FOnBeginContact.Add(AEventHandler);
end;

procedure TSoCollider.AddOnEndContactHandler(
  AEventHandler: TEvent<TPairCollidedEventArgs>);
begin
  FOnEndContact.Add(AEventHandler);
end;

procedure TSoCollider.AddTemplateFromJson(const AJson: TJsonValue);
var
  vBody, vName: TJSONValue;
begin
  if (AJson.TryGetValue('Name', vName)) and (AJson.TryGetValue('Body', vBody)) then
    FTemplates.Add(vName.Value, TSoColliderTemplate.Create(vBody));
end;

function TSoCollider.Contains(const AX, AY: Single): TArray<TSoObject>;
var
  i, vN: Integer;
  vRes: TArray<TSoObject>;
begin
  SetLength(vRes, FList.Count);
  vN := 0;
  for i := 0 to FList.Count - 1 do
    if FList[i].IsContainsPoint(AX, AY) then
    begin
      vRes[vN] := FList[i].Subject;
      vN := vN + 1;
    end;

  SetLength(vRes, vN);
  Result := vRes;
end;

constructor TSoCollider.Create(const ACritical: TCriticalSection; const AExtender: TSoColliderExtender);
begin
  inherited Create(ACritical);

  FOnBeginContact := TEventList<TPairCollidedEventArgs>.Create;
  FOnEndContact := TEventList<TPairCollidedEventArgs>.Create;

  FExtender := AExtender;
  FExtender.OnBeginContact := OnBeginContact;
  FExtender.OnEndContact := OnBeginContact;

  FTemplates := TDict<string, TSoColliderTemplate>.Create;
  FColliderObjBySubject := TDict<TSoObject, TList<TSoColliderObj>>.Create;
end;

destructor TSoCollider.Destroy;
var
  vTemp: TSoColliderTemplate;
begin
  for vTemp in FTemplates.Values do
    vTemp.Free;
  FTemplates.Free;

  FColliderObjBySubject.Free;

  FOnBeginContact.Free;
  FOnEndContact.Free;

  inherited;
end;

procedure TSoCollider.Execute;
begin
  inherited;
  FExtender.ProcessStep;
 end;

procedure TSoCollider.OnBeginContact(ASender: TObject; AEventArgs: TPairCollidedEventArgs);
begin
  FOnBeginContact.RaiseEvent(Self, AEventArgs);
end;

procedure TSoCollider.OnEndContact(ASender: TObject;
  AEventArgs: TPairCollidedEventArgs);
begin
  FOnEndContact.RaiseEvent(Self, AEventArgs);
end;

procedure TSoCollider.OnItemDestroy(ASender: TObject);
begin
  FList.Delete(TSoColliderObj(ASender));
end;

procedure TSoCollider.RemoveOnBeginContactHandler(
  AEventHandler: TEvent<TPairCollidedEventArgs>);
begin
  FOnBeginContact.Remove(AEventHandler);
end;

procedure TSoCollider.RemoveOnEndContactHandler(
  AEventHandler: TEvent<TPairCollidedEventArgs>);
begin
  FOnEndContact.Remove(AEventHandler);
end;

end.

