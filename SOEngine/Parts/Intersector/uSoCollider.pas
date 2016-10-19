unit uSoCollider;

interface

uses
  UPhysics2DTypes, System.SysUtils, System.JSON,
  uSoTypes, uSoColliderObject, uSoBaseOperator, uSoObject, uSoContainerTypes, uSoBasePart,
  UPhysics2D, uSoBox2DListener, uSoColliderTemplate;

type
  TSoCollider = class(TSoOperator<TSoColliderObj>)
  private
    FTemplates: TDict<string, TSoColliderTemplate>;
    FColliderObjBySubject: TDict<TSoObject, TList<TSoColliderObj>>;
    FWorld: Tb2World;
    FContactListener: TSoBox2DListener;
    procedure OnItemDestroy(ASender: TObject);
    procedure InitilizeBox2D;
  public
    procedure Execute; // Test for collide on tick
    function Contains(const AX, AY: Single): TArray<TSoObject>;
    procedure Add(const AItem: TSoColliderObj; const AName: string = ''); override;
    procedure AddTemplateFromJson(const AJson: TJsonValue);
    constructor Create(const ACritical: TCriticalSection); override;
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

constructor TSoCollider.Create(const ACritical: TCriticalSection);
begin
  inherited;

  FTemplates := TDict<string, TSoColliderTemplate>.Create;
  FColliderObjBySubject := TDict<TSoObject, TList<TSoColliderObj>>.Create;
end;

destructor TSoCollider.Destroy;
var
  vTemp: TSoColliderTemplate;
begin
  for vTemp  in FTemplates.Values do
    vTemp.Free;
  FTemplates.Free;

  FColliderObjBySubject.Free;

  FWorld.Free;
  FContactListener.Free;

  inherited;
end;

procedure TSoCollider.Execute;
begin
  inherited;
  { TODO : Add method for colliding }
end;

procedure TSoCollider.InitilizeBox2D;
begin
   if Assigned(FWorld) then
      FWorld.Free; // box2D只需销毁world即可，其中的物体也会被销毁

   FContactListener := TSoBox2DListener.Create(nil);
   FWorld := Tb2World.Create(b2Vec2_Zero);
   FWorld.SetContactListener(FContactListener);
end;

procedure TSoCollider.OnItemDestroy(ASender: TObject);
begin
  FList.Delete(TSoColliderObj(ASender));
end;

end.
