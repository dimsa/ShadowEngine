unit uSoCollider;

interface

uses
  System.SysUtils, System.SyncObjs, System.JSON,
  uSoColliderObject, uSoBaseOperator, uSoObject, uSoContainerTypes, uSoBasePart,
  UPhysics2D, uSoBox2DListener, UPhysics2DTypes;

type
  TSoCollider = class(TSoOperator<TSoColliderObj>)
  private
    FWorld: Tb2World;
    FContactListener: TSoBox2DListener;
    procedure OnItemDestroy(ASender: TObject);
    procedure InitilizeBox2D;
  public
    procedure Execute; // Test for collide on tick
    function Contains(const AX, AY: Single): TArray<TSoObject>;
    procedure Add(const AItem: TSoColliderObj; const AName: string = ''); override;
    procedure LoadTemplateFromSeJson(const AFilename: string);
    procedure AddTemplateFromJson(const AJson: TJsonObject);
  end;

implementation

{ TSoCollider }

procedure TSoCollider.Add(const AItem: TSoColliderObj; const AName: string);
var
  vName: string;
begin
  {$I .\Template\uItemAdd.inc}
end;

procedure TSoCollider.AddTemplateFromJson(const AJson: TJsonObject);
begin

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

procedure TSoCollider.LoadTemplateFromSeJson(const AFilename: string);
begin

end;

procedure TSoCollider.OnItemDestroy(ASender: TObject);
begin
  FList.Delete(TSoColliderObj(ASender));
end;

end.
