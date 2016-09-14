unit uSoCollider;

interface

uses
  System.SysUtils, System.SyncObjs,
  uSoColliderObject, uSoBaseOperator, uSoObject, uSoContainerTypes, uSoBasePart;

type
  TSoCollider = class(TSoOperator<TSoColliderObj>)
  private
    procedure OnItemDestroy(ASender: TObject);
  public
    procedure Execute; // Test for collide on tick
    function Contains(const AX, AY: Single): TArray<TSoObject>;
    procedure Add(const AItem: TSoColliderObj; const AName: string = ''); override;
    procedure LoadTemplateFromSeJson(const AFilename: string);
  end;

implementation

{ TSoCollider }

procedure TSoCollider.Add(const AItem: TSoColliderObj; const AName: string);
var
  vName: string;
begin
  {$I .\Template\uItemAdd.inc}
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

procedure TSoCollider.LoadTemplateFromSeJson(const AFilename: string);
begin

end;

procedure TSoCollider.OnItemDestroy(ASender: TObject);
begin
  FList.Delete(TSoColliderObj(ASender));
end;

end.
