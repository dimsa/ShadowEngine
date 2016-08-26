unit uSoCollider;

interface

uses
  System.SysUtils, System.SyncObjs,
  uSoColliderObject, uSoBaseOperator;

type
  TSoCollider = class(TSoOperator<TSoColliderObj>)
  private
    procedure OnItemDestroy(ASender: TObject);
  public
    procedure Execute; // Test for collide on tick
    procedure Add(const AItem: TSoColliderObj; const AName: string = ''); override;
  end;

implementation

{ TSoCollider }

procedure TSoCollider.Add(const AItem: TSoColliderObj; const AName: string);
var
  vName: string;
begin
  {$I .\Template\uItemAdd.inc}
end;

procedure TSoCollider.Execute;
begin
  inherited;
  { TODO : Add method for colliding }
end;

procedure TSoCollider.OnItemDestroy(ASender: TObject);
begin
  FList.Delete(TSoColliderObj(ASender));
end;

end.
