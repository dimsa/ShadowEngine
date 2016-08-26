// This class contains Logic that is maked by Unit in One Tick.
unit uSoLogicKeeper;

interface

uses
  System.SysUtils, System.SyncObjs,
  uSoLogic, uSoBaseOperator;

type
  TSoLogicFriend = class(TSoLogic);

  TSoLogicKeeper = class(TSoOperator<TSoLogicFriend>)
  private
    FAddedObjects: Integer;
    procedure OnItemDestroy(ASender: TObject);
  public
    procedure Execute; // Do some logic on tick
    procedure Add(const AItem: TSoLogicFriend; const AName: string = ''); override;
    constructor Create(const ACritical: TCriticalSection); override;
  end;


implementation

{ TSoLogicKeeper }

procedure TSoLogicKeeper.Add(const AItem: TSoLogicFriend; const AName: string);
var
  vName: string;
begin
  {$I .\Template\uItemAdd.inc}
end;

constructor TSoLogicKeeper.Create(const ACritical: TCriticalSection);
begin
  inherited;
  FAddedObjects := 0;
end;

procedure TSoLogicKeeper.Execute;
var
  i: Integer;
begin
  inherited;
  for i := 0 to FList.Count - 1 do
    FList[i].Execute;
end;

procedure TSoLogicKeeper.OnItemDestroy(ASender: TObject);
begin
  FList.Delete(TSoLogicFriend(ASender));
end;

end.
