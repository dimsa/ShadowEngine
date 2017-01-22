// This class contains Logic that is maked by Unit in One Tick.
unit uSoLogicKeeper;

interface

uses
  System.SysUtils,
  uSoTypes, uSoLogic, uSoBaseOperator, uSoObject, uSoContainerTypes, uSoBasePart;

type
  TSoLogicFriend = class(TSoLogic);

  TSoLogicKeeper = class(TSoOperator<TSoLogic>)
  private
    procedure OnItemDestroy(ASender: TObject);
  public
    procedure Execute; // Do some logic on tick
    procedure Add(const AItem: TSoLogic); override;
    function AddFromTemplate(const ASubject: TSoObject; const ATemplateName: string): TSoLogic; override;
    constructor Create(const ACritical: TCriticalSection); override;
  end;

implementation

{ TSoLogicKeeper }

procedure TSoLogicKeeper.Add(const AItem: TSoLogic);
var
  vName: string;
begin
  {$I .\SoObject\uItemAdd.inc}
end;

function TSoLogicKeeper.AddFromTemplate(const ASubject: TSoObject; const ATemplateName: string): TSoLogic;
begin

end;

constructor TSoLogicKeeper.Create(const ACritical: TCriticalSection);
begin
  inherited;

end;

procedure TSoLogicKeeper.Execute;
var
  i: Integer;
  vItem: TSoLogic;
begin
  inherited;
  for i := 0 to FList.Count - 1 do
    if FList.TryGetValue(i, TObject(vItem)) then
      if vItem.Enabled then
        TSoLogicFriend(vItem).Execute;
end;

procedure TSoLogicKeeper.OnItemDestroy(ASender: TObject);
begin
  FList.Remove(TSoLogic(ASender));
end;

end.
