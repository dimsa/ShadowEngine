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
    procedure Add(const AItem: TSoLogic; const AName: string = ''); override;
    function AddFromTemplate(const ASubject: TSoObject; const ATemplateName: string; const AName: string = ''): TSoLogic; override;
    constructor Create(const ACritical: TCriticalSection); override;
  end;

implementation

{ TSoLogicKeeper }

procedure TSoLogicKeeper.Add(const AItem: TSoLogic; const AName: string);
var
  vName: string;
begin
  {$I .\Template\uItemAdd.inc}
end;

function TSoLogicKeeper.AddFromTemplate(const ASubject: TSoObject; const ATemplateName,
  AName: string): TSoLogic;
begin

end;

constructor TSoLogicKeeper.Create(const ACritical: TCriticalSection);
begin
  inherited;

end;

procedure TSoLogicKeeper.Execute;
var
  i: Integer;
begin
  inherited;
  for i := 0 to FList.Count - 1 do
    if FList[i].Enabled then
      TSoLogicFriend(FList[i]).Execute;
end;

procedure TSoLogicKeeper.OnItemDestroy(ASender: TObject);
begin
  FList.Delete(TSoLogic(ASender));
end;

end.
