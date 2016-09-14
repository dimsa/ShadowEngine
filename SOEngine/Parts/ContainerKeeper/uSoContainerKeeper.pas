unit uSoContainerKeeper;

interface

uses
  System.Generics.Collections,
  uSoObject, uSoBasePart, uSoContainer, uSoContainerTypes;

type
  TSoContainerFriend = class (TSoContainer);

  TSoContainerKeeper = class
  private
    FContainers: TDictionary<TSoObject, TSoContainer>;
    function GetContainer(Index: TSoObject): TSoContainer;
  public
    procedure OnAdd(ASender: TObject; AEventArgs: TOnAddContainerEventArgs);
    property Items[Index: TSoObject]: TSoContainer read GetContainer;

    constructor Create;
    destructor Destroy; override;
  end;

implementation

{ TSoContainerKeeper }

constructor TSoContainerKeeper.Create;
begin
  FContainers := TDictionary<TSoObject, TSoContainer>.Create;
end;

destructor TSoContainerKeeper.Destroy;
var
  It: TSoObject;
begin
  for It in FContainers.Keys do
    FContainers[It].Free;

  FContainers.Free;
  inherited;
end;

function TSoContainerKeeper.GetContainer(Index: TSoObject): TSoContainer;
begin
  Result := FContainers[Index];
end;


procedure TSoContainerKeeper.OnAdd(ASender: TObject; AEventArgs: TOnAddContainerEventArgs);
begin
  if not FContainers.ContainsKey(AEventArgs.Subject) then
    FContainers.Add(AEventArgs.Subject, TSoContainer.Create);

  TSoContainerFriend(FContainers[AEventArgs.Subject]).Add(AEventArgs.BasePart);

 { if not FContainers[AEventArgs.Subject].ContainsKey(TSoBasePartClass(AEventArgs.BasePart.ClassType)) then
    FContainers.Add(AEventArgs.Subject, TPartDict.Create);

  //if FContainers[AEventArgs.Subject].ContainsKey(TSoBasePartClass(AEventArgs.BasePart.ClassType)) then
    FContainers[AEventArgs.Subject][TSoBasePartClass(AEventArgs.BasePart.ClassType)].Add(AEventArgs.BasePart);    }
end;

end.
