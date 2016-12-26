unit uSoContainerKeeper;

interface

uses
  uSoTypes, uSoObject, uSoBasePart, uSoOldContainer, uSoContainerTypes;

type
  TSoContainerFriend = class(TSoOldContainer);

  TSoObjectFriend = class(TSoObject);

  TSoOldContainerKeeper = class
  private
    FContainers: TDict<TSoObject, TSoOldContainer>;
    function GetContainer(Index: TSoObject): TSoOldContainer;
    procedure OnObjectDestroy(ASender: TObject);
  public
    procedure OnAdd(ASender: TObject; AEventArgs: TOnAddContainerEventArgs);
    property Items[Index: TSoObject]: TSoOldContainer read GetContainer;

    constructor Create;
    destructor Destroy; override;
  end;

implementation

{ TSoOldContainerKeeper }

constructor TSoOldContainerKeeper.Create;
begin
  FContainers := TDict<TSoObject, TSoOldContainer>.Create;
end;

destructor TSoOldContainerKeeper.Destroy;
var
  It: TSoObject;
begin
  for It in FContainers.Keys do
    FContainers[It].Free;

  FContainers.Free;
  inherited;
end;

function TSoOldContainerKeeper.GetContainer(Index: TSoObject): TSoOldContainer;
begin
  Result := FContainers[Index];
end;


procedure TSoOldContainerKeeper.OnAdd(ASender: TObject; AEventArgs: TOnAddContainerEventArgs);
var
  vContainer: TSoOldContainer;
begin
  if not FContainers.ContainsKey(AEventArgs.Subject) then
  begin
    vContainer := TSoOldContainer.Create;
    FContainers.Add(AEventArgs.Subject, vContainer);
    TSoObjectFriend(AEventArgs.Subject).SetContainer(vContainer);
    AEventArgs.Subject.AddDestroyHandler(OnObjectDestroy);
  end else
    vContainer := FContainers[AEventArgs.Subject];

  TSoContainerFriend(vContainer).Add(AEventArgs.BasePart);
end;

procedure TSoOldContainerKeeper.OnObjectDestroy(ASender: TObject);
var
  vCont: TSoOldContainer;
begin
  vCont := FContainers[TSoObject(ASender)];
  FContainers.Remove(TSoObject(ASender));
  vCont.Free;
end;

end.
