unit uSoContainerKeeper;

interface

uses
  uSoTypes, uSoObject, uSoBasePart, uSoContainer, uSoContainerTypes;

type
  TSoContainerFriend = class(TSoContainer);

  TSoObjectFriend = class(TSoObject);

  TSoContainerKeeper = class
  private
    FContainers: TDict<TSoObject, TSoContainer>;
    function GetContainer(Index: TSoObject): TSoContainer;
    procedure OnObjectDestroy(ASender: TObject);
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
  FContainers := TDict<TSoObject, TSoContainer>.Create;
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
var
  vContainer: TSoContainer;
begin
  if not FContainers.ContainsKey(AEventArgs.Subject) then
  begin
    vContainer := TSoContainer.Create;
    FContainers.Add(AEventArgs.Subject, vContainer);
    TSoObjectFriend(AEventArgs.Subject).SetContainer(vContainer);
    AEventArgs.Subject.AddDestroyHandler(OnObjectDestroy);
  end else
    vContainer := FContainers[AEventArgs.Subject];

  TSoContainerFriend(vContainer).Add(AEventArgs.BasePart);
end;

procedure TSoContainerKeeper.OnObjectDestroy(ASender: TObject);
var
  vCont: TSoContainer;
begin
  vCont := FContainers[TSoObject(ASender)];
  FContainers.Remove(TSoObject(ASender));
  vCont.Free;
end;

end.
