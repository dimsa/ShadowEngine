unit uSoContainerKeeper;

interface

uses
  uSoTypes, uSoModel, uSoObject,
  uSoContainer;

type
  TSoContainerKeeper = class
  private type
    TSoObjectFriend = class(TSoObject);
  private
    FModel: TSoModel;
    FContainerByObject: TDict<TSoObject, TSoContainer>;
    FObjectByContainer: TDict<TSoContainer, TSoObject>;
    procedure OnContainerDestroy(ASender: TObject);
  public
    function Add(const AObject: TSoObject): TSoContainer;
    constructor Create(const AModel: TSoModel);
    destructor Destroy; override;
  end;

implementation

{ TSoContainerKeeper }

function TSoContainerKeeper.Add(const AObject: TSoObject): TSoContainer;
var
  vCont: TSoContainer;
begin
  vCont := TSoContainer.Create(AObject, FModel);
  vCont.AddOnDestroy(OnContainerDestroy);
  TSoObjectFriend(AObject).SetContainer(vCont);
  FContainerByObject.Add(AObject, vCont);
  FObjectByContainer.Add(vCont, AObject);
end;

constructor TSoContainerKeeper.Create(const AModel: TSoModel);
begin
  FModel := AModel;

  FContainerByObject := TDict<TSoObject, TSoContainer>.Create;
  FObjectByContainer := TDict<TSoContainer, TSoObject>.Create;
end;

destructor TSoContainerKeeper.Destroy;
begin
  FContainerByObject.Free;
  FObjectByContainer.Free;
  FModel := nil;
  inherited;
end;

procedure TSoContainerKeeper.OnContainerDestroy(ASender: TObject);
var
  vCont: TSoContainer;
begin
  vCont := TSoContainer(ASender);

  if FObjectByContainer.ContainsKey(vCont) then
    FObjectByContainer.Remove(vCont);

  if FContainerByObject.ContainsKey(FObjectByContainer[vCont]) then
    FContainerByObject.Remove(FObjectByContainer[vCont]);
end;

end.
