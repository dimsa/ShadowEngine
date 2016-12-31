unit uSoContainerKeeper;

interface

uses
  uSoTypes, uSoModel, uSoObject,
  uSoContainer;

type
  TSoContainerKeeper = class
  private type
    TSoObjectFriend = class(TSoObject);
    TSoModelFriend = class(TSoModel);
  private
    FModel: TSoModelFriend;
    FContainerByObject: TDict<TSoObject, TSoContainer>;
    FObjectByContainer: TDict<TSoContainer, TSoObject>;
    procedure OnContainerDestroy(ASender: TObject);
    function Add(const AObject: TSoObject): TSoContainer;
  public
    function AddAbs: TSoContainer; // Add SoContainer with SoObject with Absolute PositionAdapter
    function Add320: TSoContainer; // Add SoContainer with SoObject with 320x240 PositionAdapter
    constructor Create(const AModel: TSoModel);
    destructor Destroy; override;
  end;

implementation

uses
  uSoPositionAdapterAbsolute, uSoPositionAdapter320;

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

function TSoContainerKeeper.Add320: TSoContainer;
begin
  Add(
    TSoObject.Create(
      TSoPositionAdapter320.Create(@FModel.EngineWidth, @FModel.EngineHeight)));
end;

function TSoContainerKeeper.AddAbs: TSoContainer;
begin
  Add(
    TSoObject.Create(
      TSoPositionAdapterAbsolute.Create));
end;

constructor TSoContainerKeeper.Create(const AModel: TSoModel);
begin
  FModel := TSoModelFriend(AModel);

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
