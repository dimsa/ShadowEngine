unit uSoContainerKeeper;

interface

uses
  uSoTypes, uSoObject,
  uSoContainer;

type
  TSoContainerKeeper = class
  private type
    TSoObjectFriend = class(TSoObject);
  private
    FEngineWidth, FEngineHeight: PInteger;
    FContainerByObject: TDict<TSoObject, TSoContainer>;
    FObjectByContainer: TDict<TSoContainer, TSoObject>;
    procedure OnContainerDestroy(ASender: TObject);
    function Add(const AObject: TSoObject): TSoContainer;
  public
    function AddAbs: TSoContainer; // Add SoContainer with SoObject with Absolute PositionAdapter
    function Add320: TSoContainer; // Add SoContainer with SoObject with 320x240 PositionAdapter
    constructor Create(const AEngineWidth, AEngineHeight: PInteger);
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
  vCont := TSoContainer.Create(AObject);
  TSoObjectFriend(AObject).SetContainer(vCont);
  vCont.AddOnDestroy(OnContainerDestroy);

  FContainerByObject.Add(AObject, vCont);
  FObjectByContainer.Add(vCont, AObject);
end;

function TSoContainerKeeper.Add320: TSoContainer;
var
  vObj: TSoObject;
begin
  vObj := TSoObject.Create(
    TSoPositionAdapter320.Create(@FEngineWidth, @FEngineHeight));

  Add(vObj);
end;

function TSoContainerKeeper.AddAbs: TSoContainer;
var
  vObj: TSoObject;
begin
  vObj := TSoObject.Create(
    TSoPositionAdapterAbsolute.Create);

  Add(vObj);
end;

constructor TSoContainerKeeper.Create(const AEngineWidth, AEngineHeight: PInteger);
begin
  FEngineWidth := AEngineWidth;
  FEngineHeight := AEngineHeight;

  FContainerByObject := TDict<TSoObject, TSoContainer>.Create;
  FObjectByContainer := TDict<TSoContainer, TSoObject>.Create;
end;

destructor TSoContainerKeeper.Destroy;
begin
  FContainerByObject.Free;
  FObjectByContainer.Free;
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
