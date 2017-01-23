unit uSoContainerKeeper;

interface

uses
  uSoTypes, uSoObject, uCommonClasses,
  uSoContainer, uSoEngineSize, uISoPositionAdapter;

type
  TSoContainerKeeper = class
  private type
    TSoObjectFriend = class(TSoObject);
  private
    FEngineWidth, FEngineHeight: PInteger;
    FContainers: TUniqueList<TSoContainer>;
    FContainerByName: TDict<string, TSoContainer>;
    FContainerByObject: TDict<TSoObject, TSoContainer>;

    FContainerAdded: TEvent<ISoPositionAdapter>;
    procedure OnContainerDestroy(ASender: TObject);
    procedure OnContainerOnLayoutAdded(ASender: TObject);
    procedure Add(const AContainer: TSoContainer; const AName: string = '');
  public
    procedure OnLayoutAdded(ASender: TObject);
    constructor Create(const AEngineSize: TSoEngineSize);
    destructor Destroy; override;
  end;

implementation

uses
  uSoPositionAdapterAbsolute, uSoPositionAdapter320, uSoLayout;

{ TSoContainerKeeper }

procedure TSoContainerKeeper.Add(const AContainer: TSoContainer; const AName: string);
var
  vCont: TSoContainer;
begin
  vCont := AContainer;

  TSoObjectFriend(vCont.Subject).SetContainer(vCont);
  vCont.AddOnDestroy(OnContainerDestroy);

  FContainerByObject.Add(vCont.Subject, vCont);
  FContainerByName.Add(AName, vCont);
end;

constructor TSoContainerKeeper.Create(const AEngineSize: TSoEngineSize);
begin
  FEngineWidth := @AEngineSize.Width;
  FEngineHeight := @AEngineSize.Height;

  FContainers := TUniqueList<TSoContainer>.Create;
  FContainerByName := TDict<string, TSoContainer>.Create;
  FContainerByObject := TDict<TSoObject, TSoContainer>.Create;
end;

destructor TSoContainerKeeper.Destroy;
var
  i: Integer;
begin
  for i := 0 to FContainers.Count do
    FContainers[i].Free;

  FContainers.Free;
  FContainerByObject.Free;
  FContainerByName.Free;
  inherited;
end;

procedure TSoContainerKeeper.OnContainerOnLayoutAdded(ASender: TObject);
begin
  Add(TSoContainer(ASender));
end;

procedure TSoContainerKeeper.OnContainerDestroy(ASender: TObject);
var
  vCont: TSoContainer;
begin
  vCont := TSoContainer(ASender);

  if FContainerByObject.ContainsKey(vCont.Subject) then
    FContainerByObject.Remove(vCont.Subject);

  FContainers.Remove(vCont);
end;

procedure TSoContainerKeeper.OnLayoutAdded(ASender: TObject);
begin
  TSoLayout(ASender).ContainerAdded := OnContainerOnLayoutAdded;
end;

end.

