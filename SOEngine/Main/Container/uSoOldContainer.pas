unit uSoOldContainer;

interface

uses
  uSoTypes, uSoBasePart, uSoContainerTypes,
  uSoAnimation, uSoColliderObject, uSoKeyHandler, uSoMouseHandler, uE2DRendition, uSoLogic,
  uSoObject;

type
  TSoBasePartClass = class of TSoBasePart;

  TPartDict = TDict<TSoBasePartClass, TList<TSoBasePart>>;

  TSoOldContainer = class
  private
    FSoObject: TSoObject;
    FParts: TPartDict;
    function GetItem(AIndex: TSoBasePartClass): TList<TSoBasePart>;
    procedure OnBasePartDestroy(ASender: TObject);
  protected
    procedure Add(APart: TSoBasePart);
  public
    property Items[AIndex: TSoBasePartClass]: TList<TSoBasePart> read GetItem; default;
    constructor Create;
    destructor Destroy; override;
  end;

implementation

{ TSoContainer }

procedure TSoOldContainer.Add(APart: TSoBasePart);
var
  vClass: TSoBasePartClass;
begin
  if not FParts.ContainsKey(TSoBasePartClass(APart.ClassType)) then
  begin
    vClass := TSoBasePartClass(APart.ClassType);

    FParts.Add(TSoBasePartClass(APart.ClassType), TList<TSoBasePart>.Create);
  end;

  FParts[TSoBasePartClass(APart.ClassType)].Add(APart);
  APart.AddDestroyHandler(OnBasePartDestroy);
end;

constructor TSoOldContainer.Create;
begin
  FParts := TPartDict.Create();
end;

destructor TSoOldContainer.Destroy;
begin
  FParts.Free;
  inherited;
end;

function TSoOldContainer.GetItem(AIndex: TSoBasePartClass): TList<TSoBasePart>;
begin
  Result := FParts[AIndex];
end;

procedure TSoOldContainer.OnBasePartDestroy(ASender: TObject);
var
  vBasePart: TSoBasePart;
begin
  vBasePart := TSoBasePart(ASender);
  FParts[TSoBasePartClass(vBasePart.ClassType)].Remove(vBasePart);
  if FParts[TSoBasePartClass(vBasePart.ClassType)].Count <= 0 then
    FParts.Remove(TSoBasePartClass(vBasePart.ClassType));
  //vBasePart.Free;
end;

end.
