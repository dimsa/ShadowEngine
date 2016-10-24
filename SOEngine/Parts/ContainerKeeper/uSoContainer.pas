unit uSoContainer;

interface

uses
  uSoTypes, uSoBasePart, uSoContainerTypes,
  uSoAnimation, uSoColliderObject, uSoKeyHandler, uSoMouseHandler, uE2DRendition, uSoLogic;

type
  TSoBasePartClass = class of TSoBasePart;

  TPartDict = TDict<TSoBasePartClass, TList<TSoBasePart>>;

  TSoContainer = class
  private
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

procedure TSoContainer.Add(APart: TSoBasePart);
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

constructor TSoContainer.Create;
begin
  FParts := TPartDict.Create();
end;

destructor TSoContainer.Destroy;
begin
  FParts.Free;
  inherited;
end;

function TSoContainer.GetItem(AIndex: TSoBasePartClass): TList<TSoBasePart>;
begin
  Result := FParts[AIndex];
end;

procedure TSoContainer.OnBasePartDestroy(ASender: TObject);
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
