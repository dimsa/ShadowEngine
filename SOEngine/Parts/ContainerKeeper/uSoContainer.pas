unit uSoContainer;

interface

uses
  System.Generics.Collections, uSoBasePart, uSoContainerTypes;

type
 TSoBasePartClass = class of TSoBasePart;

  TPartDict = TDictionary<TSoBasePartClass, TList<TSoBasePart>>;

  TSoContainer = class
  private
    FParts: TPartDict;
    function GetItem(AIndex: TSoBasePartClass): TList<TSoBasePart>;
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
begin
  FParts[TSoBasePartClass(APart.ClassType)].Add(APart);
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

end.
