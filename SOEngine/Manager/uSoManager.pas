unit uSoManager;

interface

uses
  uUnitManager, uWorldManager, uTemplateManager, uSoModel, uCommonClasses, uSoTypes;

type
  TSoManager = class
  private
    FUnitManager: TUnitManager;
    FTemplateManager: TTemplateManager;
    FWorldManager: TWorldManager;
  public
    property UnitManager: TUnitManager read FUnitManager;
    property WorldManager: TWorldManager read FWorldManager;
    property TemplateManager: TTemplateManager read FTemplateManager;
    constructor Create(const AModel: TSoModel; const AOnResize: TEventList<TAnonImage>);
  end;

implementation

{ TSoManager }

constructor TSoManager.Create(const AModel: TSoModel; const AOnResize: TEventList<TAnonImage>);
begin
  FUnitManager := TUnitManager.Create(AModel);
  FTemplateManager := TTemplateManager.Create(AModel);
  FWorldManager := TWorldManager.Create(AModel, AOnResize);
end;

end.

