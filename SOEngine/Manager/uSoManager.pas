unit uSoManager;

interface

uses
  uUnitManager, uWorldManager, uTemplateManager, uSoModel, uCommonClasses, uSoTypes, uSoEngineEvents;

type
  TSoManager = class
  private
    FUnitManager: TUnitManager; // Controller for creating units form template and etc
    FTemplateManager: TTemplateManager; // Controller to Load Templates if their loaders are ready
    FWorldManager: TWorldManager; // Controller to create different lowlevel world render.
  public
    property UnitManager: TUnitManager read FUnitManager;
    property WorldManager: TWorldManager read FWorldManager;
    property TemplateManager: TTemplateManager read FTemplateManager;
    constructor Create(const AModel: TSoModel; const AEvents: TSoEngineEvents);
  end;

implementation

{ TSoManager }

constructor TSoManager.Create(const AModel: TSoModel; const AEvents: TSoEngineEvents);
begin
  FUnitManager := TUnitManager.Create(AModel);
  FTemplateManager := TTemplateManager.Create(AModel);
  FWorldManager := TWorldManager.Create(AModel, AEvents);
end;

end.

