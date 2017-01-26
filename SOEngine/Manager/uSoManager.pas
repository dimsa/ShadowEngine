unit uSoManager;

interface

uses
  uUnitManagerNew, uWorldManager, uTemplateManager, uCommonClasses, uSoTypes,
  uSoEngineEvents, uSoLayoutFactory, uSoContainerKeeper, uSoLayoutKeeper,
  uSoTemplateLoader;

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
    constructor Create(
      const AUnitManager: TUnitManager;
      const AWorldManager: TWorldManager;
      const ATemplateManager: TTemplateManager);
    destructor Destroy; override;
  end;

implementation

{ TSoManager }

constructor TSoManager.Create(
      const AUnitManager: TUnitManager;
      const AWorldManager: TWorldManager;
      const ATemplateManager: TTemplateManager);
begin
  FUnitManager := AUnitManager;
  FWorldManager := AWorldManager;
  FTemplateManager := ATemplateManager;
end;

destructor TSoManager.Destroy;
begin
  FUnitManager.Free;
  FTemplateManager.Free;
  FWorldManager.Free;
  inherited;
end;

end.

