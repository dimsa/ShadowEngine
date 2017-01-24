unit uSoManager;

interface

uses
  uUnitManagerNew, uWorldManager, uTemplateManager, uCommonClasses, uSoTypes,
  uSoEngineEvents, uSoLayoutFactory, uSoContainerKeeper, uSoLayoutKeeper,
  uTemplateLoader;

type
  TSoManager = class
  private
    FEngineWidth, FEngineHeight: Single;

    FUnitManager: TUnitManager; // Controller for creating units form template and etc
    FTemplateManager: TTemplateManager; // Controller to Load Templates if their loaders are ready
    FWorldManager: TWorldManager; // Controller to create different lowlevel world render.
    procedure OnResize(ASender: TObject);
  public
    property UnitManager: TUnitManager read FUnitManager;
    property WorldManager: TWorldManager read FWorldManager;
    property TemplateManager: TTemplateManager read FTemplateManager;
    constructor Create(
      const AContainerKeeper: TSoContainerKeeper;
      const ALayoutKeeper: TSoLayoutKeeper;
      const AEvents: TSoEngineEvents;
      const ATemplateLoader: ITemplateLoader);
    destructor Destroy; override;
  end;

implementation

{ TSoManager }

constructor TSoManager.Create(
  const AContainerKeeper: TSoContainerKeeper;
  const ALayoutKeeper: TSoLayoutKeeper;
  const AEvents: TSoEngineEvents;
  const ATemplateLoader: ITemplateLoader);
begin
  AEvents.OnResize.Add(OnResize);

  FUnitManager := TUnitManager.Create(AContainerKeeper, ALayoutKeeper);
  FTemplateManager := TTemplateManager.Create(ATemplateLoader);
  FWorldManager := TWorldManager.Create(AModel, AEvents);
end;

destructor TSoManager.Destroy;
begin
  FUnitManager.Free;
  FTemplateManager.Free;
  FWorldManager.Free;
  inherited;
end;

procedure TSoManager.OnResize(ASender: TObject);
begin
  FEngineWidth := TAnonImage(ASender).Width;
  FEngineHeight := TAnonImage(ASender).Height;
end;

end.

