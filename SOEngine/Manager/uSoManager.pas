unit uSoManager;

interface

uses
  uUnitManager, uWorldManager, uTemplateManager, uSoModel, uCommonClasses, uSoTypes,
  uSoEngineEvents, uSoLayoutFactory;

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
    constructor Create(const AModel: TSoModel; const AEvents: TSoEngineEvents);
    destructor Destroy; override;
  end;

implementation

{ TSoManager }

constructor TSoManager.Create(const AModel: TSoModel; const AEvents: TSoEngineEvents);
begin
  AEvents.OnResize.Add(OnResize);

  FUnitManager := TUnitManager.Create(AModel, nil);
  FTemplateManager := TTemplateManager.Create(AModel);
  FWorldManager := TWorldManager.Create(AModel, AEvents);
end;

destructor TSoManager.Destroy;
begin

  inherited;
end;

procedure TSoManager.OnResize(ASender: TObject);
begin
  FEngineWidth := TAnonImage(ASender).Width;
  FEngineHeight := TAnonImage(ASender).Height;
end;

end.

