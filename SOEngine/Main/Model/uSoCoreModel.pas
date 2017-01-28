unit uSoCoreModel;

interface

uses
  System.JSON,
  uSoTypes, uEasyDevice,
  uSoRenderer, uSoEngineEvents, uSoContainerKeeper, uSoLayoutKeeper,
  uSoEngineSize, uSoTemplateLoader, uSoContainerDelegates,
  uUnitManagerNew, uWorldManager, uTemplateManager, uSoContainerDelegateCollector;

type
  TSoCoreModel = class
  private
    FImage: TAnonImage;
    FRenderer: TSoRenderer;
    FContainerKeeper: TSoContainerKeeper;
    FLayoutKeeper: TSoLayoutKeeper;
    FEngineSize: TSoEngineSize;
    FEvents: TSoEngineEvents;

    // Managers
    FUnitManager: TUnitManager;
    FWorldManager: TWorldManager;
    FTemplateManager: TTemplateManager;
    FTemplateLoader: TSoTemplateLoader;

    FContainerAddDelegateCollector: TSoContainerDelegateCollector;

    procedure SubscribeEvents;
    procedure OnImageResize(ASender: TObject);
  public
    procedure ExecuteOnTick;

    procedure AddJsonTemplate(const AClassName: string; AJson: TJsonObject);
    procedure AddTemplateFromFile(const AClassName: string; AFileName: string);

    // Workers
    property Renderer: TSoRenderer read FRenderer;
    // Keepers
    property ContainerKeeper: TSoContainerKeeper read FContainerKeeper;
    property LayoutKeeper: TSoLayoutKeeper read FLayoutKeeper;

    property UnitManager: TUnitManager read FUnitManager;
    property WorldManager: TWorldManager read FWorldManager;
    property TemplateManager: TTemplateManager read FTemplateManager;

    constructor Create(
      const AEvents: TSoEngineEvents;
      const AImage: TAnonImage;
      const ACritical: TCriticalSection);
    destructor Destroy; override;
  end;

implementation

{ TSoCoreModelPart }

procedure TSoCoreModel.AddJsonTemplate(const AClassName: string; AJson: TJsonObject);
begin

end;

procedure TSoCoreModel.AddTemplateFromFile(const AClassName: string; AFileName: string);
begin

end;

constructor TSoCoreModel.Create(
      const AEvents: TSoEngineEvents;
      const AImage: TAnonImage;
      const ACritical: TCriticalSection);
begin
  FImage := AImage;
  FEvents := AEvents;
// It should be moved to another place. But what is this place? o_O
  FRenderer := TSoRenderer.Create(ACritical, AImage);
  FEngineSize := TSoEngineSize.Create(FEvents, AImage.Width, AImage.Height);
  FContainerKeeper := TSoContainerKeeper.Create(FEngineSize);
  FLayoutKeeper := TSoLayoutKeeper.Create(FEngineSize, FContainerAddDelegateCollector);
  FLayoutKeeper.LayoutAdded := FContainerKeeper.OnLayoutAdded;

  FTemplateLoader := TSoTemplateLoader.Create(AddJsonTemplate, AddTemplateFromFile);

  FUnitManager := TUnitManager.Create(FContainerKeeper, FLayoutKeeper);
  FTemplateManager := TTemplateManager.Create(FTemplateLoader);
  FWorldManager := TWorldManager.Create(FRenderer, FEvents);
end;

destructor TSoCoreModel.Destroy;
begin
  FRenderer.Free;
  FContainerAddDelegateCollector.Free;
  inherited;
end;

procedure TSoCoreModel.ExecuteOnTick;
begin
  FRenderer.Execute;
end;

procedure TSoCoreModel.SubscribeEvents;
begin
  with FEvents do begin
    OnResize.Add(OnImageResize);
  end;
end;


procedure TSoCoreModel.OnImageResize(ASender: TObject);
begin
  FImage.Bitmap.Width := Round(FImage.Width * getScreenScale);
  FImage.Bitmap.Height := Round(FImage.Height * getScreenScale);
end;

end.
