unit uSoCoreModel;

interface

uses
  System.JSON,
  uSoTypes, uEasyDevice,
  uSoRenderer, uSoEngineEvents, uSoContainerKeeper, uSoLayoutKeeper,
  uSoEngineSize, uSoTemplateLoader, uSoContainerDelegates,
  uUnitManagerNew, uWorldManager, uTemplateManager, uSoContainerDelegateCollector;

type
  TSoCoreModelPart = class
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
  protected
    // Workers
    property Renderer: TSoRenderer read FRenderer;
  public
    procedure ExecuteOnTick;

    procedure AddJsonTemplate(const AClassName: string; AJson: TJsonObject);
    procedure AddTemplateFromFile(const AClassName: string; AFileName: string);

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

procedure TSoCoreModelPart.AddJsonTemplate(const AClassName: string; AJson: TJsonObject);
begin

end;

procedure TSoCoreModelPart.AddTemplateFromFile(const AClassName: string; AFileName: string);
begin

end;

constructor TSoCoreModelPart.Create(
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

destructor TSoCoreModelPart.Destroy;
begin
  FRenderer.Free;
  FContainerAddDelegateCollector.Free;
  inherited;
end;

procedure TSoCoreModelPart.ExecuteOnTick;
begin
  FRenderer.Execute;
end;

procedure TSoCoreModelPart.SubscribeEvents;
begin
  with FEvents do begin
    OnResize.Add(OnImageResize);
  end;
end;


procedure TSoCoreModelPart.OnImageResize(ASender: TObject);
begin
  FImage.Bitmap.Width := Round(FImage.Width * getScreenScale);
  FImage.Bitmap.Height := Round(FImage.Height * getScreenScale);
end;

end.
