unit uSoModel;

interface

uses
  System.SyncObjs, uEngine2DClasses, System.SysUtils, System.JSON,
  uSoTypes, uCommonClasses, uEasyDevice,
  uClasses, uSoObjectKeeper, uSoRenderer, uSoCollider, uSoFormattor, uSoObject,
  uSoAnimator, uSoKeyProcessor, uSoMouseProcessor, uSoLogicKeeper,
  uSoPropertyKeeper, uSoEngineOptions, uSoColliderExtenderFactory, uSoSoundKeeper,
  uSoContainerKeeper, uSoEngineSize, uSoLayoutFactory, uSoLayoutKeeper,
  uSoContainerDelegateCollector, uSoIDelegateCollector,
  uTemplateManager, uWorldManager, uUnitManagerNew, uSoEngineEvents, uSoTemplateLoader;

type
  TSoModel = class
  private
    FCritical: TCriticalSection;
    FImage: TAnonImage;
    FOptions: TSoEngineOptions;
    // Workers
    FRenderer: TSoRenderer;
    FCollider: TSoCollider;
    FFormattor: TSoFormattor;
    FAnimator: TSoAnimator;
    // Keepers
    FObjectKeeper: TSoObjectKeeper;
    FLogicKeeper: TSoLogicKeeper;
    FSoundKeeper: TSoSoundKeeper;
    FContainerKeeper: TSoContainerKeeper;
    FLayoutKeeper: TSoLayoutKeeper;
    // Processors
    FKeyProcessor: TSoKeyProcessor;
    FMouseProcessor: TSoMouseProcessor;
    // Factories
    FColliderExtenderFactory: TSoColliderExtenderFactory;

    // Managers
    FUnitManager: TUnitManager;
    FWorldManager: TWorldManager;
    FTemplateManager: TTemplateManager;
    FTemplateLoader: TSoTemplateLoader;

    FContainerAddDelegateCollector: TSoContainerDelegateCollector;

    FEvents: TSoEngineEvents;
    FEngineSize: TSoEngineSize;
    procedure OnImageResize(ASender: TObject);
    procedure InitFactories;
    procedure InitContainerDelegateCollector;
    procedure CreateManager;
    procedure SubscribeImageEvent;
  protected
    // Workers
    property Renderer: TSoRenderer read FRenderer;
    property Collider: TSoCollider read FCollider;
    property Formattor: TSoFormattor read FFormattor;
    property Animator: TSoAnimator read FAnimator;
    // Keepers
    property ObjectKeeper: TSoObjectKeeper read FObjectKeeper;
    property SoundKeeper: TSoSoundKeeper read FSoundKeeper;
    property LogicKeeper: TSoLogicKeeper read FLogicKeeper;
    property ContainerKeeper: TSoContainerKeeper read FContainerKeeper;
    property LayoutKeeper: TSoLayoutKeeper read FLayoutKeeper;
    // Processors
    property KeyProcessor: TSoKeyProcessor read FKeyProcessor;
    property MouseProcessor: TSoMouseProcessor read FMouseProcessor;
    function OnPartAdded(const AObject: TSoObject; const AClass: TClass; const AName: string): TObject;

    procedure AddJsonTemplate(const AClassName: string; AJson: TJsonObject);
    procedure AddTemplateFromFile(const AClassName: string; AFileName: string);
  public
    property UnitManager: TUnitManager read FUnitManager;
    property WorldManager: TWorldManager read FWorldManager;
    property TemplateManager: TTemplateManager read FTemplateManager;
//    function ObjectByName(const AObjectName: string): TSoObject;
    procedure ExecuteOnTick;
    procedure ExecuteKeyUp(ASender: TObject; Key: Word; KeyChar: Char; Shift: TShiftState); // Process key on tick
    procedure ExecuteKeyDown(ASender: TObject; Key: Word; KeyChar: Char; Shift: TShiftState); // Process key on tick
    procedure ExecuteMouseDown(ASender: TObject; AEventArgs: TMouseEventArgs);
    procedure ExecuteMouseUp(ASender: TObject; AEventArgs: TMouseEventArgs);
    procedure ExecuteMouseMove(Sender: TObject; AEventArgs: TMouseMoveEventArgs);
    constructor Create(
      const AImage: TAnonImage;
      const ACritical: TCriticalSection;
      const AOptions: TSoEngineOptions);
    destructor Destroy; override;
  end;

implementation

{ TSoModel }

procedure TSoModel.AddJsonTemplate(const AClassName: string; AJson: TJsonObject);
begin

end;

procedure TSoModel.AddTemplateFromFile(const AClassName: string; AFileName: string);
begin

end;

constructor TSoModel.Create(
  const AImage: TAnonImage;
  const ACritical: TCriticalSection;
  const AOptions: TSoEngineOptions);
begin
  FCritical := ACritical;
  FImage := AImage;
  FOptions := AOptions;

  FEvents := TSoEngineEvents.Create(AImage);
  FEngineSize := TSoEngineSize.Create(FEvents, AImage.Width, AImage.Height);

  InitFactories;

  FObjectKeeper := TSoObjectKeeper.Create(FCritical);
  FLogicKeeper := TSoLogicKeeper.Create(FCritical);
  FRenderer := TSoRenderer.Create(FCritical, AImage);
  FCollider := TSoCollider.Create(FCritical, FColliderExtenderFactory.Produce, FOptions.ColliderOptions);
  FFormattor := TSoFormattor.Create(FCritical);
  FAnimator := TSoAnimator.Create(FCritical);
  FKeyProcessor := TSoKeyProcessor.Create(FCritical);
  FMouseProcessor := TSoMouseProcessor.Create(FCritical);
  FSoundKeeper := TSoSoundKeeper.Create(FCritical);

  FContainerKeeper := TSoContainerKeeper.Create(FEngineSize);

  InitContainerDelegateCollector;
  FLayoutKeeper := TSoLayoutKeeper.Create(FEngineSize, FContainerAddDelegateCollector);
  FLayoutKeeper.LayoutAdded := FContainerKeeper.OnLayoutAdded;
end;

procedure TSoModel.CreateManager;
begin
  FTemplateLoader := TSoTemplateLoader.Create(Self.AddTemplateFromFile, Self.AddJsonTemplate);

  FUnitManager := TUnitManager.Create(FContainerKeeper, FLayoutKeeper);
  FTemplateManager := TTemplateManager.Create(FTemplateLoader);
  FWorldManager := TWorldManager.Create(FRenderer, FEvents);
end;

destructor TSoModel.Destroy;
begin
  FLayoutKeeper.Free;
  FContainerKeeper.Free;

  FObjectKeeper.Free;
  FLogicKeeper.Free;
  FRenderer.Free;
  FCollider.Free;
  FFormattor.Free;
  FAnimator.Free;
  FKeyProcessor.Free;
  FMouseProcessor.Free;

  FContainerAddDelegateCollector.Free;
  inherited;
end;

procedure TSoModel.ExecuteKeyDown(ASender: TObject; Key: Word; KeyChar: Char; Shift: TShiftState);
begin
  FKeyProcessor.ExecuteKeyDown(Key, KeyChar, Shift);
end;

procedure TSoModel.ExecuteKeyUp(ASender: TObject; Key: Word; KeyChar: Char; Shift: TShiftState);
begin
  FKeyProcessor.ExecuteKeyUp(Key, KeyChar, Shift);
end;

procedure TSoModel.ExecuteMouseDown(ASender: TObject; AEventArgs: TMouseEventArgs);
begin
  FMouseProcessor.ExecuteMouseDown(AEventArgs);
end;

procedure TSoModel.ExecuteMouseMove(Sender: TObject; AEventArgs: TMouseMoveEventArgs);
begin
  FMouseProcessor.ExecuteMouseMove(AEventArgs);
end;

procedure TSoModel.ExecuteMouseUp(ASender: TObject; AEventArgs: TMouseEventArgs);
begin
  FMouseProcessor.ExecuteMouseUp(AEventArgs);
end;

procedure TSoModel.ExecuteOnTick;
begin
  FAnimator.Execute;
  FLogicKeeper.Execute;
  FCollider.Execute;
  FRenderer.Execute;
end;

procedure TSoModel.InitContainerDelegateCollector;
begin
{  FContainerAddDelegateCollector := TSoContainerDelegateCollector.Create(
    FAnimator.Add,
    FAnimator.AddFromTemplate,
    FCollider.AddFromTemplate,
    FCollider.Add,
    FFormattor.Add,
    FFormattor.AddFromTemplate,
    FFormattor.AddFromCode,
    FMouseProcessor.Add,
    FMouseProcessor.AddFromTemplate,
    FKeyProcessor.Add,
    FKeyProcessor.AddFromTemplate,
    FLogicKeeper.Add,
    FLogicKeeper.AddFromTemplate,
    nil,
    nil,
    FRenderer.Add,
    FRenderer.AddFromTemplate,
    FSoundKeeper.Add,
    FSoundKeeper.AddFromTemplate,
    FSoundKeeper.AddByFileName
  );  }
end;

procedure TSoModel.InitFactories;
begin
  FColliderExtenderFactory := TSoColliderExtenderFactory.Create(FOptions.ColliderOptions);
end;

{function TSoModel.ObjectByName(const AObjectName: string): TSoObject;
begin
  Result := FObjectKeeper.Items[AObjectName];
end; }

procedure TSoModel.OnImageResize(ASender: TObject);
begin
  //FEngineObject[RenditionRect].RaiseOnChange;

  FImage.Bitmap.Width := Round(FImage.Width * getScreenScale);
  FImage.Bitmap.Height := Round(FImage.Height * getScreenScale);
end;

function TSoModel.OnPartAdded(const AObject: TSoObject; const AClass: TClass;
  const AName: string): TObject;
begin

end;

procedure TSoModel.SubscribeImageEvent;
begin
  with FEvents do begin
    OnResize.Add(OnImageResize);
    OnMouseDown.Add(ExecuteMouseDown);
    OnMouseUp.Add(ExecuteMouseUp);
    OnMouseMove.Add(ExecuteMouseMove);
  end;
end;

end.

