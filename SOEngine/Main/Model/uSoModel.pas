unit uSoModel;

interface

uses
  System.SyncObjs, uEngine2DClasses, System.SysUtils,
  uSoTypes, uCommonClasses, uSoEngineSize,
  uClasses, uSoObjectKeeper, uSoCollider, uSoFormattor, uSoObject,
  uSoAnimator, uSoKeyProcessor, uSoMouseProcessor, uSoLogicKeeper,
  uSoPropertyKeeper, uSoEngineOptions, uSoColliderExtenderFactory, uSoSoundKeeper,
  uSoContainerKeeper, uSoLayoutFactory, uSoLayoutKeeper,
  uSoIDelegateCollector, uSoDelegateCollector, uSoIOperator,
  uSoEngineEvents, uSoCoreModel, uSoRenderer;

type
  TSoModel = class
  private
    FCoreModel: TSoCoreModel;
    FCritical: TCriticalSection;
    FImage: TAnonImage;
    FOptions: TSoEngineOptions;
    FOperators: TList<ISoOperator>;
    // Workers
    FCollider: TSoCollider;
    FFormattor: TSoFormattor;
    FAnimator: TSoAnimator;
    FRenderer: TSoRenderer;
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

    //
    FRegisterDelegate: IRegisterDelegateCollector;

    FEvents: TSoEngineEvents;
    FEngineSize: TSoEngineSize;
    procedure InitFactories;
    procedure InitDelegateCollector;
    procedure SubscribeEvents;
  protected
    // Workers
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
  public
//    function ObjectByName(const AObjectName: string): TSoObject;
    procedure ExecuteOnTick;
    procedure ExecuteKeyUp(ASender: TObject; Key: Word; KeyChar: Char; Shift: TShiftState); // Process key on tick
    procedure ExecuteKeyDown(ASender: TObject; Key: Word; KeyChar: Char; Shift: TShiftState); // Process key on tick
    procedure ExecuteMouseDown(ASender: TObject; AEventArgs: TMouseEventArgs);
    procedure ExecuteMouseUp(ASender: TObject; AEventArgs: TMouseEventArgs);
    procedure ExecuteMouseMove(Sender: TObject; AEventArgs: TMouseMoveEventArgs);
    constructor Create(
      const AEvents: TSoEngineEvents;
      const AImage: TAnonImage;
      const ACritical: TCriticalSection;
      const AOptions: TSoEngineOptions);
    destructor Destroy; override;
  end;

implementation

{ TSoModel }

constructor TSoModel.Create(
  const AEvents: TSoEngineEvents;
  const AImage: TAnonImage;
  const ACritical: TCriticalSection;
  const AOptions: TSoEngineOptions);
begin
  FCritical := ACritical;
  FEvents := AEvents;
  FOptions := AOptions;

  InitFactories;

  FCoreModel := TSoCoreModel.Create(FEvents, AImage, ACritical);

  FRenderer := FCoreModel.Renderer;
  FLayoutKeeper := FCoreModel.LayoutKeeper;
  FContainerKeeper := FCoreModel.ContainerKeeper;

  FObjectKeeper := TSoObjectKeeper.Create(FCritical);
  FLogicKeeper := TSoLogicKeeper.Create(FCritical);
  FCollider := TSoCollider.Create(FCritical, FColliderExtenderFactory.Produce, FOptions.ColliderOptions);
  FFormattor := TSoFormattor.Create(FCritical);
  FAnimator := TSoAnimator.Create(FCritical);
  FKeyProcessor := TSoKeyProcessor.Create(FCritical);
  FMouseProcessor := TSoMouseProcessor.Create(FCritical);
  FSoundKeeper := TSoSoundKeeper.Create(FCritical);

  with FOperators do begin
    Add(FObjectKeeper);
    Add(FLogicKeeper);
    Add(FCollider);
    Add(FFormattor);
    Add(FAnimator);
    Add(FKeyProcessor);
    Add(FMouseProcessor);
    Add(FSoundKeeper);
    Add(FRenderer);
  end;

  InitDelegateCollector;

end;

destructor TSoModel.Destroy;
begin
  FObjectKeeper.Free;
  FLogicKeeper.Free;
  FCollider.Free;
  FFormattor.Free;
  FAnimator.Free;
  FKeyProcessor.Free;
  FMouseProcessor.Free;

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

procedure TSoModel.InitDelegateCollector;
var
  vOperator: ISoOperator;
begin
  FRegisterDelegate := TSoDelegateCollector.Create;

  for vOperator in FOperators do
    vOperator.VisitByDelegateCollector(FRegisterDelegate);
end;

procedure TSoModel.InitFactories;
begin
  FColliderExtenderFactory := TSoColliderExtenderFactory.Create(FOptions.ColliderOptions);
end;

function TSoModel.OnPartAdded(const AObject: TSoObject; const AClass: TClass;
  const AName: string): TObject;
begin

end;

procedure TSoModel.SubscribeEvents;
begin
  with FEvents do begin
    OnMouseDown.Add(ExecuteMouseDown);
    OnMouseUp.Add(ExecuteMouseUp);
    OnMouseMove.Add(ExecuteMouseMove);
  end;
end;

end.

