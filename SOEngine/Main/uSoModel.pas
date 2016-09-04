unit uSoModel;

interface

uses
  System.SyncObjs, System.Classes, System.UITypes, uEngine2DClasses,
  uClasses, uSoContainerKeeper, uSoRenderer, uSoCollider, uSoFormattor,
  uSoAnimator, uSoKeyProcessor, uSoMouseProcessor, uSoLogicKeeper, uSoContainer;

type
  TSoModel = class
  private
    FCritical: TCriticalSection;
    FImage: TAnonImage;
    // Workers
    FRenderer: TSoRenderer;
    FCollider: TSoCollider;
    FFormattor: TSoFormattor;
    FAnimator: TSoAnimator;
    // Keepers
    FContainerKeeper: TSoContainerKeeper;
    FLogicKeper: TSoLogicKeeper;
    // Processors
    FKeyProcessor: TSoKeyProcessor;
    FMouseProcessor: TSoMouseProcessor;
  protected
    // Workers
    property Renderer: TSoRenderer read FRenderer;
    property Collider: TSoCollider read FCollider;
    property Formattor: TSoFormattor read FFormattor;
    property Animator: TSoAnimator read FAnimator;
    // Keepers
    property ContainerKeeper: TSoContainerKeeper read FContainerKeeper;
    property LogicKeeper: TSoLogicKeeper read FLogicKeper;
    // Processors
    property KeyProcessor: TSoKeyProcessor read FKeyProcessor;
    property MouseProcessor: TSoMouseProcessor read FMouseProcessor;
  public
    procedure ExecuteOnTick;
    procedure ExecuteKeyUp(ASender: TObject; Key: Word; KeyChar: Char; Shift: TShiftState); // Process key on tick
    procedure ExecuteKeyDown(ASender: TObject; Key: Word; KeyChar: Char; Shift: TShiftState); // Process key on tick
    procedure ExecuteMouseDown(ASender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure ExecuteMouseUp(ASender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure ExecuteMouseMove(X, Y: Single);
    constructor Create(const AImage: TAnonImage; const ACritical: TCriticalSection; const AIsHor: TBooleanFunction);
    destructor Destroy; override;
  end;

implementation

{ TSoModel }

constructor TSoModel.Create(const AImage: TAnonImage; const ACritical: TCriticalSection;
  const AIsHor: TBooleanFunction);
begin
  FCritical := ACritical;
  FContainerKeeper := TSoContainerKeeper.Create(FCritical);
  FLogicKeper := TSoLogicKeeper.Create(FCritical);
  FRenderer := TSoRenderer.Create(FCritical, FImage);
  FCollider := TSoCollider.Create(FCritical);
  FFormattor := TSoFormattor.Create(FCritical);
  FAnimator := TSoAnimator.Create(FCritical);
  FKeyProcessor := TSoKeyProcessor.Create(FCritical);
  FMouseProcessor := TSoMouseProcessor.Create(FCritical, FCollider);
end;

destructor TSoModel.Destroy;
begin
    FContainerKeeper.Free;
    FLogicKeper.Free;
    FRenderer.Free;
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

procedure TSoModel.ExecuteMouseDown(ASender: TObject; Button: TMouseButton; Shift: TShiftState; X,
  Y: Single);
begin
  FMouseProcessor.ExecuteMouseDown(Button, Shift, X, Y);
end;

procedure TSoModel.ExecuteMouseMove(X, Y: Single);
begin
  FMouseProcessor.ExecuteMouseMove(X, Y);
end;

procedure TSoModel.ExecuteMouseUp(ASender: TObject; Button: TMouseButton; Shift: TShiftState; X,
  Y: Single);
begin
  FMouseProcessor.ExecuteMouseUp(Button, Shift, X, Y);
end;

procedure TSoModel.ExecuteOnTick;
begin
  FAnimator.Execute;
  FLogicKeper.Execute;
  FCollider.Execute;
  FRenderer.Execute;
end;

end.
