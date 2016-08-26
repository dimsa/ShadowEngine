unit uSoModel;

interface

uses
  System.SyncObjs, System.Classes, System.UITypes,
  uClasses, uSoContainerKeeper, uSoRenderer, uSoCollider, uSoFormattor,
  uSoAnimator, uSoKeyProcessor, uSoMouseProcessor, uSoLogicKeeper;

type
  TSoModel = class
  private
    FCritical: TCriticalSection;

    FContainerKeeper: TSoContainerKeeper;
    FLogicKeper: TSoLogicKeeper;
    FRenderer: TSoRenderer;
    FIntersector: TSoCollider;
    FFormattor: TSoFormattor;
    FAnimator: TSoAnimator;
    FKeyProcessor: TSoKeyProcessor;
    FMouseProcessor: TSoMouseProcessor;
public
  procedure ExecuteOnTick;
  procedure ExecuteKeyUp(Key: Word; KeyChar: Char; Shift: TShiftState); // Process key on tick
  procedure ExecuteKeyDown(Key: Word; KeyChar: Char; Shift: TShiftState); // Process key on tick
  procedure ExecuteMouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
  procedure ExecuteMouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
  constructor Create(const ACritical: TCriticalSection; const AIsHor: TBooleanFunction);
  destructor Destroy; override;
end;

implementation

{ TSoModel }

constructor TSoModel.Create(const ACritical: TCriticalSection;
  const AIsHor: TBooleanFunction);
begin
  FCritical := ACritical;
  FContainerKeeper := TSoContainerKeeper.Create(FCritical);
  FLogicKeper := TSoLogicKeeper.Create(FCritical);
  FRenderer := TSoRenderer.Create(FCritical);
  FIntersector := TSoCollider.Create(FCritical);

  FAnimator := TSoAnimator.Create(FCritical);
  FKeyProcessor := TSoKeyProcessor.Create(FCritical);
  FMouseProcessor := TSoMouseProcessor.Create;
end;

destructor TSoModel.Destroy;
begin

  inherited;
end;

procedure TSoModel.ExecuteKeyDown(Key: Word; KeyChar: Char; Shift: TShiftState);
begin
  FKeyProcessor.ExecuteKeyDown(Key, KeyChar, Shift);
end;

procedure TSoModel.ExecuteKeyUp(Key: Word; KeyChar: Char; Shift: TShiftState);
begin
  FKeyProcessor.ExecuteKeyUp(Key, KeyChar, Shift);
end;

procedure TSoModel.ExecuteMouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Single);
begin

end;

procedure TSoModel.ExecuteMouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Single);
begin

end;

procedure TSoModel.ExecuteOnTick;
begin
  FAnimator.Execute;
  FLogicKeper.Execute;
  FIntersector.Execute;
  FRenderer.Execute;
end;

end.
