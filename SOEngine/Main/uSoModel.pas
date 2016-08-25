unit uSoModel;

interface

uses
  System.SyncObjs,
  uClasses, uSoContainerKeeper, uSoRenderer, uSoIntersector, uSoFormattor,
  uSoAnimator, uSoKeyProcessor, uSoMouseProcessor;

type
  TSoModel = class
  private
    FCritical: TCriticalSection;

    FContainerKeeper: TSoContainerKeeper;
    FRenderer: TSoRenderer;
    FIntersector: TSoIntersector;
    FFormattor: TSoFormattor;
    FAnimator: TSoAnimator;
    FKeyProcessor: TSoKeyProcessor;
    FMouseProcessor: TSoMouseProcessor;
public
  procedure Execute;
  constructor Create(const ACritical: TCriticalSection; const AIsHor: TBooleanFunction);
  destructor Destroy; override;
end;

implementation

{ TSoModel }

constructor TSoModel.Create(const ACritical: TCriticalSection;
  const AIsHor: TBooleanFunction);
begin

end;

destructor TSoModel.Destroy;
begin

  inherited;
end;

procedure TSoModel.Execute;
begin

end;

end.
