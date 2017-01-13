unit uSoLayoutFactory;

interface

uses
  uISoPositionAdapter, uSoLayout, uSoEngineSize, uSoContainerDelegateCollector;

type
  TSoLayoutFactory = class
  private
    FEngineSize: TSoEngineSize;
    FContainerDelegateCollector: TSoContainerDelegateCollector;
  public
    function ProduceLayout(const APositionAdapter: ISoPositionAdapter): TSoLayout;
    constructor Create(
      const AEngineSize: TSoEngineSize;
      const AContainerDelegateCollector: TSoContainerDelegateCollector);
  end;

implementation

{ TSoLayoutFactory }

constructor TSoLayoutFactory.Create(
  const AEngineSize: TSoEngineSize;
  const AContainerDelegateCollector: TSoContainerDelegateCollector);
begin
  FEngineSize := AEngineSize;
  FContainerDelegateCollector := AContainerDelegateCollector;
end;

function TSoLayoutFactory.ProduceLayout(const APositionAdapter: ISoPositionAdapter): TSoLayout;
begin
  Result := TSoLayout.Create(APositionAdapter, FEngineSize, FContainerDelegateCollector);
end;

end.
