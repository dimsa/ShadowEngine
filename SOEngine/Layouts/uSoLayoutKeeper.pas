unit uSoLayoutKeeper;

interface

uses

  uSoTypes, uSoLayout, uSoLayoutFActory, uISoPositionAdapter, uSoEngineSize,
  uSoContainerDelegateCollector;

type
  TSoLayoutKeeper = class
  private
    FLayouts: TDict<string, TSoLayout>;
    FLayoutFactory: TSoLayoutFactory;
    FEngineSize: TSoEngineSize;
    FLayoutAdded: TNotifyEvent;
    FContainerDelegateCollector: TSoContainerDelegateCollector;
  public
    property LayoutAdded: TNotifyEvent read FLayoutAdded write FLayoutAdded;
    function Add(const AName: string; const APositionAdapter: ISoPositionAdapter): TSoLayout;
    function Get(const AName: string): TSoLayout;
    constructor Create(
      const AEngineSize: TSoEngineSize;
      const AContainerDelegateCollector: TSoContainerDelegateCollector);
    destructor Destroy; override;

end;

implementation

{ TSoLayoutKeeper }

function TSoLayoutKeeper.Add(const AName: string; const APositionAdapter: ISoPositionAdapter): TSoLayout;
begin
  Result := FLayoutFactory.ProduceLayout(APositionAdapter);
  FLayouts.Add(AName, Result);
end;

constructor TSoLayoutKeeper.Create(
      const AEngineSize: TSoEngineSize;
      const AContainerDelegateCollector: TSoContainerDelegateCollector);
begin
  FEngineSize := AEngineSize;
  FContainerDelegateCollector := AContainerDelegateCollector;

  FlayoutFactory := TSoLayoutFactory.Create(AEngineSize, AContainerDelegateCollector);
end;

destructor TSoLayoutKeeper.Destroy;
var
  vLayout: TSoLayout;
begin
  for vLayout in FLayouts.Values do
    vLayout.Free;

  FLayouts.Free;
  FLayoutFactory := nil;
  inherited;
end;

function TSoLayoutKeeper.Get(const AName: string): TSoLayout;
begin
  Result := FLayouts[AName];
end;

end.
