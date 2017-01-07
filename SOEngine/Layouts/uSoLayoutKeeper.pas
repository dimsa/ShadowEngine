unit uSoLayoutKeeper;

interface

uses

  uSoTypes, uSoLayout, uSoLayoutFActory, uISoPositionAdapter, uSoEngineSize;

type
  TSoLayoutKeeper = class
  private
    FLayouts: TDict<string, TSoLayout>;
    FLayoutFactory: TSoLayoutFactory;
    FEngineSize: TSoEngineSize;
    FLayoutAdded: TNotifyEvent;
  public
    property LayoutAdded: TNotifyEvent read FLayoutAdded write FLayoutAdded;
    function Add(const AName: string; const APositionAdapter: ISoPositionAdapter): TSoLayout;
    function Get(const AName: string): TSoLayout;
    constructor Create(const AEngineSize: TSoEngineSize);
    destructor Destroy; override;

end;

implementation

{ TSoLayoutKeeper }

function TSoLayoutKeeper.Add(const AName: string; const APositionAdapter: ISoPositionAdapter): TSoLayout;
begin
  Result := FLayoutFactory.ProduceLayout(APositionAdapter);
  FLayouts.Add(AName, Result);
end;

constructor TSoLayoutKeeper.Create(const AEngineSize: TSoEngineSize);
begin
  FEngineSize := AEngineSize;
  FlayoutFactory := TSoLayoutFactory.Create(AEngineSize);
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
