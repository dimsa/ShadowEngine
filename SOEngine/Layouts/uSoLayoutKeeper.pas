unit uSoLayoutKeeper;

interface

uses

  uSoTypes, uSoLayout, uSoLayoutFActory, uISoPositionAdapter;

type
  TSoLayoutKeeper = class
  private
    FLayouts: TDict<string, TSoLayout>;
    FLayoutFactory: TSoLayoutFactory;
    AWidth, AHeight: PSingle;
  public
    function Add(const AName: string; const APositionAdapter: ISoPositionAdapter): TSoLayout;
    function Get(const AName: string): TSoLayout;
    constructor Create(const AWidth, AHeight: PSingle);
    destructor Destroy; override;

end;

implementation

{ TSoLayoutKeeper }

function TSoLayoutKeeper.Add(const AName: string;
  const APositionAdapter: ISoPositionAdapter): TSoLayout;
begin
  Result := FLayoutFactory.ProduceLayout(APositionAdapter);
  FLayouts.Add(AName, Result);
end;

constructor TSoLayoutKeeper.Create(const AWidth, AHeight: PSingle);
begin
  FLAyoutFactory := TSoLayoutFactory.Create(AWidth, AHeight);
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
