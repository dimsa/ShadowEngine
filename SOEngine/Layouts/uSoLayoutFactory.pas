unit uSoLayoutFactory;

interface

uses
  uISoPositionAdapter, uSoLayout, uSoEngineSize;

type
  TSoLayoutFactory = class
  private
    FEngineSize: TSoEngineSize;
  public
    constructor Create(const AEngineSize: TSoEngineSize);
    function ProduceLayout(const APositionAdapter: ISoPositionAdapter): TSoLayout;
  end;

implementation

{ TSoLayoutFactory }

constructor TSoLayoutFactory.Create(const AEngineSize: TSoEngineSize);
begin
  FEngineSize := AEngineSize;
end;

function TSoLayoutFactory.ProduceLayout(const APositionAdapter: ISoPositionAdapter): TSoLayout;
begin
  Result := TSoLayout.Create(APositionAdapter, FEngineSize);
end;

end.
