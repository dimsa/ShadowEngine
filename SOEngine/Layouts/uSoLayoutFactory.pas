unit uSoLayoutFactory;

interface

uses
  uISoPositionAdapter, uSoLayout;

type
  TSoLayoutFactory = class
  private
    FEngineWidth, FEngineHeight: PSingle;
  public
    constructor Create(const AWidth, AHeight: PSingle);
    function ProduceLayout(const APositionAdapter: ISoPositionAdapter): TSoLayout;
  end;

implementation

{ TSoLayoutFactory }

constructor TSoLayoutFactory.Create(const AWidth, AHeight: PSingle);
begin
  FEngineWidth := AWidth;
  FEngineHeight := AHeight;
end;


function TSoLayoutFactory.ProduceLayout(const APositionAdapter: ISoPositionAdapter): TSoLayout;
begin
  Result := TSoLayout.Create(APositionAdapter, FEngineWidth, FEngineHeight);
end;

end.
