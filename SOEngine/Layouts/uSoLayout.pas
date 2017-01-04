unit uSoLayout;

interface

uses
  uISoPositionAdapter, uSoContainer, uSoObject;

type
  TSoLayout = class abstract
  private
    FEngineWidth, FEngineHeight: PInteger;
    FPositionAdapter: ISoPositionAdapter;
    function GetWidth: Single;
    function GetHeight: Single;
  public
    property Width: Single read GetWidth;
    property Height: Single read GetHeight;
    function AddContainer: TSoContainer;

    constructor Create(const APositionAdapter: ISoPositionAdapter; const AEngineWidth, AEngineHeight: PInteger);
    destructor Destroy; override;
  end;

implementation

{ TSoLayout }

function TSoLayout.AddContainer: TSoContainer;
var
  vObj: TSoObject;
begin
  vObj := TSoObject.Create(FPositionAdapter);
  Result := TSoContainer.Create(vObj);
end;

constructor TSoLayout.Create(const APositionAdapter: ISoPositionAdapter; const AEngineWidth, AEngineHeight: PInteger);
begin
  FPositionAdapter := APositionAdapter;
  FEngineWidth := AEngineWidth;
  FEngineHeight := AEngineHeight;
end;

destructor TSoLayout.Destroy;
begin
  FPositionAdapter := nil;
  FEngineWidth := nil;
  FEngineHeight := nil;
  inherited;
end;

function TSoLayout.GetHeight: Single;
begin
  Result := FPositionAdapter.AdaptWidth(FEngineWidth^);
end;

function TSoLayout.GetWidth: Single;
begin
  Result := FPositionAdapter.AdaptHeight(FEngineHeight^);
end;

end.
