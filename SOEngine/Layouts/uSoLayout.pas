unit uSoLayout;

interface

uses
  uSoTypes, uISoPositionAdapter, uSoContainer, uSoObject;

type
  TSoLayout = class
  private
    FEngineWidth, FEngineHeight: PSingle;
    FPositionAdapter: ISoPositionAdapter;
    FContainerAdded: TNotifyEvent;
    function GetWidth: Single;
    function GetHeight: Single;
  protected
    property ContainerAdded: TNotifyEvent read FContainerAdded write FContainerAdded;
  public
    property Width: Single read GetWidth;
    property Height: Single read GetHeight;
    function AddContainer: TSoContainer;

    constructor Create(const APositionAdapter: ISoPositionAdapter; const AEngineWidth, AEngineHeight: PSingle);
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
