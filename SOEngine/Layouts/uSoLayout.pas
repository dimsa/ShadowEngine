unit uSoLayout;

interface

uses
  uSoTypes, uISoPositionAdapter, uSoContainer, uSoObject, uSoEngineSize;

type
  TSoLayout = class
  private
    FEngineSize: TSoEngineSize;
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

    constructor Create(const APositionAdapter: ISoPositionAdapter; const AEngineSize: TSoEngineSize);
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

constructor TSoLayout.Create(const APositionAdapter: ISoPositionAdapter; const AEngineSize: TSoEngineSize);
begin
  FPositionAdapter := APositionAdapter;
  FEngineSize := AEngineSize;
end;

destructor TSoLayout.Destroy;
begin
  FPositionAdapter := nil;
  FEngineSize := nil;
  inherited;
end;

function TSoLayout.GetHeight: Single;
begin
  Result := FPositionAdapter.AdaptWidth(FEngineSize.Width);
end;

function TSoLayout.GetWidth: Single;
begin
  Result := FPositionAdapter.AdaptHeight(FEngineSize.Height);
end;

end.
