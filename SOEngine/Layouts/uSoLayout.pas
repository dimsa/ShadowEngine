unit uSoLayout;

interface

uses
  uSoTypes, uISoPositionAdapter, uSoContainer, uSoObject, uSoEngineSize,
  uSoContainerTypes, uSoContainerDelegateCollector;

type
  TSoLayout = class
  private
    FEngineSize: TSoEngineSize;
    FPositionAdapter: ISoPositionAdapter;
    FContainerAdded: TNotifyEvent;
    FContainerDelegateCollector: TSoContainerDelegateCollector;
    function GetWidth: Single;
    function GetHeight: Single;
    function ContainerOnGetDelegate(AObject: TSoObject; AClass: TClass; AName: string): TObject;
    function ContainerOnAddDelegate(AObject: TSoObject; AClass: TClass; AContainerElement: TContainerElement): TObject;
    function ContainerOnAddByTemplateDelegate(AObject: TSoObject; AClass: TClass; AContainerElement: TContainerElementByTemplate): TObject;
  public
    property Width: Single read GetWidth;
    property Height: Single read GetHeight;

    property ContainerAdded: TNotifyEvent read FContainerAdded write FContainerAdded;
    function AddContainer(const AName: string = ''): TSoContainer;

    constructor Create(
      const APositionAdapter: ISoPositionAdapter;
      const AEngineSize: TSoEngineSize;
      const AContainerDelegateCollector: TSoContainerDelegateCollector);
    destructor Destroy; override;
  end;

implementation

{ TSoLayout }

function TSoLayout.AddContainer(const AName: string = ''): TSoContainer;
var
  vObj: TSoObject;
begin
  vObj := TSoObject.Create(FPositionAdapter);
  Result := TSoContainer.Create(vObj, ContainerOnGetDelegate, nil, nil, FContainerDelegateCollector);
end;

function TSoLayout.ContainerOnAddByTemplateDelegate(AObject: TSoObject;
  AClass: TClass; AContainerElement: TContainerElementByTemplate): TObject;
begin

end;

function TSoLayout.ContainerOnAddDelegate(AObject: TSoObject; AClass: TClass;
  AContainerElement: TContainerElement): TObject;
begin

end;

function TSoLayout.ContainerOnGetDelegate(AObject: TSoObject; AClass: TClass; AName: string): TObject;
begin

end;

constructor TSoLayout.Create(
      const APositionAdapter: ISoPositionAdapter;
      const AEngineSize: TSoEngineSize;
      const AContainerDelegateCollector: TSoContainerDelegateCollector);
begin
  FPositionAdapter := APositionAdapter;
  FEngineSize := AEngineSize;
  FContainerDelegateCollector := AContainerDelegateCollector;
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
