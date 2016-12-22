unit uSoObject;

interface

uses
  uGeometryClasses, System.Types, System.Classes, uCommonClasses, uSoProperties, uSoProperty,
  uSoObjectDefaultProperties, uSoTypes, uSoPosition, uSoPositionAdapter;

type
  TSoObject = class
  private
    FContainer: TObject;
    function GetProperty(APropertyName: string): TSoProperty;
    function GetHeight: Single;
    function GetWidth: Single;
  protected
    FPosition: TSoPosition;
    FProperties: TSoProperties;
    FOnDestroyHandlers: TNotifyEventList;
    FOnChangePositionHandlers: TEventList<TPosition>;
    procedure SetContainer(const AContainer: TObject);
  public
    property Position: TSoPosition read FPosition;
    property Width: Single read GetWidth;
    property Height: Single read GetHeight;

    property Container: TObject read FContainer;
    property Properties[APropertyName: string]: TSoProperty read GetProperty; default;// write SetProperty; default;
    function HasProperty(const APropertyName: string): Boolean;
    function AddProperty(const AName: string): TSoProperty;
    procedure AddDestroyHandler(const AHandler: TNotifyEvent);
    procedure RemoveDestroyHandler(const AHandler: TNotifyEvent);

    constructor Create;
    destructor Destroy; override;
  end;

implementation

{ TBaseUnitContainer }

procedure TSoObject.AddDestroyHandler(const AHandler: TNotifyEvent);
begin
  FOnDestroyHandlers.Add(AHandler);
end;

function TSoObject.AddProperty(const AName: string): TSoProperty;
begin
  Result := FProperties.Add(AName);
end;

constructor TSoObject.Create;
begin
  FOnDestroyHandlers := TNotifyEventList.Create;
  FOnChangePositionHandlers := TEventList<TPosition>.Create;
  FProperties := TSoProperties.Create;

  FPosition := TSoPosition.Create(TAbsolutePositionAdapter.Create);
end;

destructor TSoObject.Destroy;
begin
  FOnDestroyHandlers.RaiseEvent(Self);
  FOnDestroyHandlers.Free;
  FOnChangePositionHandlers.Free;
  FProperties.Free;
  FPosition.Free;

  inherited;
end;

function TSoObject.GetHeight: Single;
begin
  Result := FProperties[RenditionRect].Val<TRectObject>.Height;
end;

function TSoObject.GetProperty(APropertyName: string): TSoProperty;
begin
  Result := FProperties[APropertyName];
end;

function TSoObject.GetWidth: Single;
begin
  Result := FProperties[RenditionRect].Val<TRectObject>.Width;
end;

function TSoObject.HasProperty(const APropertyName: string): Boolean;
begin
  Result := FProperties.HasProperty(APropertyName);
end;

procedure TSoObject.RemoveDestroyHandler(const AHandler: TNotifyEvent);
begin
  FOnDestroyHandlers.Remove(AHandler);
end;

procedure TSoObject.SetContainer(const AContainer: TObject);
begin
  FContainer := AContainer;
end;

end.
