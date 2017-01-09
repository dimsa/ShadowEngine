unit uUnitManagerNew;

interface

uses
  uCommonClasses, uSoTypes,
  uSoObject, uSoContainer, uSoContainerKeeper, uSoLayout, uISoPositionAdapter,
  uSoLayoutKeeper;

type
  TUnitManager = class
  private
    FLastLayout: TSoLayout;
    FContainerKeeper: TSoContainerKeeper;
    FLayoutKeeper: TSoLayoutKeeper;
  public
    function ByObject(const AObject: TSoObject): TSoContainer; overload;
    function ByName(const AContainerName: string): TSoContainer; overload;

    function AddLayout(const AName: string; const APositionAdapter: ISoPositionAdapter): TSoLayout;
    function GetLayout(const AName: string): TSoLayout;

    function AddContainer(const AName: string = ''): TSoContainer;

    constructor Create(const AContainerKeeper: TSoContainerKeeper; const ALayoutKeeper: TSoLayoutKeeper);
    destructor Destroy; override;
  end;

implementation

{ TSoManager }

function TUnitManager.AddLayout(const AName: string; const APositionAdapter: ISoPositionAdapter): TSoLayout;
begin
  FLastLayout := FLayoutKeeper.Add(AName, APositionAdapter);
  Result := FLastLayout;
end;

function TUnitManager.AddContainer(const AName: string): TSoContainer;
begin
  result := FLastLayout.AddContainer(AName);
end;

function TUnitManager.ByName(const AContainerName: string): TSoContainer;
begin

end;

function TUnitManager.ByObject(const AObject: TSoObject): TSoContainer;
begin

end;

constructor TUnitManager.Create(const AContainerKeeper: TSoContainerKeeper; const ALayoutKeeper: TSoLayoutKeeper);
begin
  FContainerKeeper := AContainerKeeper;
  FLayoutKeeper := ALayoutKeeper;
end;

destructor TUnitManager.Destroy;
begin
  FContainerKeeper := nil;
  FLayoutKeeper := nil;
  inherited;
end;

function TUnitManager.GetLayout(const AName: string): TSoLayout;
begin
  FLastLayout := FLayoutKeeper.Get(AName);
  Result := FLastLayout;
end;

end.

