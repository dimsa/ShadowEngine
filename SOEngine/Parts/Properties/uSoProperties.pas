unit uSoProperties;

interface

uses
  System.Generics.Collections,
  uCommonClasses, uSoProperty;

type
  TSoProperties = class
  private type
    TSoPropertyFriend = class(TSoProperty);
  private
    FDict: TDictionary<string, TSoProperty>;
    FKeys: TDictionary<TSoProperty, string>;
    FPropertyChanged: TEventList<string>;
    function GetProperty(AName: string): TSoProperty;
    procedure SetProperty(AName: string; const Value: TSoProperty);
    procedure OnPropertyChanged(ASender: TObject);// On one property changed
  public
    property Data[AName: string]: TSoProperty read GetProperty;{ write SetProperty;} default;
    function HasProperty(const AName: string): Boolean;
    function Add(const AName: string): TSoProperty; overload;
    function Add(const AName: string; const AProperty: TSoProperty): TSoProperty; overload;
    function Remove(const AName: string): TSoProperty;
    procedure AddOnChangeHandler(const AHandler: TEvent<string>);
    procedure RemOnChangeHandler(const AHandler: TEvent<string>);
    constructor Create;
    destructor Destroy; override;
  end;

implementation

{ TSoProperties }

function TSoProperties.Add(const AName: string): TSoProperty;
begin
  Result := TSoProperty.Create;
  FDict.Add(AName, Result);
end;

function TSoProperties.Add(const AName: string; const AProperty: TSoProperty): TSoProperty;
begin
  FDict.Add(AName, AProperty);
end;

procedure TSoProperties.AddOnChangeHandler(const AHandler: TEvent<string>);
begin
  FPropertyChanged.Add(AHandler);
end;

constructor TSoProperties.Create;
begin
//  inherited;
  FPropertyChanged := TEventList<string>.Create;
  FDict := TDictionary<string, TSoProperty>.Create;
  FKeys := TDictionary<TSoProperty, string>.Create;
end;

destructor TSoProperties.Destroy;
var
  vI: string;
begin
  for vI in FDict.Keys do
    FDict[vI].Free;

  FDict.Free;
  FKeys.Free;
  FPropertyChanged.Free;
  inherited;
end;

function TSoProperties.GetProperty(AName: string): TSoProperty;
begin
  Result := FDict[AName];
end;

function TSoProperties.HasProperty(const AName: string): Boolean;
begin
  Result := FDict.ContainsKey(AName);
end;

procedure TSoProperties.OnPropertyChanged(ASender: TObject);
begin
  FPropertyChanged.RaiseEvent(Self, FKeys[TSoProperty(ASender)]);
end;

procedure TSoProperties.RemOnChangeHandler(const AHandler: TEvent<string>);
begin
  FPropertyChanged.Remove(AHandler);
end;

function TSoProperties.Remove(const AName: string): TSoProperty;
begin
  Result := FDict[AName];
  FDict.Remove(AName);
end;

procedure TSoProperties.SetProperty(AName: string; const Value: TSoProperty);
begin
  if not FDict.ContainsKey(AName) then
  begin
    FDict.Add(AName, Value);
    FKeys.Add(Value, AName);
  end
  else
  begin
    FDict[AName] := Value;
  end;

  TSoPropertyFriend(Value).AddOnChangeHandler(OnPropertyChanged);
end;

end.
