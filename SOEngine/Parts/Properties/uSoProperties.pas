unit uSoProperties;

interface

uses
  System.Generics.Collections,
  uCommonClasses, uSoProperty;

type
  TSoPropertyFriend = class(TSoProperty);

  TSoProperties = class
  private
    FDict: TDictionary<string, TSoProperty>;
    FKeys: TDictionary<TSoProperty, string>;
    FPropertyChanged: TEventList<string>;
    function GetProperty(AName: string): TSoProperty;
    procedure SetProperty(AName: string; const Value: TSoProperty);
    procedure OnPropertyChanged(ASender: TObject); // On one property changed
  public
    property Data[AName: string]: TSoProperty read GetProperty write SetProperty; default;
    procedure AddOnChangeHandler(const AHandler: TEvent<string>);
    procedure RemOnChangeHandler(const AHandler: TEvent<string>);
    constructor Create;
    destructor Destroy; override;
  end;

implementation

{ TSoProperties }

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

procedure TSoProperties.OnPropertyChanged(ASender: TObject);
begin
  FPropertyChanged.RaiseEvent(Self, FKeys[TSoProperty(ASender)]);
end;

procedure TSoProperties.RemOnChangeHandler(const AHandler: TEvent<string>);
begin
  FPropertyChanged.Remove(AHandler);
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

  TSoPropertyFriend(Value).OnChange := OnPropertyChanged;
end;

end.
