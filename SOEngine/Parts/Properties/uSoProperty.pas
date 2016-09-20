unit uSoProperty;

interface

uses
  System.Classes;

type
  TPropertyType = (ptUndefined, ptObject, ptInt, ptDouble, ptString);

  TSoProperty = class(TObject)
  private
    FObject: TObject;
    FInt: Integer;
    FDouble: Double;
    FString: string;
    FOnChange: TNotifyEvent;
    FPropertyType: TPropertyType;
    procedure SetDouble(const Value: Double);
    procedure SetInt(const Value: Integer);
    procedure SetObject(const Value: TObject);
    procedure SetString(const Value: string);
  protected
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  public
    property Value: TObject read FObject write SetObject;
    property AsInt: Integer read FInt write SetInt;
    property AsDouble: Double read FDouble write SetDouble;
    property AsString: string read FString write SetString;
    property PropertyType: TPropertyType read FPropertyType;
    constructor Create; overload;
    constructor Create(AObject: TObject); overload;
    constructor Create(AString: string); overload;
    constructor Create(AInteger: Integer); overload;
    constructor Create(ADouble: Double); overload;
  end;

implementation

{ TSoProperty }

constructor TSoProperty.Create(AObject: TObject);
begin
  FObject := AObject;
  FPropertyType := ptObject;
end;

constructor TSoProperty.Create;
begin
  FPropertyType := ptUndefined;
end;

constructor TSoProperty.Create(AString: string);
begin
  SetString(AString);
end;

constructor TSoProperty.Create(AInteger: Integer);
begin
  SetInt(AInteger);
end;

{function TSoProperty.GetDouble: Double;
begin
  Result := PDouble(FObject)^;
end;

function TSoProperty.GetInt: Integer;
begin
  Result := PInteger(FObject)^;
end;

function TSoProperty.GetString: string;
begin
  Result := PChar(FObject)^;
end;   }

procedure TSoProperty.SetDouble(const Value: Double);
begin
  FDouble := Value;
  FPropertyType := ptDouble;

  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TSoProperty.SetInt(const Value: Integer);
begin
  FInt := Value;
  FPropertyType := ptInt;

  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TSoProperty.SetObject(const Value: TObject);
begin
  FObject := Value;
  FPropertyType := ptObject;

  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TSoProperty.SetString(const Value: string);
begin
  FString := Value;
  FPropertyType := ptString;

  if Assigned(FOnChange) then
    FOnChange(Self);
end;

constructor TSoProperty.Create(ADouble: Double);
begin
  SetDouble(ADouble);
end;

end.
