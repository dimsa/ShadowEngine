unit uSoProperty;

interface

uses
  System.Classes;

type
  TSoProperty = class(TObject)
  private
    FObject: TObject;
    FOnChange: TNotifyEvent;
    function GetDouble: Double;
    function GetInt: Integer;
    function GetString: string;
    procedure SetDouble(const Value: Double);
    procedure SetInt(const Value: Integer);
    procedure SetObject(const Value: TObject);
    procedure SetString(const Value: string);
  protected
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  public
    property Value: TObject read FObject write SetObject;
    property AsInt: Integer read GetInt write SetInt;
    property AsDouble: Double read GetDouble write SetDouble;
    property AsString: string read GetString write SetString;
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
end;

constructor TSoProperty.Create;
begin

end;

constructor TSoProperty.Create(AString: string);
begin
  SetString(AString);
end;

constructor TSoProperty.Create(AInteger: Integer);
begin
  SetInt(AInteger);
end;

function TSoProperty.GetDouble: Double;
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
end;

procedure TSoProperty.SetDouble(const Value: Double);
begin
  FObject := @Value;

  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TSoProperty.SetInt(const Value: Integer);
begin
  FObject := @Value;

  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TSoProperty.SetObject(const Value: TObject);
begin
  FObject := Value;

  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TSoProperty.SetString(const Value: string);
begin
  FObject := @Value;

  if Assigned(FOnChange) then
    FOnChange(Self);
end;

constructor TSoProperty.Create(ADouble: Double);
begin
  SetDouble(ADouble);
end;

end.
