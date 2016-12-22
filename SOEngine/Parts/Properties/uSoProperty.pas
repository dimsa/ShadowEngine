unit uSoProperty;

interface

uses
  System.Classes, uCommonClasses;

type
  TPropertyType = (ptUndefined, ptObject, ptInt, ptDouble, ptString);

  TSoProperty  = class
  private
    FObject: TObject;
    FInt: Integer;
    FDouble: Double;
    FString: string;
    FOnChangeHandlers: TNotifyEventList;
    FPropertyType: TPropertyType;
    FProp: TObject;
    procedure SetDouble(const Value: Double);
    procedure SetInt(const Value: Integer);
    procedure SetObject(const Value: TObject);
    procedure SetString(const Value: string);
  public
    function Val<T: class>: T;
    property Obj: TObject read FObject write SetObject;
    property PropertyType: TPropertyType read FPropertyType;
    constructor Create; overload;
    procedure RaiseOnChange;
    procedure AddOnChangeHandler(AHandler: TNotifyEvent);
    procedure RemOnChangeHandler(AHandler: TNotifyEvent);
  end;

implementation

{ TSoProperty }



constructor TSoProperty.Create;
begin
  FOnChangeHandlers := TNotifyEventList.Create;
  FPropertyType := ptUndefined;
end;

{constructor TSoProperty.Create(AObject: TObject);
begin
  FOnChangeHandlers := TNotifyEventList.Create;
  FObject := AObject;
  FPropertyType := ptObject;
end;

constructor TSoProperty.Create(AString: string);
begin
  FOnChangeHandlers := TNotifyEventList.Create;
  SetString(AString);
end;

constructor TSoProperty.Create(AInteger: Integer);
begin
  FOnChangeHandlers := TNotifyEventList.Create;
  SetInt(AInteger);
end;

constructor TSoProperty.Create(ADouble: Double);
begin
  FOnChangeHandlers := TNotifyEventList.Create;
  SetDouble(ADouble);
end;
}

procedure TSoProperty.SetDouble(const Value: Double);
begin
  FDouble := Value;
  FPropertyType := ptDouble;

  RaiseOnChange;
end;

procedure TSoProperty.SetInt(const Value: Integer);
begin
  FInt := Value;
  FPropertyType := ptInt;

  RaiseOnChange;
end;

procedure TSoProperty.SetObject(const Value: TObject);
begin
  FObject := Value;
  FPropertyType := ptObject;

  RaiseOnChange;
end;

procedure TSoProperty.SetString(const Value: string);
begin
  FString := Value;
  FPropertyType := ptString;

  RaiseOnChange;
end;

function TSoProperty.Val<T>: T;
begin
  Result := T(FObject);
end;

procedure TSoProperty.AddOnChangeHandler(AHandler: TNotifyEvent);
begin
  FOnChangeHandlers.Add(AHandler);
end;

procedure TSoProperty.RaiseOnChange;
begin
  FOnChangeHandlers.RaiseEvent(Self);
end;

procedure TSoProperty.RemOnChangeHandler(AHandler: TNotifyEvent);
begin
  FOnChangeHandlers.Remove(AHandler);
end;

end.
