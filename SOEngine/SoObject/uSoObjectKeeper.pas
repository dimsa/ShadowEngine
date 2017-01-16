unit uSoObjectKeeper;

interface

uses
  uSoObject, uSoBaseOperator,
  uSoPositionAdapterAbsolute;

type

  TSoObjectKeeper = class(TSoOperator<TSoObject>)
  private
    function GetItem(AName: string): TSoObject;
  public
    property Items[AName: string]: TSoObject read GetItem; default;
    function AddNewObject(const AName: string = ''): TSoObject;
  end;

implementation

{ TSoObjectKeeper }

function TSoObjectKeeper.AddNewObject(const AName: string): TSoObject;
begin
  Result := TSoObject.Create(TSoPositionAdapterAbsolute.Create);
  Add(Result, AName);
end;

function TSoObjectKeeper.GetItem(AName: string): TSoObject;
begin
  if not FList.TryGetValueByKey(AName, TObject(Result)) then
    Result := nil;
end;

end.
