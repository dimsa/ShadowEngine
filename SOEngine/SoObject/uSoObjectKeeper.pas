unit uSoObjectKeeper;

interface

uses
  uSoObject, uSoBaseOperator, System.Generics.Collections;

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
  Result := TSoObject.Create;
  Add(Result, AName);
end;

function TSoObjectKeeper.GetItem(AName: string): TSoObject;
begin
  Result := FList[AName];
end;

end.
