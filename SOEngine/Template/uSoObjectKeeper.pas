unit uSoObjectKeeper;

interface

uses
  uSoObject, uSoBaseOperator, System.Generics.Collections;

type

  TSoObjectKeeper = class(TSoOperator<TSoObject>)
  private

  public
    function AddNewObject(const AName: string = ''): TSoObject;
  end;

implementation

{ TSoObjectKeeper }

function TSoObjectKeeper.AddNewObject(const AName: string): TSoObject;
begin
  Result := TSoObject.Create;
  Add(Result, AName);
end;

end.
