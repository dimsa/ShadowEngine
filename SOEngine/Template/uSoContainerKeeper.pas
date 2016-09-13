unit uSoContainerKeeper;

interface

uses
  uSoContainer, uSoBaseOperator, System.Generics.Collections;

type

  TSoContainerKeeper = class(TSoOperator<TSoObject>)
  private

  public
    function AddNewContainer(const AName: string = ''): TSoObject;
  end;

implementation

{ TSoContainerKeeper }

function TSoContainerKeeper.AddNewContainer(const AName: string): TSoObject;
begin
  Result := TSoObject.Create;
  Add(Result, AName);
end;

end.
