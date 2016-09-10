unit uSoContainerKeeper;

interface

uses
  uSoContainer, uSoBaseOperator, System.Generics.Collections;

type

  TSoContainerKeeper = class(TSoOperator<TSoContainer>)
  private

  public
    function AddNewContainer(const AName: string = ''): TSoContainer;
  end;

implementation

{ TSoContainerKeeper }

function TSoContainerKeeper.AddNewContainer(const AName: string): TSoContainer;
begin
  Result := TSoContainer.Create;
  Add(Result, AName);
end;

end.
