unit uSoObjectKeeper;

interface

uses
  uSoBaseOperator, System.Generics.Collections, uISoObject;

type

  TSoObjectKeeper = class(TSoOperator<ISoObject>)
  private
    function GetItem(AName: string): ISoObject;
  public
    property Items[AName: string]: ISoObject read GetItem; default;
    function AddNewObject(const AName: string = ''): ISoObject;
  end;

implementation

uses
  uSoObject;

{ TSoObjectKeeper }

function TSoObjectKeeper.AddNewObject(const AName: string): ISoObject;
begin
  Result := TSoObject.Create;
  Add(Result, AName);
end;

function TSoObjectKeeper.GetItem(AName: string): ISoObject;
begin
  Result := FList[AName];
end;

end.
