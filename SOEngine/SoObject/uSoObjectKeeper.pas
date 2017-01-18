unit uSoObjectKeeper;

interface

uses
  uSoObject, uSoBaseOperator,
  uSoPositionAdapterAbsolute;

type

  TSoObjectKeeper = class(TSoOperator<TSoObject>)
  public
    procedure Add(const AItem: TSoObject); override;
  end;

implementation

{ TSoObjectKeeper }


{ TSoObjectKeeper }

procedure TSoObjectKeeper.Add(const AItem: TSoObject);
begin
  inherited;

end;

end.
